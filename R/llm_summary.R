# R/llm_summary.R
# Gemini API integration for plain-language mortality trend interpretation

library(httr2)
library(glue)

# Extract text from Gemini response variants.
extract_gemini_text <- function(resp) {
  if (is.null(resp)) return(NULL)

  if (is.character(resp)) {
    text_out <- paste(resp, collapse = " ")
    return(trimws(text_out))
  }

  if (!is.list(resp)) return(NULL)

  pull_parts_text <- function(parts) {
    if (is.null(parts)) return(NULL)
    text_vec <- vapply(parts, function(p) {
      if (!is.null(p$text)) as.character(p$text) else ""
    }, character(1))
    text_out <- paste0(text_vec, collapse = "")
    if (nchar(trimws(text_out)) == 0) return(NULL)
    trimws(text_out)
  }

  # Standard Gemini response: resp$candidates[[1]]$content$parts
  if (!is.null(resp$candidates) && length(resp$candidates) > 0) {
    parts <- resp$candidates[[1]]$content$parts %||% NULL
    text_out <- pull_parts_text(parts)
    if (!is.null(text_out)) return(text_out)
  }

  # If a candidates list was returned directly.
  if (!is.null(resp[[1]]$content$parts)) {
    text_out <- pull_parts_text(resp[[1]]$content$parts)
    if (!is.null(text_out)) return(text_out)
  }

  # If content/parts is at the top level.
  if (!is.null(resp$content$parts)) {
    text_out <- pull_parts_text(resp$content$parts)
    if (!is.null(text_out)) return(text_out)
  }

  NULL
}

# Coerce any LLM output into a single clean text string.
coerce_llm_output <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.character(x)) {
    text_out <- paste(x, collapse = " ")
    return(trimws(text_out))
  }

  if (!is.list(x)) return(NULL)

  text_hits <- character(0)
  collect_text <- function(node) {
    if (is.null(node)) return()
    if (is.character(node)) {
      if (length(node) > 0) {
        text_hits <<- c(text_hits, node)
      }
      return()
    }
    if (is.list(node)) {
      if (!is.null(node$text) && is.character(node$text)) {
        text_hits <<- c(text_hits, node$text)
      }
      for (child in node) collect_text(child)
    }
  }
  collect_text(x)

  if (length(text_hits) == 0) return(NULL)
  text_out <- paste(text_hits, collapse = " ")
  trimws(text_out)
}

# Build prompt
build_prompt <- function(data_summary, cause, state) {

  system_prompt <- glue(
    "You are a public health epidemiologist helping students and health professionals ",
    "understand U.S. mortality trends. You write clearly, use precise language, and ",
    "always contextualize data within known public health events or policy changes. ",
    "You clearly distinguish between correlation and causation. You note when trends ",
    "may reflect real changes in disease burden versus changes in coding, reporting, ",
    "or demographics. Keep responses professional, evidence-grounded, and accessible."
  )

  user_prompt <- glue(
    "Below is a summary of CDC mortality data for '{cause}' in {state}.\n\n",
    "{data_summary}\n\n",
    "Please write a 4-6 sentence plain-language interpretation for a public health ",
    "student or professional. Your response should:\n",
    "1. Describe the overall trend and whether mortality is increasing, decreasing, or stable.\n",
    "2. Put the numbers in context - is this rate high or low relative to historical norms?\n",
    "3. Suggest 1-2 plausible public health explanations for the trend (e.g., aging population, ",
    "   prevention programs, opioid epidemic, smoking decline).\n",
    "4. Note any important caveats about interpreting this data ",
    "   (e.g., ICD-10 coding changes, COVID-19 disruption to 2020 data, population shifts).\n",
    "Keep the tone informative but accessible - not overly clinical."
  )

  list(system = system_prompt, user = user_prompt)
}

# Call Gemini API (REST)
get_llm_summary <- function(data_summary,
                            cause,
                            state,
                            api_key = Sys.getenv("GEMINI_API_KEY"),
                            model   = Sys.getenv("GEMINI_MODEL", "gemini-1.5-flash"),
                            api_version = Sys.getenv("GEMINI_API_VERSION", "v1")) {

  if (is.null(api_key) || nchar(api_key) == 0) {
    api_key <- Sys.getenv("GOOGLE_API_KEY")
  }

  if (is.null(api_key) || nchar(api_key) == 0) {
    return("No Gemini API key found. Set GEMINI_API_KEY in your .env file.")
  }

  prompts <- build_prompt(data_summary, cause, state)

  prompt_text <- enc2utf8(glue("{prompts$system}\n\n{prompts$user}"))
  prompt_text <- as.character(prompt_text)
  if (length(prompt_text) != 1) {
    prompt_text <- paste(prompt_text, collapse = " ")
  }

  base_url <- glue("https://generativelanguage.googleapis.com/{api_version}")

  normalize_model_name <- function(name) {
    if (is.null(name)) return(NA_character_)
    name <- as.character(name)
    sub("^models/", "", name)
  }

  list_models <- function() {
    req <- request(glue("{base_url}/models")) |>
      req_headers(
        "x-goog-api-key" = api_key,
        "content-type"  = "application/json"
      ) |>
      req_error(is_error = function(resp) FALSE)

    raw_resp <- req |> req_perform()
    if (httr2::resp_is_error(raw_resp)) return(NULL)

    resp <- tryCatch(resp_body_json(raw_resp), error = function(e) NULL)
    if (is.null(resp) || is.null(resp$models)) return(NULL)

    models <- resp$models
    out <- purrr::map_chr(models, ~ .x$name %||% NA_character_)
    out <- out[!is.na(out)]
    unique(normalize_model_name(out))
  }

  do_request <- function(model_name) {
    body <- list(
      contents = list(
        list(
          role  = "user",
          parts = list(
            list(text = prompt_text)
          )
        )
      )
    )

    body_json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")

    model_name <- normalize_model_name(model_name)
    req <- request(glue("{base_url}/models/{model_name}:generateContent")) |>
      req_headers(
        "x-goog-api-key" = api_key,
        "content-type"  = "application/json"
      ) |>
      req_error(is_error = function(resp) FALSE) |>
      req_body_raw(body_json, type = "application/json")

    raw_resp <- req |> req_perform()

    if (httr2::resp_is_error(raw_resp)) {
      err_body <- tryCatch(resp_body_json(raw_resp), error = function(e) NULL)
      return(list(
        .error = TRUE,
        status = httr2::resp_status(raw_resp),
        message = err_body$error$message %||% "Unknown error from AI service.",
        model = model_name
      ))
    }

    resp_body_json(raw_resp)
  }

  resp <- tryCatch({
    first <- do_request(model)

    if (is.list(first) && isTRUE(first$.error) && first$status == 404) {
      # Try to discover available models and pick a compatible one.
      available <- list_models()
      if (!is.null(available) && length(available) > 0) {
        # Prefer common text models if available.
        preferred <- c("gemini-1.5-flash", "gemini-1.5-pro", "gemini-1.0-pro")
        pick <- preferred[preferred %in% available][1]
        if (is.na(pick)) pick <- available[1]
        return(do_request(pick))
      }
    }

    first
  }, error = function(e) {
    message("LLM API error: ", e$message)
    return(NULL)
  })

  if (is.null(resp)) {
    return("Could not reach AI service. Check your API key and internet connection.")
  }

  if (is.list(resp) && isTRUE(resp$.error)) {
    return(glue("AI service error (HTTP {resp$status}) for model '{resp$model}': {resp$message}"))
  }

  text_out <- tryCatch(extract_gemini_text(resp), error = function(e) NULL)
  if (is.null(text_out) || length(text_out) == 0 || is.na(text_out)) {
    text_out <- tryCatch(coerce_llm_output(resp), error = function(e) NULL)
  }

  if (is.null(text_out) || length(text_out) == 0 || is.na(text_out)) {
    return("Unexpected response format from AI service.")
  }

  text_out <- as.character(text_out)
  if (length(text_out) > 1) {
    text_out <- paste(text_out, collapse = " ")
  }
  # Defensive cleanup: strip any trailing model metadata accidentally appended.
  text_out <- gsub("(?is)\\bmodel\\s+stop\\b.*$", "", text_out, perl = TRUE)
  text_out <- gsub("(?is)\\bgemini-[^\\n]*$", "", text_out, perl = TRUE)
  trimws(text_out)
}

# Shiny-safe wrapper
generate_summary_safe <- function(cause, state, year_range, df, stats,
                                  metric = "age_adjusted_rate_100k",
                                  metric_label = "Age-adjusted Death Rate (per 100k)",
                                  api_key) {

  if (nrow(df) == 0) return("No data available to summarize.")

  data_summary <- build_llm_data_summary(
    cause, state, year_range, df, stats,
    metric = metric, metric_label = metric_label
  )
  get_llm_summary(data_summary, cause, state, api_key = api_key)
}
