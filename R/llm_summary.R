# R/llm_summary.R
# Gemini API integration for plain-language mortality trend interpretation

library(httr2)
library(glue)

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
    "   (e.g., ICD-10 coding changes, COVID-19 disruption to 2020 data, age-adjustment).\n",
    "Keep the tone informative but accessible - not overly clinical."
  )

  list(system = system_prompt, user = user_prompt)
}

# Call Gemini API (REST)
get_llm_summary <- function(data_summary,
                            cause,
                            state,
                            api_key = Sys.getenv("GEMINI_API_KEY"),
                            model   = "gemini-2.5-flash") {

  if (is.null(api_key) || nchar(api_key) == 0) {
    api_key <- Sys.getenv("GOOGLE_API_KEY")
  }

  if (is.null(api_key) || nchar(api_key) == 0) {
    return("No Gemini API key found. Set GEMINI_API_KEY in your .env file.")
  }

  prompts <- build_prompt(data_summary, cause, state)

  resp <- tryCatch(
    request(glue("https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent")) |>
      req_headers(
        "x-goog-api-key" = api_key,
        "content-type"  = "application/json"
      ) |>
      req_body_json(list(
        contents = list(
          list(
            role  = "user",
            parts = list(
              list(text = glue("{prompts$system}\n\n{prompts$user}"))
            )
          )
        )
      )) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) {
      message("LLM API error: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp)) {
    return("Could not reach AI service. Check your API key and internet connection.")
  }

  text_out <- tryCatch({
    parts <- resp$candidates[[1]]$content$parts
    if (is.null(parts)) return(NULL)
    text_vec <- vapply(parts, function(p) {
      if (!is.null(p$text)) as.character(p$text) else ""
    }, character(1))
    paste0(text_vec, collapse = "")
  }, error = function(e) NULL)

  if (is.null(text_out) || length(text_out) == 0 || is.na(text_out)) {
    return("Unexpected response format from AI service.")
  }

  as.character(text_out)
}

# Shiny-safe wrapper
generate_summary_safe <- function(cause, state, year_range, df, stats, api_key) {

  if (nrow(df) == 0) return("No data available to summarize.")

  data_summary <- build_llm_data_summary(cause, state, year_range, df, stats)
  get_llm_summary(data_summary, cause, state, api_key = api_key)
}
