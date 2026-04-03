# R/cdc_api.R
# Functions to fetch mortality data from CDC WONDER and supplementary APIs
#
# CDC WONDER does not have a fully open REST API for all queries, so this
# module uses a combination of:
#   1. CDC WONDER Mortality API (limited public endpoint)
#   2. data.cdc.gov Socrata API (WONDER data published as open datasets)
#   3. CDC FluView / MMWR supplementary data
#
# Primary dataset used:
#   "NCHS - Leading Causes of Death: United States" — data.cdc.gov
#   "Underlying Cause of Death" wonder.cdc.gov (via pre-built query API)

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# ── Dataset endpoints (data.cdc.gov Socrata API) ──────────────────────────────
CDC_ENDPOINTS <- list(

  # NCHS Leading Causes of Death: United States (1999–2020)
  leading_causes = "https://data.cdc.gov/resource/bi63-dtpu.json",

  # NCHS Death Rates and Life Expectancy at Birth
  life_expectancy = "https://data.cdc.gov/resource/w9j2-ggv5.json",

  # Compressed Mortality (ICD-10 causes, by state + year)
  # Accessed via CDC WONDER wonder.cdc.gov public API
  wonder_base    = "https://wonder.cdc.gov/controller/datarequest/D76"
)

# ── Leading Causes of Death ───────────────────────────────────────────────────
#' Fetch NCHS Leading Causes of Death data
#'
#' @param cause     Character or NULL. Filter to a specific cause (partial match).
#' @param state     Character or NULL. Two-letter state abbreviation or "United States".
#' @param year_min  Integer. Earliest year.
#' @param year_max  Integer. Latest year.
#' @param limit     Integer. Max records (Socrata max = 50000).
#' @return Tibble of mortality records.
fetch_leading_causes <- function(cause     = NULL,
                                  state     = NULL,
                                  year_min  = 2005,
                                  year_max  = 2020,
                                  limit     = 5000) {
  normalize_cause <- function(x) {
    if (is.null(x) || x == "" || x == "All causes") return(NULL)
    if (x == "CLRD") return("Chronic lower respiratory diseases")
    x
  }

  escape_soql <- function(x) {
    if (is.null(x)) return(NULL)
    gsub("'", "''", x, fixed = TRUE)
  }

  cause <- normalize_cause(cause)
  cause_escaped <- escape_soql(cause)
  state_escaped <- escape_soql(state)

  # Build SoQL WHERE clause
  where_parts <- c(
    paste0("year >= ", year_min),
    paste0("year <= ", year_max)
  )

  if (!is.null(state) && state != "All" && state != "") {
    where_parts <- c(where_parts, paste0("state = '", state_escaped, "'"))
  }

  if (!is.null(cause) && cause != "All" && cause != "") {
    where_parts <- c(where_parts,
                     paste0("cause_name = '", cause_escaped, "'"))
  }

  where_clause <- paste(where_parts, collapse = " AND ")

  resp <- tryCatch(
    request(CDC_ENDPOINTS$leading_causes) |>
      req_url_query(
        `$where` = where_clause,
        `$limit` = limit,
        `$order` = "year ASC"
      ) |>
      req_perform() |>
      resp_body_json(),
    error = function(e) {
      message("CDC API error: ", e$message)
      return(NULL)
    }
  )

  if (is.null(resp) || length(resp) == 0) return(tibble())

  out <- parse_leading_causes(resp)

  # Fallback: if exact cause match returns nothing, refetch without cause and filter locally
  if (nrow(out) == 0 && !is.null(cause)) {
    where_parts_fallback <- c(
      paste0("year >= ", year_min),
      paste0("year <= ", year_max)
    )

    if (!is.null(state) && state != "All" && state != "") {
      where_parts_fallback <- c(where_parts_fallback, paste0("state = '", state_escaped, "'"))
    }

    where_clause_fallback <- paste(where_parts_fallback, collapse = " AND ")

    resp_fallback <- tryCatch(
      request(CDC_ENDPOINTS$leading_causes) |>
        req_url_query(
          `$where` = where_clause_fallback,
          `$limit` = limit,
          `$order` = "year ASC"
        ) |>
        req_perform() |>
        resp_body_json(),
      error = function(e) {
        message("CDC API error (fallback): ", e$message)
        return(NULL)
      }
    )

    if (!is.null(resp_fallback) && length(resp_fallback) > 0) {
      out <- parse_leading_causes(resp_fallback) |>
        filter(str_detect(cause_name, fixed(cause, ignore_case = TRUE)))
    }
  }

  out
}

#' Parse leading causes JSON to tidy tibble
parse_leading_causes <- function(resp) {
  clean_chr <- function(x) {
    if (is.null(x)) return(NA_character_)
    stringr::str_trim(as.character(x))
  }

  parse_num <- function(x) {
    readr::parse_number(as.character(x))
  }

  first_non_null <- function(...) {
    vals <- list(...)
    for (v in vals) if (!is.null(v)) return(v)
    NULL
  }

  map_dfr(resp, function(r) {
    rate_raw <- first_non_null(
      r$rate,
      r$death_rate,
      r$crude_rate,
      r$crude_death_rate
    )
    tibble(
      year         = as.integer(r$year %||% NA),
      cause_name   = clean_chr(r$cause_name %||% NA_character_),
      cause_icd10  = clean_chr(r$`113_cause_name` %||% NA_character_),
      state        = clean_chr(r$state %||% NA_character_),
      deaths       = parse_num(r$deaths %||% NA),
      rate         = parse_num(rate_raw %||% NA)
    )
  }) |>
    filter(!is.na(year), !is.na(deaths))
}

# ── Available causes list (for dropdown) ─────────────────────────────────────
#' Get a sorted list of unique cause names in the leading causes dataset
#'
#' @return Character vector of cause names.
get_available_causes <- function() {
  # Static list derived from NCHS dataset to avoid a slow API call on startup
  c(
    "All causes",
    "Alzheimer's disease",
    "Cancer",
    "Chronic lower respiratory diseases",
    "Diabetes",
    "Heart disease",
    "Influenza and pneumonia",
    "Kidney disease",
    "Stroke",
    "Suicide",
    "Unintentional injuries"
  )
}

#' Get a sorted list of U.S. states + "United States" for the dropdown
get_available_states <- function() {
  c(
    "United States",
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
    "District of Columbia"
  )
}

# ── Helper ────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

