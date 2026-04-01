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
#   "VSRR Provisional Drug Overdose Death Counts" — data.cdc.gov
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

  # VSRR Provisional Drug Overdose Death Counts (monthly, by state)
  drug_overdose  = "https://data.cdc.gov/resource/xkb8-kh2a.json",

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

  # Build SoQL WHERE clause
  where_parts <- c(
    paste0("year >= ", year_min),
    paste0("year <= ", year_max)
  )

  if (!is.null(state) && state != "All" && state != "") {
    where_parts <- c(where_parts, paste0("state = '", state, "'"))
  }

  if (!is.null(cause) && cause != "All" && cause != "") {
    where_parts <- c(where_parts,
                     paste0("cause_name = '", cause, "'"))
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

  parse_leading_causes(resp)
}

#' Parse leading causes JSON to tidy tibble
parse_leading_causes <- function(resp) {
  map_dfr(resp, function(r) {
    tibble(
      year         = as.integer(r$year %||% NA),
      cause_name   = r$cause_name %||% NA_character_,
      cause_icd10  = r$`113_cause_name` %||% NA_character_,
      state        = r$state %||% NA_character_,
      deaths       = as.numeric(r$deaths %||% NA),
      age_adj_rate = as.numeric(r$age_adjusted_death_rate %||% NA)
    )
  }) |>
    filter(!is.na(year), !is.na(deaths))
}

# ── Drug Overdose Deaths ──────────────────────────────────────────────────────
#' Fetch VSRR Provisional Drug Overdose Death Counts
#'
#' @param state      Character or "US" for national.
#' @param indicator  Character. Drug type filter (e.g., "Opioids", "Cocaine").
#' @param year_min   Integer.
#' @param year_max   Integer.
#' @return Tibble of overdose death counts by month/state.
fetch_drug_overdose <- function(state     = NULL,
                                 indicator = NULL,
                                 year_min  = 2015,
                                 year_max  = as.integer(format(Sys.Date(), "%Y")),
                                 limit     = 10000) {

  where_parts <- character(0)

  if (!is.null(state) && state != "All" && state != "") {
    where_parts <- c(where_parts, paste0("state_name = '", state, "'"))
  }

  if (!is.null(indicator) && indicator != "All" && indicator != "") {
    where_parts <- c(where_parts,
                     paste0("indicator = '", indicator, "'"))
  }

  where_clause <- if (length(where_parts) > 0)
    paste(where_parts, collapse = " AND ")
  else NULL

  req <- request(CDC_ENDPOINTS$drug_overdose) |>
    req_url_query(`$limit` = limit, `$order` = "year ASC")

  if (!is.null(where_clause)) {
    req <- req |> req_url_query(`$where` = where_clause)
  }

  resp <- tryCatch(
    req |> req_perform() |> resp_body_json(),
    error = function(e) { message("CDC overdose API error: ", e$message); NULL }
  )

  if (is.null(resp) || length(resp) == 0) return(tibble())

  parse_drug_overdose(resp)
}

#' Parse drug overdose JSON
parse_drug_overdose <- function(resp) {
  map_dfr(resp, function(r) {
    tibble(
      state_name   = r$state_name %||% NA_character_,
      state_abbr   = r$state %||% NA_character_,
      year         = as.integer(r$year %||% NA),
      month        = r$month %||% NA_character_,
      period       = r$period %||% NA_character_,
      indicator    = r$indicator %||% NA_character_,
      data_value   = suppressWarnings(as.numeric(r$data_value %||% NA)),
      predicted_value = suppressWarnings(as.numeric(r$predicted_value %||% NA))
    )
  }) |>
    filter(!is.na(year))
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
    "CLRD",                              # Chronic lower respiratory diseases
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

#' Get list of drug overdose indicator types
get_overdose_indicators <- function() {
  c(
    "Number of Drug Overdose Deaths",
    "Number of Deaths with Underlying Cause of Death Coded as Drug Poisoning (T36-T50.9)",
    "Opioids (T40.0-T40.4,T40.6)",
    "Heroin (T40.1)",
    "Cocaine (T40.5)",
    "Psychostimulants with abuse potential (T43.6)",
    "Methadone (T40.3)",
    "Natural & semi-synthetic opioids (T40.2)",
    "Synthetic opioids, excl. methadone (T40.4)"
  )
}

# ── Helper ────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

