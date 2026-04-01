# R/clean_data.R
# Wrangling, summarizing, and preparing CDC mortality data for visualization

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)

# ── Leading causes: trend over time ──────────────────────────────────────────
#' Aggregate death counts and rates by year for trend analysis
#'
#' @param df     Tibble from fetch_leading_causes().
#' @param metric Character. "deaths" or "age_adj_rate".
#' @return Tibble: year, cause_name, state, value.
trend_by_year <- function(df, metric = "age_adj_rate") {
  metric_use <- metric
  if (!metric_use %in% names(df)) metric_use <- "deaths"
  if (!is.numeric(df[[metric_use]])) {
    df[[metric_use]] <- readr::parse_number(as.character(df[[metric_use]]))
  }
  if (all(is.na(df[[metric_use]]))) {
    metric_use <- "deaths"
    if (!is.numeric(df[[metric_use]])) {
      df[[metric_use]] <- readr::parse_number(as.character(df[[metric_use]]))
    }
  }

  df |>
    filter(!is.na(.data[[metric_use]])) |>
    group_by(year, cause_name, state) |>
    summarise(value = sum(.data[[metric_use]], na.rm = TRUE), .groups = "drop") |>
    arrange(year)
}

#' Compute % change from baseline year to most recent year
#'
#' @param df     Tibble from trend_by_year().
#' @return Tibble with pct_change column added.
add_pct_change <- function(df) {
  df |>
    group_by(cause_name, state) |>
    arrange(year) |>
    mutate(
      baseline   = first(value),
      pct_change = round((value - baseline) / baseline * 100, 1)
    ) |>
    ungroup()
}

# ── Drug overdose: parse month/period into date ───────────────────────────────
#' Add a proper Date column to the overdose data
#'
#' @param df Tibble from fetch_drug_overdose().
#' @return Tibble with `date` column added.
add_overdose_date <- function(df) {
  month_map <- c(
    "January" = 1, "February" = 2, "March" = 3, "April" = 4,
    "May" = 5, "June" = 6, "July" = 7, "August" = 8,
    "September" = 9, "October" = 10, "November" = 11, "December" = 12
  )

  df |>
    mutate(
      month_num = month_map[month],
      date = if_else(
        !is.na(month_num),
        as.Date(paste(year, month_num, "01", sep = "-")),
        as.Date(paste(year, "07", "01", sep = "-"))   # fallback: mid-year
      )
    ) |>
    select(-month_num)
}

#' Summarize overdose deaths by year (national or state)
overdose_by_year <- function(df) {
  df |>
    add_overdose_date() |>
    mutate(value_use = dplyr::coalesce(data_value, predicted_value)) |>
    filter(!is.na(value_use)) |>
    group_by(year, state_name, indicator) |>
    summarise(total = sum(value_use, na.rm = TRUE), .groups = "drop")
}

#' Monthly trend line for a specific indicator
overdose_monthly_trend <- function(df, indicator_filter = NULL) {
  out <- df |> add_overdose_date()

  if (!is.null(indicator_filter) && indicator_filter != "All") {
    out <- filter(out, str_detect(indicator, fixed(indicator_filter, ignore_case = TRUE)))
  }

  out |>
    mutate(value_use = dplyr::coalesce(data_value, predicted_value)) |>
    filter(!is.na(value_use)) |>
    group_by(date, state_name) |>
    summarise(value = sum(value_use, na.rm = TRUE), .groups = "drop") |>
    arrange(date)
}

# ── Summary statistics for KPI cards ─────────────────────────────────────────
#' Compute headline stats for display cards
#'
#' @param df         Tibble from fetch_leading_causes().
#' @param metric     Character.
#' @return Named list of summary values.
compute_headline_stats <- function(df, metric = "age_adj_rate") {
  if (nrow(df) == 0) return(list(total_deaths = 0, peak_year = NA,
                                  latest_rate = NA, trend_dir = "—"))
  if (!metric %in% names(df)) metric <- "deaths"
  if (!is.numeric(df[[metric]])) {
    df[[metric]] <- readr::parse_number(as.character(df[[metric]]))
  }

  total_deaths <- df |>
    filter(!is.na(deaths)) |>
    summarise(n = sum(deaths, na.rm = TRUE)) |>
    pull(n)

  peak_year <- df |>
    filter(!is.na(deaths)) |>
    group_by(year) |>
    summarise(n = sum(deaths)) |>
    slice_max(n, n = 1) |>
    pull(year)

  latest_rate <- df |>
    filter(!is.na(.data[[metric]])) |>
    slice_max(year, n = 1) |>
    summarise(r = mean(.data[[metric]], na.rm = TRUE)) |>
    pull(r)

  # Trend direction: compare first vs last year average rate
  year_rates <- df |>
    filter(!is.na(.data[[metric]])) |>
    group_by(year) |>
    summarise(r = mean(.data[[metric]], na.rm = TRUE)) |>
    arrange(year)

  trend_dir <- if (nrow(year_rates) >= 2) {
    diff_val <- last(year_rates$r) - first(year_rates$r)
    if (diff_val > 0.5) "↑ Increasing" else if (diff_val < -0.5) "↓ Decreasing" else "→ Stable"
  } else "—"

  list(
    total_deaths = total_deaths,
    peak_year    = peak_year,
    latest_rate  = round(latest_rate, 1),
    trend_dir    = trend_dir
  )
}

# ── LLM prompt data builder ───────────────────────────────────────────────────
#' Format a structured data summary for the LLM
#'
#' @param cause      Character. Selected cause of death.
#' @param state      Character.
#' @param year_range Integer vector of length 2.
#' @param df         Tibble from fetch_leading_causes().
#' @param stats      List from compute_headline_stats().
#' @return Character string.
build_llm_data_summary <- function(cause, state, year_range, df, stats) {

  trend_df <- trend_by_year(df) |>
    add_pct_change() |>
    filter(state == !!state | state == "United States") |>
    arrange(year)

  # Year-over-year rate summary (first, middle, last)
  years_sample <- trend_df |>
    slice(c(1, ceiling(n()/2), n())) |>
    mutate(label = paste0(year, ": ", round(value, 1), " per 100k"))

  year_series <- paste(years_sample$label, collapse = " → ")

  overall_pct_change <- if (nrow(trend_df) >= 2) {
    first_rate <- first(trend_df$value)
    last_rate  <- last(trend_df$value)
    round((last_rate - first_rate) / first_rate * 100, 1)
  } else NA

  glue::glue(
    "Cause of death: {cause}\n",
    "Geography: {state}\n",
    "Years analyzed: {year_range[1]}–{year_range[2]}\n",
    "Total deaths in period: {scales::comma(stats$total_deaths)}\n",
    "Peak mortality year: {stats$peak_year}\n",
    "Latest age-adjusted rate: {stats$latest_rate} per 100,000\n",
    "Overall trend direction: {stats$trend_dir}\n",
    "Rate trend (age-adjusted per 100k): {year_series}\n",
    "Overall % change across period: {overall_pct_change}%"
  )
}

# â”€â”€ Overdose summary helpers â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
#' Compute overdose headline stats
#'
#' @param df Tibble from fetch_drug_overdose().
#' @return Named list of summary values.
compute_overdose_stats <- function(df) {
  if (nrow(df) == 0) return(list(total = 0, peak_date = NA, latest_value = NA, trend_dir = "â€”"))

  trend_df <- overdose_monthly_trend(df)

  if (nrow(trend_df) == 0) return(list(total = 0, peak_date = NA, latest_value = NA, trend_dir = "â€”"))

  total <- trend_df |>
    summarise(n = sum(value, na.rm = TRUE)) |>
    pull(n)

  peak_date <- trend_df |>
    slice_max(value, n = 1) |>
    pull(date)

  latest_value <- trend_df |>
    slice_max(date, n = 1) |>
    pull(value)

  trend_dir <- if (nrow(trend_df) >= 2) {
    first_val <- first(trend_df$value)
    last_val  <- last(trend_df$value)
    diff_val  <- last_val - first_val
    if (diff_val > 1) "â†‘ Increasing" else if (diff_val < -1) "â†“ Decreasing" else "â†’ Stable"
  } else "â€”"

  list(
    total        = total,
    peak_date    = peak_date,
    latest_value = round(latest_value, 1),
    trend_dir    = trend_dir
  )
}

#' Format a structured data summary for overdose LLM
#'
#' @param state      Character.
#' @param indicator  Character.
#' @param year_range Integer vector of length 2.
#' @param df         Tibble from fetch_drug_overdose().
#' @param stats      List from compute_overdose_stats().
#' @return Character string.
build_overdose_llm_data_summary <- function(state, indicator, year_range, df, stats) {
  trend_df <- overdose_monthly_trend(df, indicator_filter = indicator) |>
    filter(state_name == !!state | state_name == "United States") |>
    arrange(date)

  months_sample <- trend_df |>
    slice(c(1, ceiling(n()/2), n())) |>
    mutate(label = paste0(format(date, "%b %Y"), ": ", round(value, 1)))

  series <- paste(months_sample$label, collapse = " â†’ ")

  overall_pct_change <- if (nrow(trend_df) >= 2) {
    first_val <- first(trend_df$value)
    last_val  <- last(trend_df$value)
    round((last_val - first_val) / first_val * 100, 1)
  } else NA

  glue::glue(
    "Indicator: {indicator}\n",
    "Geography: {state}\n",
    "Years analyzed: {year_range[1]}â€“{year_range[2]}\n",
    "Total overdose deaths (period): {scales::comma(stats$total)}\n",
    "Peak month: {format(stats$peak_date, '%b %Y')}\n",
    "Latest monthly value: {stats$latest_value}\n",
    "Overall trend direction: {stats$trend_dir}\n",
    "Monthly trend sample: {series}\n",
    "Overall % change across period: {overall_pct_change}%"
  )
}
