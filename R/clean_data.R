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
#' @param metric Character. "deaths", "rate", or "age_adj_rate".
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

  agg_fun <- if (metric_use %in% c("age_adj_rate", "rate")) mean else sum

  df |>
    filter(!is.na(.data[[metric_use]])) |>
    group_by(year, cause_name, state) |>
    summarise(value = agg_fun(.data[[metric_use]], na.rm = TRUE), .groups = "drop") |>
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
      pct_change = dplyr::if_else(
        is.na(baseline) | baseline == 0,
        NA_real_,
        round((value - baseline) / baseline * 100, 1)
      )
    ) |>
    ungroup()
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
    summarise(r = if (metric == "deaths") sum(.data[[metric]], na.rm = TRUE)
              else mean(.data[[metric]], na.rm = TRUE)) |>
    pull(r)

  # Trend direction: compare first vs last year average rate
  year_rates <- df |>
    filter(!is.na(.data[[metric]])) |>
    group_by(year) |>
    summarise(r = if (metric == "deaths") sum(.data[[metric]], na.rm = TRUE)
              else mean(.data[[metric]], na.rm = TRUE)) |>
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
#' @param cdi_df     Tibble from fetch_chronic_indicators() (optional).
#' @param cdi_label  Character label for CDI indicator (optional).
#' @return Character string.
build_llm_data_summary <- function(cause, state, year_range, df, stats,
                                   metric = "age_adj_rate",
                                   metric_label = "Age-Adjusted Rate (per 100k)",
                                   cdi_df = NULL, cdi_label = NULL) {

  trend_df <- trend_by_year(df, metric = metric) |>
    add_pct_change() |>
    filter(state == !!state | state == "United States") |>
    arrange(year)

  # Year-over-year rate summary (first, middle, last)
  years_sample <- trend_df |>
    slice(c(1, ceiling(n()/2), n())) |>
    mutate(label = paste0(year, ": ", round(value, 1)))

  year_series <- paste(years_sample$label, collapse = " → ")

  overall_pct_change <- if (nrow(trend_df) >= 2) {
    first_rate <- first(trend_df$value)
    last_rate  <- last(trend_df$value)
    if (isTRUE(is.finite(first_rate)) && first_rate != 0) {
      round((last_rate - first_rate) / first_rate * 100, 1)
    } else {
      NA
    }
  } else NA

  cdi_block <- ""
  if (!is.null(cdi_df) && nrow(cdi_df) > 0 && "cdi_value" %in% names(cdi_df)) {
    cdi_trend <- cdi_df |>
      filter(state == !!state | state == "United States") |>
      arrange(year)

    cdi_sample <- cdi_trend |>
      slice(c(1, ceiling(n()/2), n())) |>
      mutate(label = paste0(year, ": ", round(cdi_value, 1)))

    cdi_series <- if (nrow(cdi_sample) > 0) {
      paste(cdi_sample$label, collapse = " → ")
    } else {
      "No CDI values available."
    }

    cdi_overall_change <- if (nrow(cdi_trend) >= 2) {
      first_val <- first(cdi_trend$cdi_value)
      last_val  <- last(cdi_trend$cdi_value)
      if (isTRUE(is.finite(first_val)) && first_val != 0) {
        round((last_val - first_val) / first_val * 100, 1)
      } else {
        NA
      }
    } else NA

    cdi_label_use <- cdi_label %||% cdi_trend$question %||% "Chronic indicator"
    cdi_unit <- cdi_trend$datavalueunit %||% ""

    cdi_block <- glue::glue(
      "\nChronic indicator: {cdi_label_use}\n",
      "CDI unit/type: {cdi_unit}\n",
      "CDI trend sample: {cdi_series}\n",
      "CDI % change across period: {cdi_overall_change}%\n"
    )
  }

  glue::glue(
    "Cause of death: {cause}\n",
    "Geography: {state}\n",
    "Years analyzed: {year_range[1]}–{year_range[2]}\n",
    "Total deaths in period: {scales::comma(stats$total_deaths)}\n",
    "Peak mortality year: {stats$peak_year}\n",
    "Latest {metric_label}: {stats$latest_rate}\n",
    "Overall trend direction: {stats$trend_dir}\n",
    "Trend series ({metric_label}): {year_series}\n",
    "Overall % change across period: {overall_pct_change}%\n",
    "{cdi_block}"
  )
}

summarize_cdi_by_year <- function(df) {
  if (nrow(df) == 0) {
    return(tibble(
      year = integer(),
      state = character(),
      question = character(),
      topic = character(),
      datavaluetype = character(),
      datavalueunit = character(),
      cdi_value = numeric()
    ))
  }
  df |>
    filter(!is.na(datavalue)) |>
    group_by(year, state, question, topic, datavaluetype, datavalueunit) |>
    summarise(cdi_value = mean(datavalue, na.rm = TRUE), .groups = "drop") |>
    arrange(year)
}
