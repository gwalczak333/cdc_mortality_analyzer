# R/plots.R
# All ggplot2 + plotly visualizations for the CDC Mortality Trend Analyzer

library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(stringr)

# ── Shared theme ──────────────────────────────────────────────────────────────
theme_mortality <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 14, color = "#1a1a2e"),
      plot.subtitle    = element_text(color = "grey45", size = 11),
      axis.text        = element_text(color = "grey35"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

# Color palette for causes
CAUSE_PALETTE <- c(
  "#E63946", "#457B9D", "#2A9D8F", "#E9C46A", "#F4A261",
  "#264653", "#A8DADC", "#6D6875", "#B5838D", "#C77DFF"
)

# ── Trend line chart ──────────────────────────────────────────────────────────
#' Age-adjusted death rate over time
#'
#' @param trend_df  Tibble from trend_by_year() + add_pct_change()
#' @param cause     Character. Used in title.
#' @param state     Character.
#' @param metric_label Character. Y-axis label.
#' @return plotly object.
plot_trend_line <- function(trend_df, cause, state, metric_label = "Age-Adjusted Rate (per 100k)") {

  if (nrow(trend_df) == 0) return(plotly_empty("No trend data available"))

  p <- trend_df |>
    ggplot(aes(x = year, y = value,
               text = paste0(year, "\nRate: ", round(value, 1), " per 100k",
                              "\nChange from baseline: ", pct_change, "%"))) +
    geom_ribbon(aes(ymin = value * 0.95, ymax = value * 1.05),
                fill = "#457B9D", alpha = 0.1) +
    geom_line(color = "#457B9D", linewidth = 1.4) +
    geom_point(color = "#457B9D", size = 2.5, fill = "white", shape = 21, stroke = 1.5) +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste0(str_to_title(cause), " — ", state),
      subtitle = metric_label,
      x        = NULL,
      y        = metric_label
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text")
}

#' Multi-cause comparison trend lines
#'
#' @param df          Tibble from fetch_leading_causes() for multiple causes.
#' @param state       Character.
#' @return plotly object.
plot_multi_cause_trend <- function(df, state) {

  if (nrow(df) == 0) return(plotly_empty())

  trend_df <- df |>
    group_by(year, cause_name) |>
    summarise(value = mean(age_adj_rate, na.rm = TRUE), .groups = "drop")

  causes <- unique(trend_df$cause_name)
  colors <- setNames(CAUSE_PALETTE[seq_along(causes)], causes)

  p <- trend_df |>
    ggplot(aes(x = year, y = value, color = cause_name,
               text = paste0(cause_name, "\n", year, ": ", round(value, 1), " per 100k"))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste0("Cause Comparison — ", state),
      subtitle = "Age-adjusted death rates per 100,000",
      x        = NULL,
      y        = "Age-Adjusted Rate"
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text")
}

# ── Causes comparison bar chart ───────────────────────────────────────────────
#' Horizontal bar chart of top causes by death count or rate
#'
#' @param compare_df  Tibble from compare_causes() with cols: cause, value.
#' @param year_val    Integer or "All years". Used in subtitle.
#' @param metric_label Character.
#' @return plotly object.
plot_causes_bar <- function(compare_df, year_val = NULL, metric_label = "Age-Adjusted Rate") {

  if (nrow(compare_df) == 0) return(plotly_empty())

  subtitle <- if (!is.null(year_val)) paste("Year:", year_val) else "All years averaged"

  p <- compare_df |>
    mutate(cause = forcats::fct_reorder(cause, value)) |>
    ggplot(aes(x = value, y = cause,
               text = paste0(cause, "\n", metric_label, ": ", round(value, 1)))) +
    geom_col(fill = "#E63946", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = round(value, 1)), hjust = -0.1, size = 3.3, color = "grey30") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title    = "Top Causes of Death Comparison",
      subtitle = subtitle,
      x        = metric_label,
      y        = NULL
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text") |>
    layout(margin = list(l = 10))
}

# ── Percent change plot ───────────────────────────────────────────────────────
#' Bar chart of % change from baseline
#'
#' @param pct_df   Tibble with cause_name and pct_change for last year.
#' @return plotly object.
plot_pct_change <- function(pct_df) {

  if (nrow(pct_df) == 0) return(plotly_empty())

  p <- pct_df |>
    mutate(
      cause_name = forcats::fct_reorder(cause_name, pct_change),
      color      = ifelse(pct_change >= 0, "Increased", "Decreased")
    ) |>
    ggplot(aes(x = pct_change, y = cause_name, fill = color,
               text = paste0(cause_name, "\nChange: ", pct_change, "%"))) +
    geom_col(width = 0.7, alpha = 0.9) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.6) +
    scale_fill_manual(values = c("Increased" = "#E63946", "Decreased" = "#2A9D8F")) +
    labs(
      title    = "% Change in Death Rate (First vs. Last Year in Range)",
      subtitle = "Negative = improvement; Positive = worsening",
      x        = "% Change",
      y        = NULL
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text") |>
    layout(margin = list(l = 10))
}

# ── U.S. State choropleth ─────────────────────────────────────────────────────
#' Choropleth map of age-adjusted death rates by state
#'
#' @param state_df  Tibble from state_summary() with cols: state, value.
#' @param cause     Character.
#' @return plotly object.
plot_state_map <- function(state_df, cause) {

  if (nrow(state_df) == 0) return(plotly_empty("No state-level data available"))

  # Add state abbreviations (plotly needs them)
  state_abb_map <- setNames(state.abb, state.name)
  state_abb_map["District of Columbia"] <- "DC"

  state_df <- state_df |>
    mutate(abbr = state_abb_map[state]) |>
    filter(!is.na(abbr))

  plot_ly(
    state_df,
    type       = "choropleth",
    locations  = ~abbr,
    z          = ~value,
    locationmode = "USA-states",
    colorscale = list(c(0, "#d4f1f4"), c(0.5, "#457B9D"), c(1, "#E63946")),
    colorbar   = list(title = "Rate per 100k"),
    text       = ~paste0(state, "\n", round(value, 1), " per 100k"),
    hoverinfo  = "text"
  ) |>
    layout(
      title = list(
        text = paste0(str_to_title(cause), " — Age-Adjusted Rate by State"),
        font = list(size = 14)
      ),
      geo = list(scope = "usa")
    )
}

# ── Drug overdose trend ───────────────────────────────────────────────────────
#' Monthly overdose death trend line
#'
#' @param overdose_df  Tibble from overdose_monthly_trend().
#' @param state_name   Character. For title.
#' @return plotly object.
plot_overdose_trend <- function(overdose_df, state_name = "United States") {

  if (nrow(overdose_df) == 0) return(plotly_empty("No overdose data available"))

  p <- overdose_df |>
    filter(state_name == !!state_name | state_name == "United States") |>
    ggplot(aes(x = date, y = value,
               text = paste0(format(date, "%b %Y"), ": ", comma(round(value))))) +
    geom_line(color = "#E63946", linewidth = 1.2) +
    geom_area(fill = "#E63946", alpha = 0.1) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste0("Drug Overdose Deaths — ", state_name),
      subtitle = "12-month provisional counts",
      x        = NULL,
      y        = "Deaths"
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text")
}

# ── Helper ────────────────────────────────────────────────────────────────────
plotly_empty <- function(msg = "No data available") {
  plot_ly() |>
    layout(
      title      = list(text = msg, font = list(color = "grey60")),
      xaxis      = list(visible = FALSE),
      yaxis      = list(visible = FALSE),
      plot_bgcolor  = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)"
    )
}

