# R/plots.R
# All ggplot2 + plotly visualizations for the CDC Mortality Trend Analyzer

library(ggplot2)
library(plotly)
library(dplyr)
library(scales)
library(stringr)

# -- Shared theme -------------------------------------------------------------
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

# -- Trend line chart ---------------------------------------------------------
#' Mortality trend over time
#'
#' @param trend_df  Tibble from trend_by_year() + add_pct_change()
#' @param cause     Character. Used in title.
#' @param state     Character.
#' @param metric_label Character. Y-axis label.
#' @return plotly object.
plot_trend_line <- function(trend_df, cause, state, metric_label = "Crude Death Rate (per 100k)") {

  if (nrow(trend_df) == 0) return(plotly_empty("No trend data available"))

  p <- trend_df |>
    ggplot(aes(x = year, y = value,
               text = paste0(year, "\n", metric_label, ": ", round(value, 1),
                              "\nChange from baseline: ", pct_change, "%"))) +
    geom_ribbon(aes(ymin = value * 0.95, ymax = value * 1.05),
                fill = "#457B9D", alpha = 0.1) +
    geom_line(color = "#457B9D", linewidth = 1.4) +
    geom_point(color = "#457B9D", size = 2.5, fill = "white", shape = 21, stroke = 1.5) +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = paste0(str_to_title(cause), " - ", state),
      subtitle = metric_label,
      x        = NULL,
      y        = metric_label
    ) +
    theme_mortality()

  ggplotly(p, tooltip = "text")
}

# -- Geographic map -----------------------------------------------------------
#' US state choropleth for selected metric
#'
#' @param geo_df Tibble with state_abbr, state_name, value
#' @param cause Character
#' @param metric_label Character
#' @param year_label Integer
#' @return plotly object
plot_geo_map <- function(geo_df, cause, metric_label, year_label) {
  if (nrow(geo_df) == 0) return(plotly_empty("No geographic data available"))

  plot_ly(
    data = geo_df,
    type = "choropleth",
    locationmode = "USA-states",
    locations = ~state_abbr,
    z = ~value,
    text = ~paste0(state_name, "<br>",
                   metric_label, ": ", comma(value)),
    colors = viridis::viridis(10),
    colorbar = list(title = metric_label)
  ) |>
    layout(
      title = list(text = paste0(str_to_title(cause), " by State (", year_label, ")")),
      geo = list(scope = "usa")
    )
}

# -- Helper -------------------------------------------------------------------
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
