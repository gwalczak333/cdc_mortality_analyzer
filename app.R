# app.R
# CDC Mortality Trend Analyzer — Main Shiny Application
# GLHLTH 562 Final Project
#
# Run locally:  shiny::runApp()
# Deploy:       rsconnect::deployApp()

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(DT)
library(plotly)
library(glue)
library(scales)
library(dotenv)

#source("R/cdc_api.R")
#source("R/clean_data.R")
#source("R/llm_summary.R")
#source("R/plots.R")

library(here)

source(here("R/cdc_api.R"))
source(here("R/clean_data.R"))
source(here("R/llm_summary.R"))
source(here("R/plots.R"))

if (file.exists(".env")) dotenv::load_dot_env(".env")

# ── Static choices ─────────────────────────────────────────────────────────────
ALL_CAUSES <- get_available_causes()
ALL_STATES <- get_available_states()

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "CDC Mortality Trend Analyzer",
  theme = bs_theme(
    bootswatch = "flatly",
    primary    = "#1a1a2e",
    secondary  = "#457B9D",
    danger     = "#E63946",
    base_font  = font_google("Source Sans Pro")
  ),

  # ── Main Explorer ──────────────────────────────────────────────────────────
  nav_panel(
    "Trends",
    icon = bs_icon("graph-up"),

    layout_sidebar(
      sidebar = sidebar(
        width = 290,

        h5("Analysis Parameters", class = "fw-bold mt-1"),
        hr(class = "mt-1 mb-2"),

        selectInput(
          "cause",
          label    = "Cause of Death",
          choices  = ALL_CAUSES,
          selected = "Heart disease"
        ),

        selectInput(
          "state",
          label    = "State / Geography",
          choices  = ALL_STATES,
          selected = "United States"
        ),

        sliderInput(
          "year_range",
          label = "Year Range",
          min   = 1999,
          max   = 2020,
          value = c(2005, 2020),
          step  = 1,
          sep   = ""
        ),

        selectInput(
          "metric",
          label    = "Mortality Metric",
          choices  = c("Age-adjusted Death Rate (per 100k)" = "age_adjusted_rate_100k",
                        "Raw Death Count"                   = "deaths"),
          selected = "age_adjusted_rate_100k"
        ),

        hr(class = "my-2"),

        actionButton(
          "fetch_btn",
          label = "Fetch CDC Data",
          class = "btn-primary w-100 fw-bold"
        ),

        br(), br(),
        uiOutput("fetch_status_ui"),

        hr(class = "my-2"),
        p("Source: ",
          a("data.cdc.gov", href = "https://data.cdc.gov", target = "_blank"),
          " (NCHS Leading Causes of Death)",
          class = "text-muted small mb-0"),
        p("Rates shown per 100,000 population.",
          class = "text-muted small")
      ),

      # ── Main content ────────────────────────────────────────────────────────
      navset_tab(

        # Tab 1: Trend Over Time
        nav_panel(
          "Trend Over Time",
          br(),
          uiOutput("kpi_cards"),
          br(),
          plotlyOutput("trend_plot", height = "420px")
        ),

        # Tab 4: Raw Data
        nav_panel(
          "Data Table",
          br(),
          DTOutput("data_table")
        ),

        # Tab 5: Geographic View
        nav_panel(
          "Geographic View",
          br(),
          uiOutput("geo_year_ui"),
          plotlyOutput("geo_plot", height = "520px")
        ),

        # Tab 5: AI Interpretation
        nav_panel(
          "AI Interpretation",
          br(),
          uiOutput("llm_panel_ui")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Fetch data on button click ───────────────────────────────────────────────
  cdc_result <- eventReactive(input$fetch_btn, {
    withProgress(message = glue("Fetching CDC data for {input$cause}..."), value = 0.2, {
      leading <- fetch_leading_causes(
        cause    = if (input$cause == "All causes") NULL else input$cause,
        state    = if (input$state == "All") NULL else input$state,
        year_min = input$year_range[1],
        year_max = input$year_range[2]
      )

      setProgress(1)
      list(leading = leading)
    })
  })

  lc_df <- reactive({ req(cdc_result()); cdc_result()$leading })

  # Separate fetch for geographic view (needs all states)
  geo_result <- eventReactive(input$fetch_btn, {
    withProgress(message = "Fetching CDC data for geographic view...", value = 0.2, {
      leading <- fetch_leading_causes(
        cause    = if (input$cause == "All causes") NULL else input$cause,
        state    = NULL,
        year_min = input$year_range[1],
        year_max = input$year_range[2],
        limit    = 50000
      )
      setProgress(1)
      list(leading = leading)
    })
  })

  geo_df <- reactive({ req(geo_result()); geo_result()$leading })

  # Keep metric choices in sync with available columns
  observeEvent(cdc_result(), {
    has_aadr <- "age_adjusted_rate_100k" %in% names(lc_df()) &&
      any(!is.na(lc_df()$age_adjusted_rate_100k))
    metric_choices <- if (has_aadr) {
      c("Age-adjusted Death Rate (per 100k)" = "age_adjusted_rate_100k",
        "Raw Death Count"                    = "deaths")
    } else {
      c("Raw Death Count" = "deaths")
    }
    updateSelectInput(session, "metric", choices = metric_choices,
                      selected = if (has_aadr) "age_adjusted_rate_100k" else "deaths")
  }, ignoreInit = TRUE)

  # ── Fetch status UI ──────────────────────────────────────────────────────────
  output$fetch_status_ui <- renderUI({
    req(lc_df())
    n_leading <- nrow(lc_df())
    if (n_leading == 0) {
      div(class = "alert alert-warning p-2 small",
          "No records returned. Try adjusting your filters.")
    } else {
      div(class = "alert alert-success p-2 small",
          bs_icon("check-circle"), " ",
          comma(n_leading), " mortality records loaded.")
    }
  })

  # ── KPI Cards ────────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    req(lc_df())
    stats <- compute_headline_stats(lc_df(), input$metric)

    latest_label <- switch(
      input$metric,
      "age_adjusted_rate_100k" = "Latest Age-adjusted Rate (per 100k)",
      "Latest Death Count"
    )

    fluidRow(
      column(3, value_box(
        title    = "Total Deaths",
        value    = comma(stats$total_deaths),
        showcase = bs_icon("people-fill"),
        theme    = "primary"
      )),
      column(3, value_box(
        title    = "Peak Year",
        value    = stats$peak_year %||% "—",
        showcase = bs_icon("calendar-event"),
        theme    = "secondary"
      )),
      column(3, value_box(
        title    = latest_label,
        value    = stats$latest_rate %||% "—",
        showcase = bs_icon("bar-chart-fill"),
        theme    = "info"
      )),
      column(3, value_box(
        title    = "Trend",
        value    = stats$trend_dir,
        showcase = bs_icon("arrow-up-right"),
        theme    = if (stats$trend_dir == "Up") {
          "danger"
        } else if (stats$trend_dir == "Down") {
          "success"
        } else {
          "secondary"
        }
      ))
    )
  })

  # ── Trend plot ───────────────────────────────────────────────────────────────
  output$trend_plot <- renderPlotly({
    req(lc_df())

    trend_df <- trend_by_year(lc_df(), metric = input$metric) |>
      add_pct_change() |>
      filter(state == input$state | state == "United States")

    metric_label <- switch(
      input$metric,
      "age_adjusted_rate_100k" = "Age-adjusted Death Rate (per 100k)",
      "Raw Death Count"
    )

    plot_trend_line(trend_df, input$cause, input$state, metric_label)
  })

  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    req(lc_df())
    dplyr::select(lc_df(), -dplyr::any_of("rate"))
  },
  options  = list(pageLength = 15, scrollX = TRUE),
  filter   = "top",
  rownames = FALSE
  )

  # ── AI Summary ────────────────────────────────────────────────────────────────
  output$llm_panel_ui <- renderUI({
    tagList(
      div(
        class = "card p-4",
        h5("AI-Generated Epidemiological Interpretation", class = "fw-bold"),
        p("After fetching data, click below to generate a plain-language summary
          of the trend for healthcare students and professionals.", class = "text-muted"),
        actionButton("gen_llm_btn", "Generate AI Interpretation",
                     class = "btn-outline-primary mb-3"),
        uiOutput("llm_output_ui")
      )
    )
  })

  llm_text <- eventReactive(input$gen_llm_btn, {
    req(lc_df())
    shiny::validate(shiny::need(nrow(lc_df()) > 0, "No data loaded. Fetch data first."))

    withProgress(message = "Generating AI interpretation...", value = 0.5, {
      stats  <- compute_headline_stats(lc_df(), input$metric)
      metric_label <- switch(
        input$metric,
        "age_adjusted_rate_100k" = "Age-adjusted Death Rate (per 100k)",
        "Raw Death Count"
      )
      result <- generate_summary_safe(
        cause      = input$cause,
        state      = input$state,
        year_range = input$year_range,
        df         = lc_df(),
        stats      = stats,
        metric     = input$metric,
        metric_label = metric_label,
        api_key    = Sys.getenv("GEMINI_API_KEY")
      )
      setProgress(1)
      result
    })
  })

  output$llm_output_ui <- renderUI({
    req(llm_text())
    cleaned_text <- coerce_llm_output(llm_text())
    if (is.null(cleaned_text) || length(cleaned_text) == 0) {
      cleaned_text <- "Unexpected response format from AI service."
    }
    cleaned_text <- gsub("(?is)\\bmodel\\s+stop\\b.*$", "", cleaned_text, perl = TRUE)
    cleaned_text <- gsub("(?is)\\bgemini-[^\\n]*$", "", cleaned_text, perl = TRUE)
    cleaned_text <- trimws(cleaned_text)
    div(
      class = "alert alert-light border mt-2",
      style = "line-height: 1.8; font-size: 1.05rem;",
      p(cleaned_text)
    )
  })

  # ── Geographic view ─────────────────────────────────────────────────────────
  output$geo_plot <- renderPlotly({
    req(geo_df())

    geo_df <- geo_df() |>
      filter(state != "United States") |>
      mutate(
        state_abbr = dplyr::case_when(
          nchar(state) == 2 ~ toupper(state),
          tolower(state) == "district of columbia" ~ "DC",
          TRUE ~ state.abb[match(tolower(state), tolower(state.name))]
        ),
        state_name = dplyr::case_when(
          tolower(state) == "district of columbia" ~ "District of Columbia",
          nchar(state) == 2 ~ state.name[match(toupper(state), state.abb)],
          TRUE ~ state
        )
      ) |>
      filter(!is.na(state_abbr))

    metric_use <- input$metric
    if (!metric_use %in% names(geo_df)) metric_use <- "deaths"
    if (!is.numeric(geo_df[[metric_use]])) {
      geo_df[[metric_use]] <- readr::parse_number(as.character(geo_df[[metric_use]]))
    }

    year_use <- input$geo_year
    if (is.null(year_use) || is.na(year_use)) {
      year_use <- max(geo_df$year, na.rm = TRUE)
    }

    geo_filtered <- geo_df |>
      filter(year == year_use)

    if (nrow(geo_filtered) == 0) {
      return(plotly_empty("No geographic data for selected year."))
    }

    geo_summarized <- geo_filtered |>
      group_by(state_abbr, state_name) |>
      summarise(
        value = if (metric_use == "deaths") sum(.data[[metric_use]], na.rm = TRUE)
        else mean(.data[[metric_use]], na.rm = TRUE),
        .groups = "drop"
      )

    metric_label <- switch(
      metric_use,
      "age_adjusted_rate_100k" = "Age-adjusted Death Rate (per 100k)",
      "Raw Death Count"
    )

    plot_geo_map(
      geo_summarized,
      cause = input$cause,
      metric_label = metric_label,
      year_label = year_use
    )
  })

  output$geo_year_ui <- renderUI({
    req(geo_df())
    years <- seq(input$year_range[1], input$year_range[2])
    if (length(years) == 0) return(NULL)
    selected_year <- input$geo_year
    if (is.null(selected_year) || !(selected_year %in% years)) {
      selected_year <- max(years)
    }
    selectInput(
      "geo_year",
      label = "Map Year (within selected range)",
      choices = years,
      selected = selected_year
    )
  })
}

# ── Run ────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
