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
ALL_CDI_QUESTIONS <- get_cdi_questions_live()
if (length(ALL_CDI_QUESTIONS) == 0) {
  ALL_CDI_QUESTIONS <- c("All indicators")
}

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
          choices  = c("Age-Adjusted Rate (per 100k)" = "age_adj_rate",
                        "Raw Death Count"             = "deaths"),
          selected = "age_adj_rate"
        ),

        selectInput(
          "cdi_question",
          label    = "Chronic Indicator",
          choices  = ALL_CDI_QUESTIONS,
          selected = ALL_CDI_QUESTIONS[1]
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
          " (NCHS Leading Causes + U.S. Chronic Disease Indicators)",
          class = "text-muted small mb-0"),
        p("Age-adjusted rates per 100,000 U.S. standard population.",
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

      setProgress(0.6)

      cdi_raw <- fetch_chronic_indicators(
        state    = if (input$state == "All") NULL else input$state,
        question = if (input$cdi_question == "All indicators") NULL else input$cdi_question,
        year_min = input$year_range[1],
        year_max = input$year_range[2]
      )

      if ("Overall" %in% cdi_raw$stratification1) {
        cdi_raw <- cdi_raw |>
          dplyr::filter(stratification1 == "Overall")
      } else if ("Overall" %in% cdi_raw$stratificationcategory1) {
        cdi_raw <- cdi_raw |>
          dplyr::filter(stratificationcategory1 == "Overall")
      }

      cdi_year <- summarize_cdi_by_year(cdi_raw)

      merged <- leading |>
        left_join(cdi_year, by = c("year", "state"))

      setProgress(1)
      list(leading = leading, cdi = cdi_year, merged = merged)
    })
  })

  lc_df <- reactive({ req(cdc_result()); cdc_result()$leading })
  cdi_df <- reactive({ req(cdc_result()); cdc_result()$cdi })
  merged_df <- reactive({ req(cdc_result()); cdc_result()$merged })

  # Keep metric choices in sync with available columns
  observeEvent(cdc_result(), {
    has_age_rate <- "age_adj_rate" %in% names(lc_df()) && any(!is.na(lc_df()$age_adj_rate))
    metric_choices <- if (has_age_rate) {
      c("Age-Adjusted Rate (per 100k)" = "age_adj_rate",
        "Raw Death Count" = "deaths")
    } else {
      c("Raw Death Count" = "deaths")
    }
    updateSelectInput(session, "metric", choices = metric_choices,
                      selected = if (has_age_rate) "age_adj_rate" else "deaths")
  }, ignoreInit = TRUE)

  # ── Fetch status UI ──────────────────────────────────────────────────────────
  output$fetch_status_ui <- renderUI({
    req(lc_df(), merged_df())
    n_leading <- nrow(lc_df())
    n_merged <- nrow(merged_df())
    if (n_leading == 0) {
      div(class = "alert alert-warning p-2 small",
          "No records returned. Try adjusting your filters.")
    } else {
      div(class = "alert alert-success p-2 small",
          bs_icon("check-circle"), " ",
          comma(n_leading), " mortality records and ",
          comma(n_merged), " merged records loaded.")
    }
  })

  # ── KPI Cards ────────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    req(lc_df())
    stats <- compute_headline_stats(lc_df(), input$metric)

    latest_label <- if (input$metric == "age_adj_rate")
      "Latest Rate (per 100k)" else "Latest Death Count"

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
        theme    = if (grepl("↑", stats$trend_dir)) "danger" else "success"
      ))
    )
  })

  # ── Trend plot ───────────────────────────────────────────────────────────────
  output$trend_plot <- renderPlotly({
    req(lc_df())

    trend_df <- trend_by_year(lc_df(), metric = input$metric) |>
      add_pct_change() |>
      filter(state == input$state | state == "United States")

    metric_label <- if (input$metric == "age_adj_rate")
      "Age-Adjusted Rate (per 100k)" else "Raw Death Count"

    cdi_label <- input$cdi_question
    plot_trend_with_cdi(trend_df, cdi_df(), input$cause, input$state,
                        metric_label, cdi_label)
  })

  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    req(merged_df())
    table_df <- merged_df()
    if (!"age_adj_rate" %in% names(table_df) || all(is.na(table_df$age_adj_rate))) {
      table_df <- dplyr::select(table_df, -dplyr::any_of("age_adj_rate"))
    }
    table_df
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
      result <- generate_summary_safe(
        cause      = input$cause,
        state      = input$state,
        year_range = input$year_range,
        df         = lc_df(),
        stats      = stats,
        cdi_df     = cdi_df(),
        cdi_label  = input$cdi_question,
        api_key    = Sys.getenv("GEMINI_API_KEY")
      )
      setProgress(1)
      result
    })
  })

  output$llm_output_ui <- renderUI({
    req(llm_text())
    div(
      class = "alert alert-light border mt-2",
      style = "line-height: 1.8; font-size: 1.05rem;",
      p(llm_text())
    )
  })
}

# ── Run ────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
