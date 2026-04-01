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
ALL_INDICATORS <- get_overdose_indicators()

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

        radioButtons(
          "data_mode",
          label   = "Data Mode",
          choices = c("Leading Causes of Death" = "causes",
                      "Drug Overdose Deaths"    = "overdose"),
          selected = "causes"
        ),

        hr(class = "my-2"),

        conditionalPanel(
          condition = "input.data_mode == 'causes'",

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
            label    = "Metric",
            choices  = c("Age-Adjusted Rate (per 100k)" = "age_adj_rate",
                          "Raw Death Count"             = "deaths"),
            selected = "age_adj_rate"
          )
        ),

        conditionalPanel(
          condition = "input.data_mode == 'overdose'",

          selectInput(
            "overdose_state",
            label    = "State",
            choices  = c("United States", ALL_STATES[-1]),
            selected = "United States"
          ),

          selectInput(
            "overdose_indicator",
            label    = "Drug / Indicator",
            choices  = ALL_INDICATORS,
            selected = ALL_INDICATORS[1]
          ),

          sliderInput(
            "overdose_year_range",
            label = "Year Range",
            min   = 2015,
            max   = as.integer(format(Sys.Date(), "%Y")),
            value = c(2018, as.integer(format(Sys.Date(), "%Y"))),
            step  = 1,
            sep   = ""
          )
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
          " (NCHS/VSRR)",
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
    if (input$data_mode == "causes") {
      withProgress(message = glue("Fetching CDC data for {input$cause}..."), value = 0.3, {
        out <- fetch_leading_causes(
          cause    = if (input$cause == "All causes") NULL else input$cause,
          state    = if (input$state == "All") NULL else input$state,
          year_min = input$year_range[1],
          year_max = input$year_range[2]
        )
        setProgress(1)
        list(mode = "causes", data = out)
      })
    } else {
      withProgress(message = "Fetching CDC overdose data...", value = 0.3, {
        out <- fetch_drug_overdose(
          state     = if (input$overdose_state == "United States") NULL else input$overdose_state,
          indicator = input$overdose_indicator,
          year_min  = input$overdose_year_range[1],
          year_max  = input$overdose_year_range[2]
        )
        setProgress(1)
        list(mode = "overdose", data = out)
      })
    }
  })

  df <- reactive({ req(cdc_result()); cdc_result()$data })
  data_mode <- reactive({ req(cdc_result()); cdc_result()$mode })

  # ── Fetch status UI ──────────────────────────────────────────────────────────
  output$fetch_status_ui <- renderUI({
    req(df())
    n <- nrow(df())
    if (n == 0) {
      div(class = "alert alert-warning p-2 small",
          "No records returned. Try adjusting your filters.")
    } else {
      div(class = "alert alert-success p-2 small",
          bs_icon("check-circle"), " ", comma(n), " records loaded.")
    }
  })

  # ── KPI Cards ────────────────────────────────────────────────────────────────
  output$kpi_cards <- renderUI({
    req(df(), data_mode() == "causes")
    stats <- compute_headline_stats(df(), input$metric)

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
        title    = "Latest Rate (per 100k)",
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
    req(df())

    if (data_mode() == "causes") {
      trend_df <- trend_by_year(df(), metric = input$metric) |>
        add_pct_change() |>
        filter(state == input$state | state == "United States")

      metric_label <- if (input$metric == "age_adj_rate")
        "Age-Adjusted Rate (per 100k)" else "Raw Death Count"

      plot_trend_line(trend_df, input$cause, input$state, metric_label)
    } else {
      trend_df <- overdose_monthly_trend(df(), indicator_filter = input$overdose_indicator)
      plot_overdose_trend(trend_df, input$overdose_state)
    }
  })

  # ── Data table ───────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    req(df())
    df()
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
    req(df(), data_mode() == "causes")
    shiny::validate(shiny::need(nrow(df()) > 0, "No data loaded. Fetch data first."))

    withProgress(message = "Generating AI interpretation...", value = 0.5, {
      stats  <- compute_headline_stats(df(), input$metric)
      result <- generate_summary_safe(
        cause      = input$cause,
        state      = input$state,
        year_range = input$year_range,
        df         = df(),
        stats      = stats,
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
