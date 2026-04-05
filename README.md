# CDC Mortality Trend Analyzer

An interactive Shiny application for exploring U.S. mortality trends using live data from the CDC National Center for Health Statistics (NCHS). The app calls the CDC open data API whenever a user makes a selection. Users select a cause of death, state, and time period; the app fetches real-time data, produces interactive visualizations, and generates AI-powered plain-language interpretations.

Built for GLHLTH 562: Data Science and Visualization with R.

---

## Pipeline Documentation

### 1. Where the Data Comes From

All data is fetched live at runtime from the CDC open data API hosted on data.cdc.gov (Socrata):

| Dataset | Endpoint | Coverage |
|---|---|---|
| NCHS Leading Causes of Death: United States | `data.cdc.gov/resource/bi63-dtpu.json` | 1999-2017, all states |

No static CSV files are downloaded. Every search triggers a live API call using SoQL (Socrata Query Language) to filter by cause, state, and year range server-side before returning results.

- CDC data documentation: https://data.cdc.gov
- NCHS dataset: https://data.cdc.gov/NCHS/NCHS-Leading-Causes-of-Death-United-States/bi63-dtpu

---

### 2. How the Data Is Ingested

Package: httr2 for HTTP requests; jsonlite for JSON parsing

Flow - Leading Causes:

```
User input (cause + state + year range)
    ->
build WHERE clause (SoQL)
    ->
fetch_leading_causes()  ->  GET data.cdc.gov/resource/bi63-dtpu.json
    ->
parse_leading_causes()  ->  map_dfr() -> tidy tibble
```

Authentication: No CDC token is required to access this open dataset.

Key fields extracted:

| Field | Source column |
|---|---|
| Year | `year` |
| Cause name | `cause_name` |
| State | `state` |
| Death count | `deaths` |
| Crude death rate | `rate` |

---

### 3. How the Data Is Processed

All wrangling is in `R/clean_data.R`:

| Function | Purpose |
|---|---|
| `trend_by_year()` | Aggregates deaths/rates by year for time series |
| `add_pct_change()` | Computes % change from baseline year |
| `compare_causes()` | Ranks causes by total deaths or rate |
| `state_summary()` | Averages rates per state for choropleth |
| `compute_headline_stats()` | Computes KPI card values (total deaths, peak year, trend direction) |
| `build_llm_data_summary()` | Formats a structured text block for the LLM prompt |

GenAI pipeline (`R/llm_summary.R`):

```
processed stats + trend series
    ->
build_llm_data_summary()   ->  structured text summary
    ->
build_prompt()             ->  epidemiologist system prompt + user prompt
    ->
POST https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent
    ->
4-6 sentence plain-language interpretation displayed in app
```

The LLM prompt instructs the model to act as a public health epidemiologist, describe the trend direction, contextualize the rate, propose plausible explanations, and note important caveats (ICD-10 coding changes, reporting lags, population shifts).

---

### 4. What the Output Is

A hosted interactive Shiny app with one main view and 3 tabs:

Trends View:

| Tab | Contents |
|---|---|
| Trend Over Time | KPI cards + crude rate or death count line chart with ribbon |
| Data Table | Filterable/searchable raw data (DT) |
| Geographic View | State-level choropleth map of rates/counts |
| AI Interpretation | Gemini-generated plain-language interpretation |

---

### 5. How to Run It

Prerequisites:
- R >= 4.2
- RStudio (recommended)

Install packages:

```r
install.packages(c(
  "shiny", "bslib", "bsicons",
  "httr2", "jsonlite",
  "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "ggplot2", "plotly", "scales", "forcats",
  "DT", "glue", "dotenv"
))
```

Set up API keys:

```bash
cp .env.example .env
# Edit .env with your keys
```

- Gemini API key: https://aistudio.google.com/app/apikey

Run locally:

```r
shiny::runApp()
```

Deploy to shinyapps.io:

```r
install.packages("rsconnect")

# Configure account
rsconnect::setAccountInfo(
  name   = "YOUR_SHINYAPPS_USERNAME",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Set environment variables in shinyapps.io dashboard:
# Apps > [Your App] > Settings > Environment Variables
# Add: GEMINI_API_KEY

rsconnect::deployApp()
```

Note: Never commit your .env file. Use the shinyapps.io dashboard for environment variables in production.

---

## Project Structure

```
cdc-mortality-explorer/
+-- README.md               <- Pipeline documentation (this file)
+-- app.R                   <- Main Shiny application
+-- .env.example            <- API key template (copy to .env)
+-- .gitignore
+-- R/
?   +-- cdc_api.R           <- CDC data.cdc.gov API fetch functions
?   +-- clean_data.R        <- Wrangling, summary, and LLM prep functions
?   +-- llm_summary.R       <- Gemini API integration + prompt builder
?   +-- plots.R             <- ggplot2 + plotly visualizations
+-- data/
?   +-- cached/             <- Optional: locally cached API responses
```