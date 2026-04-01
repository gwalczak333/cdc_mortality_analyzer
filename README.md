# CDC Mortality Trend Analyzer

An interactive Shiny application for exploring U.S. mortality trends using live data from the **CDC National Center for Health Statistics (NCHS)** and the **Vital Statistics Rapid Release (VSRR)** program. Our app will call the CDC's open data API whenever a user makes a selection. Users select a cause of death, state, and time period — the app fetches real-time data, produces interactive visualizations, and generates AI-powered plain-language interpretations.

Built for GLHLTH 562: Data Science and Visualization with R.

---

## Pipeline Documentation

### 1. Where the Data Comes From

All data is fetched live at runtime from two CDC open data APIs hosted on **data.cdc.gov** (Socrata):

| Dataset | Endpoint | Coverage |
|---|---|---|
| NCHS Leading Causes of Death: United States | `data.cdc.gov/resource/bi63-dtpu.json` | 1999–2020, all states |
| VSRR Provisional Drug Overdose Death Counts | `data.cdc.gov/resource/xkb8-kh2a.json` | 2015–present, monthly |

No static CSV files are downloaded. Every search triggers a live API call using SoQL (Socrata Query Language) to filter by cause, state, and year range server-side before returning results.

- CDC data documentation: https://data.cdc.gov
- NCHS dataset: https://data.cdc.gov/NCHS/NCHS-Leading-Causes-of-Death-United-States/bi63-dtpu
- VSRR dataset: https://data.cdc.gov/NCHS/VSRR-Provisional-Drug-Overdose-Death-Counts/xkb8-kh2a

---

### 2. How the Data Is Ingested

**Package:** `httr2` for HTTP requests; `jsonlite` for JSON parsing

**Flow — Leading Causes:**

```
User input (cause + state + year range)
    ↓
build WHERE clause (SoQL)
    ↓
fetch_leading_causes()  →  GET data.cdc.gov/resource/bi63-dtpu.json
    ↓
parse_leading_causes()  →  map_dfr() → tidy tibble
```

**Flow — Drug Overdose:**

```
User input (state + indicator + year range)
    ↓
fetch_drug_overdose()   →  GET data.cdc.gov/resource/xkb8-kh2a.json
    ↓
parse_drug_overdose()   →  map_dfr() → tidy tibble
    ↓
add_overdose_date()     →  parse month/year → Date column
```

**Authentication:** No CDC token is required to access these open datasets.

**Key fields extracted:**

| Field | Source column |
|---|---|
| Year | `year` |
| Cause name | `cause_name` |
| State | `state` |
| Death count | `deaths` |
| Age-adjusted rate | `age_adjusted_death_rate` |
| Overdose indicator | `indicator` |
| Overdose count | `data_value` |

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
| `overdose_monthly_trend()` | Reshapes overdose data into monthly time series |
| `overdose_by_year()` | Aggregates overdose data annually |
| `build_llm_data_summary()` | Formats a structured text block for the LLM prompt |

**GenAI pipeline** (`R/llm_summary.R`):

```
processed stats + trend series
    ↓
build_llm_data_summary()   →  structured text summary
    ↓
build_prompt()             →  epidemiologist system prompt + user prompt
    ↓
POST https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent
    ↓
4–6 sentence plain-language interpretation displayed in app
```

The LLM prompt instructs the model to act as a public health epidemiologist, describe the trend direction, contextualize the rate, propose plausible explanations, and note important caveats (ICD-10 coding, COVID-19, age-adjustment).

---

### 4. What the Output Is

A hosted interactive Shiny app with one main view and 3 tabs:

**Trends View:**

| Tab | Contents |
|---|---|
| Trend Over Time | KPI cards + age-adjusted rate line chart with ribbon |
| Data Table | Filterable/searchable raw data (DT) |
| AI Interpretation | Gemini-generated plain-language interpretation |

---

### 5. How to Run It

#### Prerequisites

- R ≥ 4.2
- RStudio (recommended)

#### Install packages

```r
install.packages(c(
  "shiny", "bslib", "bsicons",
  "httr2", "jsonlite",
  "dplyr", "tidyr", "purrr", "stringr", "lubridate",
  "ggplot2", "plotly", "scales", "forcats",
  "DT", "glue", "dotenv"
))
```

#### Set up API keys

```bash
cp .env.example .env
# Edit .env with your keys
```

- Gemini API key: https://aistudio.google.com/app/apikey

#### Run locally

```r
shiny::runApp()
```

#### Deploy to shinyapps.io

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

> ⚠️ Never commit your `.env` file. Use the shinyapps.io dashboard for environment variables in production.

---

## Project Structure

```
cdc-mortality-explorer/
├── README.md               ← Pipeline documentation (this file)
├── app.R                   ← Main Shiny application
├── .env.example            ← API key template (copy to .env)
├── .gitignore
├── R/
│   ├── cdc_api.R           ← CDC data.cdc.gov API fetch functions
│   ├── clean_data.R        ← Wrangling, summary, and LLM prep functions
│   ├── llm_summary.R       ← Gemini API integration + prompt builder
│   └── plots.R             ← ggplot2 + plotly visualizations
├── data/
│   └── cached/             ← Optional: locally cached API responses
└── deck/
    └── presentation.qmd    ← Quarto slides for final presentation
```

---

## Capabilities Checklist

| Requirement | Met? | How |
|---|---|---|
| User input | ✅ | Cause selector, state selector, year range slider, metric toggle, drug indicator |
| API integration | ✅ | Live Socrata API calls to data.cdc.gov via `httr2` |
| GenAI in pipeline | ✅ | Gemini generates plain-language epidemiological interpretation |

---

## Key Caveats

- **Age-adjusted rates** are the preferred metric for comparing mortality across states and over time; raw counts are affected by population size.
- **2020 data** may reflect COVID-19 mortality misattribution and disruptions to routine healthcare.
- **ICD-10 coding** changes in 1999 can introduce discontinuities for some causes.
- **Provisional overdose counts** are subject to revision as death certificates are processed (up to 8-month lag).

---

## License

Course project — GLHLTH 562, Duke University.

