# Student Analytics & Placement Dashboard

An interactive R Shiny dashboard for exploring a synthetic college cohort's academic performance and placement outcomes.

## Features

- Dynamic KPIs and interactive Plotly charts
- Searchable student directory with department, year and placement filters
- Academic analytics and placement insights

## Technologies

R, Shiny, shinydashboard, ggplot2, dplyr, Plotly and DT.

## Project structure

```text
student-analytics-dashboard/
├── data/students.csv
├── plots/
├── app.R
├── generate_data.R
├── README.md
└── requirements.R
```

## Installation

Open R in this project directory and install any missing dependencies:

```r
source("requirements.R")
```

Generate the reproducible synthetic dataset:

```r
source("generate_data.R")
```

## How to run

```r
shiny::runApp()
```
