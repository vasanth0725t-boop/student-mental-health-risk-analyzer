library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

data_file <- file.path("data", "students.csv")
if (!file.exists(data_file)) {
  stop("Student data was not found. Run generate_data.R first.", call. = FALSE)
}

students <- read.csv(data_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
required_columns <- c(
  "Student_ID", "Name", "Gender", "Department", "Year", "CGPA", "Attendance",
  "Study_Hours", "Internship", "Backlogs", "Technical_Score",
  "Communication_Score", "Placement_Status", "Salary"
)
missing_columns <- setdiff(required_columns, names(students))
if (length(missing_columns) > 0) {
  stop(
    "Student data is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

department_order <- c("CSE", "AI&DS", "ECE", "EEE", "MECH")
students$Department <- factor(students$Department, levels = department_order)
students$Placement_Status <- factor(
  students$Placement_Status,
  levels = c("Placed", "Not Placed")
)

format_percent <- function(value) {
  paste0(format(round(value, 1), nsmall = 1), "%")
}

theme_dashboard <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", color = "#203047", size = 14),
      plot.subtitle = element_text(color = "#718096"),
      axis.title = element_text(color = "#596579"),
      axis.text = element_text(color = "#596579"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(12, 16, 12, 12)
    )
}

card_plot <- function(plot) {
  ggplotly(plot, tooltip = "text") %>%
    layout(
      margin = list(l = 45, r = 18, b = 45, t = 20),
      font = list(family = "Inter, Segoe UI, sans-serif", color = "#596579"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Student Analytics", titleWidth = 265),
  dashboardSidebar(
    width = 265,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge-high")),
      menuItem("Students", tabName = "students", icon = icon("users")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line")),
      menuItem("Placement", tabName = "placement", icon = icon("briefcase")),
      menuItem("About", tabName = "about", icon = icon("circle-info"))
    ),
    div(
      class = "sidebar-note",
      "ACADEMIC INSIGHTS",
      tags$small("Student outcomes at a glance")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "preconnect",
        href = "https://fonts.googleapis.com"
      ),
      tags$link(
        rel = "preconnect",
        href = "https://fonts.gstatic.com",
        crossorigin = "anonymous"
      ),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        body, .content-wrapper, .right-side { background: #f4f7fb; }
        body, h1, h2, h3, h4, .main-header .logo, .sidebar-menu { font-family: 'DM Sans', 'Segoe UI', sans-serif; }
        .main-header .logo { background: #142338 !important; color: #fff !important; font-weight: 700; letter-spacing: -.3px; }
        .main-header .navbar { background: #142338 !important; }
        .main-sidebar { background: #142338 !important; }
        .sidebar-menu>li>a { color: #b9c6d8 !important; padding: 14px 18px; }
        .sidebar-menu>li.active>a, .sidebar-menu>li:hover>a { background: #203650 !important; border-left-color: #54b6a7 !important; color: #fff !important; }
        .sidebar-note { position: absolute; bottom: 24px; left: 22px; color: #7890ac; font-size: 10px; font-weight: 700; letter-spacing: 1.4px; }
        .sidebar-note small { display: block; margin-top: 7px; color: #c0ccda; font-size: 12px; font-weight: 400; letter-spacing: 0; }
        .content-wrapper { min-height: 100vh; }
        .content { padding: 24px 28px 32px; }
        .page-heading { margin: 4px 0 22px; }
        .page-heading h1 { margin: 0 0 5px; color: #1c2d42; font-size: 26px; font-weight: 700; }
        .page-heading p { margin: 0; color: #7d899b; font-size: 14px; }
        .panel-card, .small-box, .info-box { border-radius: 12px !important; box-shadow: 0 5px 22px rgba(30, 52, 77, .06) !important; }
        .panel-card { background: #fff; padding: 18px 18px 10px; margin-bottom: 20px; border: 1px solid #edf1f5; transition: transform .18s ease, box-shadow .18s ease; }
        .panel-card:hover { transform: translateY(-2px); box-shadow: 0 9px 26px rgba(30, 52, 77, .10) !important; }
        .panel-card h3 { margin: 0 0 14px; color: #26384e; font-size: 15px; font-weight: 700; }
        .small-box { min-height: 130px; }
        .small-box>.inner { padding: 20px 20px 16px; }
        .small-box h3 { font-size: 27px; font-weight: 700; }
        .small-box p { color: rgba(255,255,255,.86); font-size: 13px; }
        .small-box .icon-large { top: 21px; right: 17px; }
        .bg-aqua { background: #367fbe !important; }
        .bg-green { background: #289782 !important; }
        .bg-yellow { background: #d29a39 !important; }
        .bg-purple { background: #6c75b8 !important; }
        .filter-row { background: #fff; padding: 15px 18px 3px; margin: 0 0 18px; border: 1px solid #edf1f5; border-radius: 12px; box-shadow: 0 4px 16px rgba(30,52,77,.04); }
        .filter-row .control-label { color: #536176; font-size: 12px; }
        .filter-row .form-control { border-color: #e2e8f0; border-radius: 7px; box-shadow: none; }
        .summary-card { background: #edf6f5; color: #345b59; padding: 17px 20px; border-radius: 10px; margin: 4px 0 18px; line-height: 1.65; }
        .insight-list { padding-left: 19px; margin: 0 0 12px; color: #526175; line-height: 1.9; }
        .about-card { background: #fff; padding: 24px 26px; border: 1px solid #edf1f5; border-radius: 12px; box-shadow: 0 5px 22px rgba(30,52,77,.06); margin-bottom: 18px; }
        .about-card h2 { margin-top: 0; color: #203047; font-size: 21px; font-weight: 700; }
        .about-card h3 { color: #34465c; font-size: 15px; font-weight: 700; margin-top: 22px; }
        .about-card p, .about-card li { color: #647286; line-height: 1.8; }
        .about-footer { color: #748197; font-weight: 600; border-top: 1px solid #edf1f5; padding-top: 16px; margin-top: 20px; }
        table.dataTable thead th { color: #526176; font-size: 12px; }
        table.dataTable { color: #435269; }
        @media (max-width: 767px) { .content { padding: 18px 12px; } .page-heading h1 { font-size: 22px; } }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            12,
            div(
              class = "page-heading",
              h1("Student Analytics Dashboard"),
              p("Academic Performance & Placement Insights")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("kpi_students", width = 3),
          valueBoxOutput("kpi_cgpa", width = 3),
          valueBoxOutput("kpi_placement", width = 3),
          valueBoxOutput("kpi_salary", width = 3)
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3("Placement Overview"), plotlyOutput("overview_donut", height = 270))),
          column(6, div(class = "panel-card", h3("Department Performance"), plotlyOutput("overview_department", height = 270)))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3("CGPA Distribution"), plotlyOutput("overview_histogram", height = 300))),
          column(6, div(class = "panel-card", h3("CGPA vs Placement"), plotlyOutput("overview_scatter", height = 300)))
        )
      ),
      tabItem(
        tabName = "students",
        div(class = "page-heading", h1("Students"), p("Search, filter and explore the student cohort.")),
        fluidRow(
          column(
            12,
            div(
              class = "filter-row",
              fluidRow(
                column(4, selectInput("filter_department", "Department", choices = c("All departments", department_order), selected = "All departments")),
                column(4, selectInput("filter_year", "Year", choices = c("All years", sort(unique(students$Year))), selected = "All years")),
                column(4, selectInput("filter_placement", "Placement status", choices = c("All statuses", "Placed", "Not Placed"), selected = "All statuses"))
              )
            )
          )
        ),
        fluidRow(
          column(12, div(class = "panel-card", h3("Student Directory"), DTOutput("students_table")))
        )
      ),
      tabItem(
        tabName = "analytics",
        div(class = "page-heading", h1("Academic Analytics"), p("Explore academic patterns across departments and student outcomes.")),
        fluidRow(column(12, uiOutput("academic_summary"))),
        fluidRow(
          column(6, div(class = "panel-card", h3("Average CGPA by Department"), plotlyOutput("analytics_cgpa", height = 290))),
          column(6, div(class = "panel-card", h3("Attendance by Department"), plotlyOutput("analytics_attendance", height = 290)))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3("Study Hours vs CGPA"), plotlyOutput("analytics_study", height = 300))),
          column(6, div(class = "panel-card", h3("Technical Score vs CGPA"), plotlyOutput("analytics_technical", height = 300)))
        )
      ),
      tabItem(
        tabName = "placement",
        div(class = "page-heading", h1("Placement Insights"), p("A closer look at placement outcomes and salary trends.")),
        fluidRow(
          column(12, valueBoxOutput("placement_rate_box", width = 12))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3("Placement Rate by Department"), plotlyOutput("placement_department", height = 290))),
          column(6, div(class = "panel-card", h3("Placement Rate by CGPA Range"), plotlyOutput("placement_cgpa", height = 290)))
        ),
        fluidRow(
          column(6, div(class = "panel-card", h3("Placement Rate by Internship"), plotlyOutput("placement_internship", height = 290))),
          column(6, div(class = "panel-card", h3("Salary Distribution (LPA)"), plotlyOutput("placement_salary", height = 290)))
        ),
        fluidRow(
          column(
            12,
            div(class = "panel-card", h3("Key Placement Insights"), uiOutput("placement_insights"))
          )
        )
      ),
      tabItem(
        tabName = "about",
        div(class = "page-heading", h1("About"), p("A clear, interactive view of academic performance and placement outcomes.")),
        fluidRow(
          column(
            12,
            div(
              class = "about-card",
              h2("Student Analytics & Placement Dashboard"),
              h3("Project Objective"),
              p("This project provides an interactive visualization of student academic performance and placement outcomes."),
              h3("Technologies"),
              p("R · Shiny · ggplot2 · Plotly · DT · dplyr"),
              h3("Features"),
              tags$ul(
                tags$li("Interactive dashboard with dynamic KPIs"),
                tags$li("Searchable and filterable student directory"),
                tags$li("Academic performance analytics"),
                tags$li("Placement and salary analysis"),
                tags$li("Interactive charts with hover details")
              ),
              div(class = "about-footer", "Developed as an Academic Data Analytics Project")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  placement_rate <- mean(students$Placement_Status == "Placed") * 100
  average_salary <- mean(students$Salary, na.rm = TRUE)

  output$kpi_students <- renderValueBox({
    valueBox(
      value = nrow(students),
      subtitle = "Total Students",
      icon = icon("users"),
      color = "aqua"
    )
  })
  output$kpi_cgpa <- renderValueBox({
    valueBox(
      value = sprintf("%.2f", mean(students$CGPA, na.rm = TRUE)),
      subtitle = "Average CGPA",
      icon = icon("graduation-cap"),
      color = "green"
    )
  })
  output$kpi_placement <- renderValueBox({
    valueBox(
      value = format_percent(placement_rate),
      subtitle = "Placement Rate",
      icon = icon("briefcase"),
      color = "yellow"
    )
  })
  output$kpi_salary <- renderValueBox({
    valueBox(
      value = paste0("₹", sprintf("%.2f", average_salary), " LPA"),
      subtitle = "Average Salary",
      icon = icon("indian-rupee-sign"),
      color = "purple"
    )
  })
  output$placement_rate_box <- renderValueBox({
    valueBox(
      value = format_percent(placement_rate),
      subtitle = "Overall Placement Rate",
      icon = icon("chart-pie"),
      color = "green"
    )
  })

  output$overview_donut <- renderPlotly({
    placement_counts <- students %>%
      count(Placement_Status) %>%
      mutate(
        label = as.character(Placement_Status),
        text = paste0(label, ": ", n, " students")
      )
    plot_ly(
      placement_counts,
      labels = ~label,
      values = ~n,
      type = "pie",
      hole = 0.58,
      textinfo = "label+percent",
      hovertemplate = "%{label}: %{value} students<extra></extra>",
      marker = list(colors = c("#289782", "#dbe3ec"), line = list(color = "#ffffff", width = 2))
    ) %>%
      plotly::layout(
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08),
        margin = list(l = 15, r = 15, b = 35, t = 15),
        font = list(family = "Inter, Segoe UI, sans-serif", color = "#596579"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  })
  output$overview_department <- renderPlotly({
    department_rates <- students %>%
      group_by(Department) %>%
      summarise(
        Placement_Rate = mean(Placement_Status == "Placed") * 100,
        text = paste0(as.character(first(Department)), ": ", round(Placement_Rate, 1), "% placed"),
        .groups = "drop"
      )
    chart <- ggplot(department_rates, aes(x = Department, y = Placement_Rate, text = text)) +
      geom_col(fill = "#367fbe", width = 0.62) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Placement rate") +
      theme_dashboard()
    card_plot(chart)
  })
  output$overview_histogram <- renderPlotly({
    chart <- ggplot(students, aes(x = CGPA, text = paste0("CGPA: ", CGPA))) +
      geom_histogram(binwidth = 0.4, fill = "#367fbe", color = "white", boundary = 5) +
      labs(x = "CGPA", y = "Students") +
      theme_dashboard()
    card_plot(chart)
  })
  output$overview_scatter <- renderPlotly({
    chart <- ggplot(
      students,
      aes(
        x = CGPA,
        y = Technical_Score,
        color = Placement_Status,
        text = paste0(
          "CGPA: ", CGPA,
          "<br>Technical score: ", Technical_Score,
          "<br>Placement: ", Placement_Status
        )
      )
    ) +
      geom_point(alpha = 0.72, size = 2.3) +
      scale_color_manual(values = c("Placed" = "#289782", "Not Placed" = "#8d9aac")) +
      labs(x = "CGPA", y = "Technical score", color = "Placement") +
      theme_dashboard()
    card_plot(chart)
  })

  filtered_students <- reactive({
    result <- students
    if (!is.null(input$filter_department) && input$filter_department != "All departments") {
      result <- result %>% filter(Department == input$filter_department)
    }
    if (!is.null(input$filter_year) && input$filter_year != "All years") {
      result <- result %>% filter(Year == as.integer(input$filter_year))
    }
    if (!is.null(input$filter_placement) && input$filter_placement != "All statuses") {
      result <- result %>% filter(Placement_Status == input$filter_placement)
    }
    result
  })
  output$students_table <- renderDT({
    table_data <- filtered_students() %>%
      transmute(
        `Student ID` = Student_ID,
        Name,
        Department,
        Year,
        CGPA,
        Attendance = paste0(sprintf("%.1f", Attendance), "%"),
        Internship,
        Backlogs,
        Placement = Placement_Status,
        Salary = ifelse(is.na(Salary), "—", paste0("₹", sprintf("%.1f", Salary), " LPA"))
      )
    datatable(
      table_data,
      rownames = FALSE,
      filter = "none",
      options = list(pageLength = 10, lengthChange = FALSE, scrollX = TRUE, dom = "ftip"),
      class = "stripe hover compact"
    )
  }, server = FALSE)

  department_summary <- students %>%
    group_by(Department) %>%
    summarise(
      Average_CGPA = mean(CGPA),
      Average_Attendance = mean(Attendance),
      .groups = "drop"
    )

  output$academic_summary <- renderUI({
    top_department <- department_summary %>%
      slice_max(Average_CGPA, n = 1, with_ties = FALSE)
    div(
      class = "summary-card",
      strong(paste0(as.character(top_department$Department), " students show the highest average CGPA")),
      paste0(" at ", sprintf("%.2f", top_department$Average_CGPA), ". Across all ", nrow(students),
             " students, average attendance is ", sprintf("%.1f", mean(students$Attendance)), "%.")
    )
  })
  output$analytics_cgpa <- renderPlotly({
    chart <- ggplot(
      department_summary,
      aes(x = Department, y = Average_CGPA, text = paste0(as.character(Department), ": ", sprintf("%.2f", Average_CGPA)))
    ) +
      geom_col(fill = "#367fbe", width = 0.62) +
      scale_y_continuous(limits = c(0, 10), expand = expansion(mult = c(0, 0.03))) +
      labs(x = NULL, y = "Average CGPA") +
      theme_dashboard()
    card_plot(chart)
  })
  output$analytics_attendance <- renderPlotly({
    chart <- ggplot(
      department_summary,
      aes(x = Department, y = Average_Attendance, text = paste0(as.character(Department), ": ", sprintf("%.1f", Average_Attendance), "%"))
    ) +
      geom_col(fill = "#4da99c", width = 0.62) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.03))) +
      labs(x = NULL, y = "Average attendance") +
      theme_dashboard()
    card_plot(chart)
  })
  output$analytics_study <- renderPlotly({
    chart <- ggplot(
      students,
      aes(x = Study_Hours, y = CGPA, color = Department, text = paste0("Study hours: ", Study_Hours, "<br>CGPA: ", CGPA, "<br>", Department))
    ) +
      geom_point(alpha = 0.72, size = 2.2) +
      labs(x = "Study hours per day", y = "CGPA", color = "Department") +
      theme_dashboard()
    card_plot(chart)
  })
  output$analytics_technical <- renderPlotly({
    chart <- ggplot(
      students,
      aes(x = Technical_Score, y = CGPA, color = Department, text = paste0("Technical score: ", Technical_Score, "<br>CGPA: ", CGPA, "<br>", Department))
    ) +
      geom_point(alpha = 0.72, size = 2.2) +
      labs(x = "Technical score", y = "CGPA", color = "Department") +
      theme_dashboard()
    card_plot(chart)
  })

  output$placement_department <- renderPlotly({
    summary <- students %>%
      group_by(Department) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, .groups = "drop")
    chart <- ggplot(summary, aes(x = Department, y = Rate, text = paste0(as.character(Department), ": ", round(Rate, 1), "%"))) +
      geom_col(fill = "#289782", width = 0.62) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.03))) +
      labs(x = NULL, y = "Placement rate") +
      theme_dashboard()
    card_plot(chart)
  })
  output$placement_cgpa <- renderPlotly({
    range_data <- students %>%
      mutate(CGPA_Range = cut(CGPA, breaks = c(-Inf, 6, 7, 8, 9, Inf), labels = c("< 6", "6–7", "7–8", "8–9", "9+"), right = FALSE)) %>%
      group_by(CGPA_Range, .drop = FALSE) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, Students = n(), .groups = "drop")
    chart <- ggplot(range_data, aes(x = CGPA_Range, y = Rate, text = paste0(CGPA_Range, ": ", round(Rate, 1), "% (", Students, " students)"))) +
      geom_col(fill = "#367fbe", width = 0.66) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.03))) +
      labs(x = "CGPA range", y = "Placement rate") +
      theme_dashboard()
    card_plot(chart)
  })
  output$placement_internship <- renderPlotly({
    internship_summary <- students %>%
      group_by(Internship) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, .groups = "drop")
    chart <- ggplot(internship_summary, aes(x = Internship, y = Rate, text = paste0("Internship ", Internship, ": ", round(Rate, 1), "% placed"))) +
      geom_col(fill = "#4da99c", width = 0.55) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.03))) +
      labs(x = NULL, y = "Placement rate") +
      theme_dashboard()
    card_plot(chart)
  })
  output$placement_salary <- renderPlotly({
    chart <- ggplot(
      students %>% filter(Placement_Status == "Placed", !is.na(Salary)),
      aes(x = Salary, text = paste0("Salary: ₹", Salary, " LPA"))
    ) +
      geom_histogram(binwidth = 1, fill = "#7581bc", color = "white", boundary = 2) +
      labs(x = "Salary (LPA)", y = "Placed students") +
      theme_dashboard()
    card_plot(chart)
  })

  output$placement_insights <- renderUI({
    internship_rates <- students %>%
      group_by(Internship) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, .groups = "drop")
    internship_yes <- internship_rates$Rate[internship_rates$Internship == "Yes"]
    internship_no <- internship_rates$Rate[internship_rates$Internship == "No"]
    cgpa_median <- median(students$CGPA)
    cgpa_rates <- students %>%
      mutate(CGPA_Group = ifelse(CGPA >= cgpa_median, "at or above", "below")) %>%
      group_by(CGPA_Group) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, .groups = "drop")
    higher_cgpa <- cgpa_rates$Rate[cgpa_rates$CGPA_Group == "at or above"]
    lower_cgpa <- cgpa_rates$Rate[cgpa_rates$CGPA_Group == "below"]
    department_rates <- students %>%
      group_by(Department) %>%
      summarise(Rate = mean(Placement_Status == "Placed") * 100, .groups = "drop")
    top_department <- department_rates %>% slice_max(Rate, n = 1, with_ties = FALSE)
    bullets <- c(
      if (length(internship_yes) && length(internship_no) && internship_yes >= internship_no) {
        paste0("Students with internships have a higher placement rate (", round(internship_yes, 1), "%) than those without (", round(internship_no, 1), "%).")
      } else if (length(internship_yes) && length(internship_no)) {
        paste0("Students without internships have a higher placement rate (", round(internship_no, 1), "%) than those with internships (", round(internship_yes, 1), "%).")
      },
      if (length(higher_cgpa) && length(lower_cgpa) && higher_cgpa >= lower_cgpa) {
        paste0("Students at or above the cohort median CGPA (", sprintf("%.2f", cgpa_median), ") have a higher placement rate.")
      } else if (length(higher_cgpa) && length(lower_cgpa)) {
        paste0("Students below the cohort median CGPA (", sprintf("%.2f", cgpa_median), ") have a higher placement rate in this cohort.")
      },
      paste0(as.character(top_department$Department), " has the highest department placement rate at ", round(top_department$Rate, 1), "%."),
      paste0("The average salary among placed students is ₹", sprintf("%.2f", mean(students$Salary, na.rm = TRUE)), " LPA.")
    )
    tags$ul(class = "insight-list", lapply(bullets, tags$li))
  })
}

shinyApp(ui = ui, server = server)
