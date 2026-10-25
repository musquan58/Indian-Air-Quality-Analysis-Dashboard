# Global/Indian Air Quality Analysis Dashboard
# Data source: Air Quality Data in India (2015-2020), city_day.csv
# Keep this R file in the same folder as city_day.csv.

required_packages <- c("shiny", "shinydashboard", "ggplot2", "dplyr", "tidyr", "leaflet", "maps")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(leaflet)
library(maps)

# Load the real dataset from the same folder as this script.
if (!file.exists("city_day.csv")) {
  stop(
    "Cannot find city_day.csv. Put city_day.csv in the same folder as this R file, ",
    "then set that folder as RStudio's working directory."
  )
}

air_data <- read.csv("city_day.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Validate and prepare required columns.
if (!all(c("City", "Date", "AQI") %in% names(air_data))) {
  stop("The CSV must contain columns named City, Date, and AQI.")
}

air_data$City <- as.character(air_data$City)
air_data$Date <- as.Date(air_data$Date, format = "%Y-%m-%d")

numeric_columns <- intersect(
  c("AQI", "PM2.5", "PM10", "NO", "NO2", "NOx", "NH3",
    "CO", "SO2", "O3", "Benzene", "Toluene", "Xylene"),
  names(air_data)
)
for (column_name in numeric_columns) {
  air_data[[column_name]] <- suppressWarnings(
    as.numeric(as.character(air_data[[column_name]]))
  )
}

air_data <- air_data %>%
  filter(!is.na(City), City != "", !is.na(Date)) %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),
    MonthNumber = as.integer(format(Date, "%m")),
    Month = factor(
      format(Date, "%b"),
      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
      ordered = TRUE
    )
  )

available_years <- sort(unique(air_data$Year))
available_cities <- sort(unique(air_data$City))

# Reusable empty-chart message.
empty_plot <- function(message_text) {
  ggplot() +
    annotate("text", x = 1, y = 1, label = message_text, size = 5) +
    xlim(0, 2) + ylim(0, 2) +
    theme_void()
}

ui <- dashboardPage(
  dashboardHeader(title = "Air Quality Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Pollution Hotspots", tabName = "hotspots",
               icon = icon("exclamation-triangle")),
      menuItem("Extreme Pollution Events", tabName = "extreme",
               icon = icon("fire")),
      menuItem("AQI Severity Classification", tabName = "severity",
               icon = icon("tachometer-alt")),
      # FEATURE 16
      menuItem("Interactive Pollution Map",
               tabName = "pollution_map",
               icon = icon("globe")),
      menuItem(
        "City Pollution Profile",
        tabName = "city_profile",
        icon = icon("city")
      ),
      menuItem("Data & Insights", tabName = "data", icon = icon("table"))
    ),
    selectInput(
      "year", "Select year:",
      choices = available_years,
      selected = max(available_years)
    ),
    numericInput(
      "aqi_threshold",
      "High AQI threshold:",
      value = 200,
      min = 1,
      max = 2000,
      step = 10
    ),
    selectizeInput(
      "cities", "Select cities:",
      choices = available_cities,
      selected = available_cities,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    helpText("Dataset: Air Quality Data in India, 2015–2020.")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f4f6f9; }
      .small-box { border-radius: 8px; }
      .box { border-radius: 8px; }
    "))),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("records_box", width = 3),
          valueBoxOutput("average_aqi_box", width = 3),
          valueBoxOutput("highest_aqi_box", width = 3),
          valueBoxOutput("cities_box", width = 3)
        ),
        fluidRow(
          box(title = "Monthly Average AQI Trend", status = "primary",
              solidHeader = TRUE, width = 8, plotOutput("trend_plot", height = 320)),
          box(title = "Average AQI by City", status = "primary",
              solidHeader = TRUE, width = 4, plotOutput("city_plot", height = 320))
        ),
        fluidRow(
          box(title = "Monthly AQI Heatmap", status = "info",
              solidHeader = TRUE, width = 7, plotOutput("heatmap_plot", height = 330)),
          box(title = "Pollutant Levels", status = "info",
              solidHeader = TRUE, width = 5, plotOutput("pollutant_plot", height = 330))
        ),
        fluidRow(
          box(
            title = "How to interpret this dashboard",
            status = "success", solidHeader = TRUE, width = 12,
            p("AQI is the Air Quality Index reported in the source dataset. Higher AQI values generally indicate poorer air quality."),
            p("Charts show averages calculated from available recorded values for the selected year and cities. Missing AQI or pollutant measurements are excluded from the relevant averages, not replaced with zero."),
            p("This dataset covers Indian cities and the years 2015–2020; it is not live/current air-quality monitoring data.")
          )
        )
      ),
      tabItem(
        tabName = "hotspots",
        
        fluidRow(
          valueBoxOutput("hotspot_city_box", width = 4),
          valueBoxOutput("hotspot_month_box", width = 4),
          valueBoxOutput("high_days_box", width = 4)
        ),
        
        fluidRow(
          box(
            title = "High-AQI Days by City",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            plotOutput("hotspot_plot", height = 400)
          )
        ),
        
        fluidRow(
          box(
            title = "Extreme Pollution Records",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tableOutput("extreme_table")
          )
        )
      ),
      tabItem(
        tabName = "extreme",
        
        fluidRow(
          valueBoxOutput("extreme_event_count", width = 4),
          valueBoxOutput("extreme_event_city", width = 4),
          valueBoxOutput("extreme_event_date", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Extreme AQI Events Over Time",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            plotOutput("extreme_event_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Extreme Pollution Event Details",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tableOutput("extreme_event_table")
          )
        )
      ),
      tabItem(
        tabName = "severity",
        
        fluidRow(
          valueBoxOutput("severity_total", width = 4),
          valueBoxOutput("severity_dominant", width = 4),
          valueBoxOutput("severity_high", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Distribution of AQI Severity Categories",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotOutput("severity_distribution", height = 350)
          ),
          
          box(
            title = "AQI Severity by City",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("severity_city_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "Monthly Pollution Severity",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("severity_month_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title = "AQI Severity Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            tableOutput("severity_summary")
          )
        )
      ),
      # ==========================================
      # FEATURE 16: INTERACTIVE POLLUTION MAP
      # ==========================================
      tabItem(
        tabName = "pollution_map",
        
        fluidRow(
          box(
            title = "Interactive Air Pollution Map of India",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            p("Click a city marker to view its average recorded AQI."),
            
            leafletOutput("pollution_map_output", height = 600)
          )
        )
      ),
      # FEATURE 8: CITY POLLUTION PROFILE
      
      tabItem(
        tabName = "city_profile",
        
        h2("City Pollution Profile Generator"),
        
        fluidRow(
          box(
            title = "Select City",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "profile_city",
              "Choose a city:",
              choices = sort(unique(air_data$City)),
              selected = sort(unique(air_data$City))[1]
            )
          )
        ),
        
        fluidRow(
          valueBoxOutput("profile_average_aqi", width = 6),
          valueBoxOutput("profile_highest_aqi", width = 6)
        ),
        
        fluidRow(
          box(
            title = "Monthly AQI Trend",
            width = 12,
            status = "info",
            plotOutput("profile_monthly_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Pollution Data Summary",
            width = 12,
            status = "success",
            tableOutput("profile_summary_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Download City Report",
            width = 12,
            downloadButton(
              "download_city_profile",
              "Download City Report (CSV)"
            )
          )
        )
      ),
      
      tabItem(
        tabName = "data",
        fluidRow(
          box(title = "Filtered Daily Records", status = "primary",
              solidHeader = TRUE, width = 12,
              tableOutput("data_table"),
              br(),
              downloadButton("download_data", "Download filtered CSV"))
        ),
        fluidRow(
          box(title = "Summary by City", status = "info",
              solidHeader = TRUE, width = 12,
              tableOutput("summary_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$year, input$cities)
    air_data %>%
      filter(Year == as.integer(input$year), City %in% input$cities)
  })
  # Data used for hotspot analysis
  hotspot_data <- reactive({
    req(input$aqi_threshold)
    filtered_data() %>%
      filter(!is.na(AQI))
  })
  
  # City with the highest average AQI
  output$hotspot_city_box <- renderValueBox({
    d <- hotspot_data()
    
    if (nrow(d) == 0) {
      city_name <- "No data"
    } else {
      city_summary <- d %>%
        group_by(City) %>%
        summarise(Average_AQI = mean(AQI), .groups = "drop") %>%
        arrange(desc(Average_AQI))
      
      city_name <- city_summary$City[1]
    }
    
    valueBox(
      city_name,
      "Highest average AQI",
      icon = icon("city"),
      color = "red"
    )
  })
  
  # Month with the highest average AQI
  output$hotspot_month_box <- renderValueBox({
    d <- hotspot_data()
    
    if (nrow(d) == 0) {
      month_name <- "No data"
    } else {
      month_summary <- d %>%
        group_by(Month, MonthNumber) %>%
        summarise(Average_AQI = mean(AQI), .groups = "drop") %>%
        arrange(desc(Average_AQI))
      
      month_name <- as.character(month_summary$Month[1])
    }
    
    valueBox(
      month_name,
      "Highest average AQI month",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # Number of city-days above the selected threshold
  output$high_days_box <- renderValueBox({
    d <- hotspot_data()
    
    high_days <- d %>%
      filter(AQI > input$aqi_threshold) %>%
      distinct(City, Date) %>%
      nrow()
    
    valueBox(
      high_days,
      "City-days above threshold",
      icon = icon("exclamation-triangle"),
      color = "purple"
    )
  })
  
  # Chart of high-AQI days by city
  output$hotspot_plot <- renderPlot({
    d <- hotspot_data() %>%
      filter(AQI > input$aqi_threshold) %>%
      distinct(City, Date) %>%
      count(City, name = "High_AQI_Days") %>%
      arrange(High_AQI_Days)
    
    if (nrow(d) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 1, y = 1,
            label = "No observations exceed this threshold."
          ) +
          theme_void()
      )
    }
    
    ggplot(
      d,
      aes(x = reorder(City, High_AQI_Days), y = High_AQI_Days)
    ) +
      geom_col(fill = "firebrick") +
      coord_flip() +
      labs(
        title = paste("Recorded AQI above", input$aqi_threshold),
        x = "City",
        y = "Number of city-days"
      ) +
      theme_minimal()
  })
  
  # Twenty highest recorded AQI observations
  output$extreme_table <- renderTable({
    hotspot_data() %>%
      arrange(desc(AQI)) %>%
      select(
        City,
        Date,
        AQI,
        any_of(c("AQI_Bucket", "PM2.5", "PM10"))
      ) %>%
      mutate(
        Date = format(as.Date(Date, origin = "1970-01-01"), "%Y-%m-%d")
      ) %>%
      head(20)
  }, striped = TRUE, bordered = TRUE)
  # Extreme Pollution Event Detection
  # Events are defined here as recorded AQI >= 300.
  
  extreme_events <- reactive({
    hotspot_data() %>%
      filter(AQI >= 300) %>%
      arrange(desc(AQI))
  })
  
  # Total extreme AQI records
  output$extreme_event_count <- renderValueBox({
    valueBox(
      nrow(extreme_events()),
      "Recorded events with AQI >= 300",
      icon = icon("fire"),
      color = "red"
    )
  })
  
  # City with the most extreme AQI records
  output$extreme_event_city <- renderValueBox({
    d <- extreme_events()
    
    if (nrow(d) == 0) {
      city_name <- "No events"
    } else {
      city_summary <- d %>%
        count(City, name = "Event_Count") %>%
        arrange(desc(Event_Count))
      
      city_name <- city_summary$City[1]
    }
    
    valueBox(
      city_name,
      "Most extreme-event records",
      icon = icon("city"),
      color = "yellow"
    )
  })
  
  # Date of the highest recorded AQI
  output$extreme_event_date <- renderValueBox({
    d <- extreme_events()
    
    if (nrow(d) == 0) {
      date_text <- "No events"
    } else {
      date_text <- format(as.Date(d$Date[1]), "%d %b %Y")
    }
    
    valueBox(
      date_text,
      "Date of highest recorded AQI",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  # Plot extreme AQI records over time
  output$extreme_event_plot <- renderPlot({
    d <- extreme_events()
    
    if (nrow(d) == 0) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 1,
            y = 1,
            label = "No records meet the selected event threshold."
          ) +
          theme_void()
      )
    }
    
    d %>%
      mutate(Event_Date = as.Date(Date)) %>%
      group_by(Event_Date) %>%
      summarise(
        Highest_AQI = max(AQI, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = Event_Date, y = Highest_AQI)) +
      geom_line(linewidth = 0.8, color = "red") +
      geom_point(size = 2, color = "darkred") +
      labs(
        title = "Recorded AQI values of extreme events",
        x = "Date",
        y = "Highest recorded AQI"
      ) +
      theme_minimal()
  })
  
  # Table of extreme events
  output$extreme_event_table <- renderTable({
    extreme_events() %>%
      mutate(Date = format(as.Date(Date), "%Y-%m-%d")) %>%
      select(
        City,
        Date,
        AQI,
        any_of(c("AQI_Bucket", "PM2.5", "PM10"))
      ) %>%
      head(100)
  }, striped = TRUE, bordered = TRUE)

  output$records_box <- renderValueBox({
    valueBox(
      value = format(nrow(filtered_data()), big.mark = ","),
      subtitle = "Daily records in selection",
      icon = icon("database"),
      color = "aqua"
    )
  })

  output$average_aqi_box <- renderValueBox({
    d <- filtered_data()
    avg <- if (all(is.na(d$AQI))) NA_real_ else mean(d$AQI, na.rm = TRUE)
    valueBox(
      value = ifelse(is.na(avg), "N/A", round(avg, 1)),
      subtitle = "Average recorded AQI",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$highest_aqi_box <- renderValueBox({
    d <- filtered_data()
    highest <- if (all(is.na(d$AQI))) NA_real_ else max(d$AQI, na.rm = TRUE)
    valueBox(
      value = ifelse(is.na(highest), "N/A", round(highest, 1)),
      subtitle = "Highest recorded AQI",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$cities_box <- renderValueBox({
    d <- filtered_data()
    valueBox(
      value = dplyr::n_distinct(d$City),
      subtitle = "Cities selected",
      icon = icon("city"),
      color = "green"
    )
  })

  output$trend_plot <- renderPlot({
    d <- filtered_data() %>%
      filter(!is.na(AQI)) %>%
      group_by(City, Month, MonthNumber) %>%
      summarise(MeanAQI = mean(AQI), .groups = "drop")

    if (nrow(d) == 0) return(empty_plot("No AQI values available for this selection."))

    ggplot(d, aes(x = Month, y = MeanAQI, color = City, group = City)) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      labs(x = "Month", y = "Average AQI", color = "City") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
  })

  output$city_plot <- renderPlot({
    d <- filtered_data() %>%
      filter(!is.na(AQI)) %>%
      group_by(City) %>%
      summarise(MeanAQI = mean(AQI), .groups = "drop") %>%
      arrange(MeanAQI)

    if (nrow(d) == 0) return(empty_plot("No AQI values available."))

    ggplot(d, aes(x = reorder(City, MeanAQI), y = MeanAQI)) +
      geom_col() +
      coord_flip() +
      labs(x = "City", y = "Average AQI") +
      theme_minimal(base_size = 12)
  })

  output$heatmap_plot <- renderPlot({
    d <- filtered_data() %>%
      filter(!is.na(AQI)) %>%
      group_by(City, Month, MonthNumber) %>%
      summarise(MeanAQI = mean(AQI), .groups = "drop")

    if (nrow(d) == 0) return(empty_plot("No AQI values available."))

    ggplot(d, aes(x = Month, y = City, fill = MeanAQI)) +
      geom_tile(color = "white") +
      labs(x = "Month", y = "City", fill = "Average AQI") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  output$pollutant_plot <- renderPlot({
    d <- filtered_data()
    pollutants <- intersect(c("PM2.5", "PM10", "NO2", "CO", "SO2", "O3"), names(d))

    if (length(pollutants) == 0) {
      return(empty_plot("Pollutant columns are not available."))
    }

    summary_data <- lapply(pollutants, function(p) {
      values <- d[[p]]
      data.frame(
        Pollutant = p,
        MeanLevel = if (all(is.na(values))) {
          NA_real_
        } else {
          mean(values, na.rm = TRUE)
        }
      )
    }) %>%
      bind_rows() %>%
      filter(!is.na(MeanLevel))

    if (nrow(summary_data) == 0) {
      return(empty_plot("No pollutant measurements available for this selection."))
    }

    ggplot(summary_data, aes(x = reorder(Pollutant, MeanLevel), y = MeanLevel)) +
      geom_col() +
      coord_flip() +
      labs(x = "Pollutant", y = "Mean concentration (source units)") +
      theme_minimal(base_size = 12)
  })

  output$data_table <- renderTable({
    filtered_data() %>%
      select(City, Date, any_of(c("AQI", "AQI_Bucket", "PM2.5", "PM10", "NO2", "CO", "SO2", "O3"))) %>%
      arrange(Date, City) %>%
      head(100)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$summary_table <- renderTable({
    filtered_data() %>%
      group_by(City) %>%
      summarise(
        Daily_records = n(),
        Days_with_AQI = sum(!is.na(AQI)),
        Average_AQI = if (all(is.na(AQI))) {
          NA_real_
        } else {
          round(mean(AQI, na.rm = TRUE), 2)
        },
        Maximum_AQI = if (all(is.na(AQI))) {
          NA_real_
        } else {
          max(AQI, na.rm = TRUE)
        },
        .groups = "drop"
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  # ==========================================
  # FEATURE 15: AQI SEVERITY CLASSIFICATION
  # ==========================================
  
  severity_data <- reactive({
    
    d <- hotspot_data()
    
    d %>%
      filter(!is.na(AQI)) %>%
      mutate(
        Severity = case_when(
          AQI < 0 ~ "Invalid AQI",
          AQI <= 50 ~ "Good",
          AQI <= 100 ~ "Satisfactory",
          AQI <= 200 ~ "Moderate",
          AQI <= 300 ~ "Poor",
          AQI <= 400 ~ "Very Poor",
          AQI <= 500 ~ "Severe",
          TRUE ~ "Above 500 / Verify"
        ),
        Severity = factor(
          Severity,
          levels = c(
            "Good",
            "Satisfactory",
            "Moderate",
            "Poor",
            "Very Poor",
            "Severe",
            "Above 500 / Verify",
            "Invalid AQI"
          )
        ),
        Month = factor(
          format(as.Date(Date), "%b"),
          levels = month.abb
        )
      )
  })
  
  # Summary cards: total valid AQI records
  
  output$severity_total <- renderValueBox({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500)
    
    valueBox(
      format(nrow(d), big.mark = ","),
      "AQI records within 0–500",
      icon = icon("database"),
      color = "aqua"
    )
  })
  
  # Most common AQI category
  
  output$severity_dominant <- renderValueBox({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500) %>%
      count(Severity, name = "Records") %>%
      arrange(desc(Records))
    
    category <- if (nrow(d) == 0) {
      "No data"
    } else {
      as.character(d$Severity[1])
    }
    
    valueBox(
      category,
      "Most common AQI category",
      icon = icon("chart-pie"),
      color = "blue"
    )
  })
  
  # Records above AQI 200
  
  output$severity_high <- renderValueBox({
    
    d <- severity_data()
    
    high_count <- sum(d$AQI > 200 & d$AQI <= 500, na.rm = TRUE)
    
    valueBox(
      format(high_count, big.mark = ","),
      "Records with AQI above 200",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Distribution of AQI categories
  
  output$severity_distribution <- renderPlot({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500) %>%
      count(Severity, name = "Records")
    
    validate(
      need(nrow(d) > 0, "No valid AQI records for this selection.")
    )
    
    ggplot(d, aes(x = Severity, y = Records, fill = Severity)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = Records),
        vjust = -0.3,
        size = 3.5
      ) +
      labs(
        x = "AQI severity category",
        y = "Number of records"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 35,
          hjust = 1
        )
      )
  })
  
  # Severity comparison across cities
  
  output$severity_city_plot <- renderPlot({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500) %>%
      count(City, Severity, name = "Records")
    
    validate(
      need(nrow(d) > 0, "No valid AQI records for this selection.")
    )
    
    ggplot(
      d,
      aes(x = City, y = Records, fill = Severity)
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = "City",
        y = "Number of records",
        fill = "AQI category"
      ) +
      theme_minimal()
  })
  
  # Monthly severity pattern
  
  output$severity_month_plot <- renderPlot({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500) %>%
      count(Month, Severity, name = "Records")
    
    validate(
      need(nrow(d) > 0, "No valid AQI records for this selection.")
    )
    
    ggplot(
      d,
      aes(x = Month, y = Records, fill = Severity)
    ) +
      geom_col() +
      labs(
        x = "Month",
        y = "Number of records",
        fill = "AQI category"
      ) +
      theme_minimal()
  })
  
  # Summary table
  
  output$severity_summary <- renderTable({
    
    d <- severity_data() %>%
      filter(AQI >= 0, AQI <= 500)
    
    d %>%
      count(Severity, name = "Records") %>%
      mutate(
        Percentage = round(
          100 * Records / sum(Records),
          2
        )
      ) %>%
      arrange(Severity)
  }, striped = TRUE, bordered = TRUE)

  # ==========================================
  # FEATURE 8: CITY POLLUTION PROFILE GENERATOR
  # ==========================================
  city_profile_data <- reactive({
    req(input$profile_city, input$year)
    air_data %>%
      filter(City == input$profile_city, Year == as.integer(input$year))
  })

  output$profile_average_aqi <- renderValueBox({
    d <- city_profile_data()
    vals <- d$AQI[!is.na(d$AQI)]
    avg <- if (length(vals) == 0) NA_real_ else mean(vals)
    valueBox(
      if (is.na(avg)) "N/A" else round(avg, 1),
      paste("Average recorded AQI —", input$profile_city),
      icon = icon("chart-line"),
      color = "aqua"
    )
  })

  output$profile_highest_aqi <- renderValueBox({
    d <- city_profile_data()
    vals <- d$AQI[!is.na(d$AQI)]
    highest <- if (length(vals) == 0) NA_real_ else max(vals)
    valueBox(
      if (is.na(highest)) "N/A" else round(highest, 1),
      "Highest recorded AQI",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$profile_monthly_plot <- renderPlot({
    d <- city_profile_data() %>%
      filter(!is.na(AQI)) %>%
      group_by(Month, MonthNumber) %>%
      summarise(Average_AQI = mean(AQI), .groups = "drop")

    if (nrow(d) == 0) return(empty_plot("No AQI data for this city and year."))

    ggplot(d, aes(x = Month, y = Average_AQI, group = 1)) +
      geom_line(color = "#e74c3c", linewidth = 1) +
      geom_point(color = "#2c3e50", size = 2) +
      labs(
        title = paste(input$profile_city, "— monthly average AQI in", input$year),
        x = "Month", y = "Average recorded AQI"
      ) +
      theme_minimal(base_size = 12)
  })

  output$profile_summary_table <- renderTable({
    d <- city_profile_data()
    if (nrow(d) == 0) return(data.frame(Message = "No records for this city and year."))

    mean_or_na <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_real_ else round(mean(x), 2)
    }
    max_or_na <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_real_ else max(x)
    }

    data.frame(
      City = input$profile_city,
      Year = as.integer(input$year),
      Daily_Records = nrow(d),
      Records_With_AQI = sum(!is.na(d$AQI)),
      Missing_AQI_Records = sum(is.na(d$AQI)),
      Average_AQI = mean_or_na(d$AQI),
      Highest_AQI = max_or_na(d$AQI),
      Average_PM25 = if ("PM2.5" %in% names(d)) mean_or_na(d[["PM2.5"]]) else NA_real_,
      Average_PM10 = if ("PM10" %in% names(d)) mean_or_na(d[["PM10"]]) else NA_real_
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_city_profile <- downloadHandler(
    filename = function() {
      paste0("city_pollution_profile_", gsub("[^A-Za-z0-9]", "_", input$profile_city), "_", input$year, ".csv")
    },
    content = function(file) {
      write.csv(city_profile_data(), file, row.names = FALSE, na = "")
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("air_quality_filtered_", input$year, ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE, na = "")
    }
  )
  # ==========================================
  # FEATURE 16: INTERACTIVE POLLUTION MAP
  # ==========================================
  
  # Approximate city-centre coordinates
  city_coordinates <- data.frame(
    City = c(
      "Ahmedabad", "Amaravati", "Amritsar", "Bengaluru",
      "Bhopal", "Chandigarh", "Chennai", "Coimbatore",
      "Delhi", "Gurugram", "Guwahati", "Hyderabad",
      "Jaipur", "Kochi", "Kolkata", "Lucknow",
      "Mumbai", "Patna", "Shillong", "Thiruvananthapuram",
      "Visakhapatnam", "Aizawl", "Brajrajnagar",
      "Ernakulam", "Jorapokhar", "Talcher"
    ),
    
    Longitude = c(
      72.57, 80.52, 74.87, 77.59,
      77.41, 76.78, 80.27, 76.96,
      77.21, 77.03, 91.74, 78.49,
      75.79, 76.27, 88.36, 80.95,
      72.88, 85.14, 92.72, 76.94,
      83.30, 92.72, 83.83, 76.30,
      86.42, 85.22
    ),
    
    Latitude = c(
      23.02, 16.51, 31.63, 12.97,
      23.26, 30.73, 13.08, 11.02,
      28.61, 28.46, 26.14, 17.39,
      26.91, 9.93, 22.57, 26.85,
      19.08, 25.59, 25.57, 8.52,
      17.69, 23.73, 21.82, 9.98,
      23.79, 20.95
    )
  )
  
  output$pollution_map_output <- renderLeaflet({
    
    # Use the existing year and city filters
    d <- filtered_data() %>%
      filter(!is.na(AQI), AQI >= 0) %>%
      group_by(City) %>%
      summarise(
        Average_AQI = mean(AQI, na.rm = TRUE),
        Records = n(),
        .groups = "drop"
      ) %>%
      inner_join(city_coordinates, by = "City") %>%
      mutate(
        Severity = case_when(
          Average_AQI <= 50  ~ "Good",
          Average_AQI <= 100 ~ "Satisfactory",
          Average_AQI <= 200 ~ "Moderate",
          Average_AQI <= 300 ~ "Poor",
          Average_AQI <= 400 ~ "Very Poor",
          Average_AQI <= 500 ~ "Severe",
          TRUE ~ "Above 500"
        ),
        
        Marker_Colour = case_when(
          Average_AQI <= 50  ~ "#00A65A",
          Average_AQI <= 100 ~ "#8BC34A",
          Average_AQI <= 200 ~ "#F1C40F",
          Average_AQI <= 300 ~ "#FF851B",
          Average_AQI <= 400 ~ "#DD4B39",
          Average_AQI <= 500 ~ "#85144B",
          TRUE ~ "#6C3483"
        )
      )
    
    # Create the base map
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      setView(
        lng = 78.96,
        lat = 20.59,
        zoom = 4
      )
    
    # Display a message if no matching city coordinates exist
    if (nrow(d) == 0) {
      return(
        map %>%
          addControl(
            "No mapped city records are available for these filters.",
            position = "topright"
          )
      )
    }
    
    # Add clickable, colour-coded city markers
    map %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        data = d,
        radius = 8,
        color = ~Marker_Colour,
        fillColor = ~Marker_Colour,
        fillOpacity = 0.85,
        stroke = TRUE,
        weight = 1,
        
        popup = ~paste0(
          "<b>City:</b> ", City,
          "<br><b>Year:</b> ", input$year,
          "<br><b>Average AQI:</b> ",
          round(Average_AQI, 1),
          "<br><b>Severity:</b> ", Severity,
          "<br><b>Recorded observations:</b> ", Records
        ),
        
        label = ~paste0(
          City, " — AQI ", round(Average_AQI, 1)
        )
      ) %>%
      
      addLegend(
        position = "bottomright",
        colors = c(
          "#00A65A", "#8BC34A", "#F1C40F",
          "#FF851B", "#DD4B39", "#85144B",
          "#6C3483"
        ),
        labels = c(
          "Good",
          "Satisfactory",
          "Moderate",
          "Poor",
          "Very Poor",
          "Severe",
          "Above 500"
        ),
        title = "AQI Severity",
        opacity = 1
      )
  })
}


shinyApp(ui = ui, server = server)

