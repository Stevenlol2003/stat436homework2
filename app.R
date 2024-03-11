library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

data = read_csv("https://uwmadison.box.com/shared/static/7ux2kexdoetugbxd5xdp8642620l0snd.csv")
data = data %>% 
  mutate(FL_DATE = as.Date(FL_DATE, format = "%m/%d/%Y"))

ui = fluidPage(
  titlePanel("Flight Delays and Cancellations"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Date Range", start = min(data$FL_DATE), end = max(data$FL_DATE)),
      HTML("<p style='color: gray; font-size: 10px; margin-top: 5px; margin-bottom: 1px;'>Select Year overrides Date Range</p>"),
      selectInput("yearFilter", "Select Year", choices = c("No selection", 2019, 2020, 2021, 2022, 2023)),
      selectInput("airportFilter", "Select Departure Airport(s)", choices = sort(unique(data$ORIGIN)), multiple = TRUE, selected = "ATL"),
      selectInput("cityFilter", "Select Departure City(s)", choices = sort(unique(data$ORIGIN_CITY)), multiple = TRUE),
      checkboxInput("showDelayed", "Show Delayed Flights", value = TRUE),
      checkboxInput("showCancelled", "Show Cancelled Flights", value = TRUE),
      HTML("<p style='color: gray; font-size: 10px; margin-top: 5px; margin-bottom: 1px;'>Select Destination (Optional)</p>"),
      selectInput("destAirportFilter", "Select Destination Airport(s)", choices = sort(unique(data$DEST)), multiple = TRUE),
      selectInput("destCityFilter", "Select Destination City(s)", choices = sort(unique(data$DEST_CITY)), multiple = TRUE)
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.showDelayed == true",
        plotOutput("delayedPlot")
      ),
      conditionalPanel(
        condition = "input.showCancelled == true",
        plotOutput("cancelledPlot")
      ),
      textOutput("highestAirlineText")
    )
  )
)

server = function(input, output) {
  filtered_data = reactive({
    if (input$yearFilter == "No selection") {
      # If "No selection" is chosen, filter by date range only
      data %>%
        filter(
          FL_DATE >= input$dateRange[1] & FL_DATE <= input$dateRange[2],
          (ORIGIN %in% input$airportFilter | ORIGIN_CITY %in% input$cityFilter) |
            (DEST %in% input$destAirportFilter | DEST_CITY %in% input$destCityFilter)
        )
    } else {
      # If a specific year is chosen, filter by year only
      data %>%
        filter(
          year(FL_DATE) == input$yearFilter,
          FL_DATE >= input$dateRange[1] & FL_DATE <= input$dateRange[2],
          (ORIGIN %in% input$airportFilter | ORIGIN_CITY %in% input$cityFilter) |
            (DEST %in% input$destAirportFilter | DEST_CITY %in% input$destCityFilter)
        )
    }
  })
  
  output$highestAirlineText = renderText({
    delayed_data = filtered_data() %>%
      group_by(AIRLINE) %>%
      summarise(avg_delayed = mean(DEP_DELAY > 0, na.rm = TRUE))
    
    cancelled_data = filtered_data() %>%
      group_by(AIRLINE) %>%
      summarise(avg_cancelled = mean(CANCELLED == 1, na.rm = TRUE))
    
    highest_airline_delayed = delayed_data %>%
      filter(avg_delayed == max(avg_delayed, na.rm = TRUE)) %>%
      slice(1)
    
    lowest_airline_delayed = delayed_data %>%
      filter(avg_delayed == min(avg_delayed, na.rm = TRUE)) %>%
      slice(1)
    
    highest_airline_cancelled = cancelled_data %>%
      filter(avg_cancelled == max(avg_cancelled, na.rm = TRUE)) %>%
      slice(1)
    
    lowest_airline_cancelled = cancelled_data %>%
      filter(avg_cancelled == min(avg_cancelled, na.rm = TRUE)) %>%
      slice(1)    
    
    if (input$showDelayed) {
      delayed_text1 = paste("Most Delayed Airline:", highest_airline_delayed$AIRLINE, "with average delay chance of", round(highest_airline_delayed$avg_delayed, 2) * 100, "%.")
      delayed_text2 = paste("Least Delayed Airline:", lowest_airline_delayed$AIRLINE, "with average delay chance of", round(lowest_airline_delayed$avg_delayed, 2) * 100, "%.")
    } else {
      delayed_text1 = NULL
      delayed_text2 = NULL
    }
    
    if (input$showCancelled) {
      cancelled_text1 = paste("Most Cancelled Airline:", highest_airline_cancelled$AIRLINE, "with average cancellation chance of", round(highest_airline_cancelled$avg_cancelled, 2) * 100, "%.")
      cancelled_text2 = paste("Least Cancelled Airline:", lowest_airline_cancelled$AIRLINE, "with average cancellation chance of", round(lowest_airline_cancelled$avg_cancelled, 2) * 100, "%.")
    } else {
      cancelled_text1 = NULL
      cancelled_text2 = NULL
    }
    
    c(delayed_text1, delayed_text2, cancelled_text1, cancelled_text2)
  })
  
  output$delayedPlot = renderPlot({
    delay_summary = filtered_data() %>%
      group_by(FL_DATE) %>%
      summarise(
        percent_delayed = if (input$showDelayed) sum(DEP_DELAY > 0) / n() * 100 else 0
      )
    
    ggplot(delay_summary, aes(x = FL_DATE, y = percent_delayed)) +
      geom_bar(fill = "blue", stat = "identity", alpha = 0.7) +
      labs(title = "Percent of Delayed Flights Over Time",
           x = "Date",
           y = "Percentage") +
      theme_minimal()
  })
  
  output$cancelledPlot = renderPlot({
    cancelled_summary = filtered_data() %>%
      group_by(FL_DATE) %>%
      summarise(
        percent_cancelled = if (input$showCancelled) sum(CANCELLED == 1) / n() * 100 else 0
      )
    
    ggplot(cancelled_summary, aes(x = FL_DATE, y = percent_cancelled)) +
      geom_bar(fill = "red", stat = "identity", alpha = 0.7) +
      labs(title = "Percent of Cancelled Flights Over Time",
           x = "Date",
           y = "Percentage") +
      theme_minimal()
  })
}

shinyApp(ui, server)
