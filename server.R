# Install and import required libraries
library(shiny)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(httr)
library(scales)

# Import model_prediction.R which contains methods to call OpenWeather API and make predictions
source("model_prediction.R")

# Test function for generating weather data
test_weather_data_generation <- function() {
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df) > 0)
  print(city_weather_bike_df)
  return(city_weather_bike_df)
}

# Create a Shiny server
shinyServer(function(input, output, session) {
  # Define a city list
  city_list <- c("Seoul", "New York", "Paris", "London", "Suzhou")
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  
  # Update the city selection input with available cities
  updateSelectInput(session, "selected_city", choices = c("All", city_list), selected = "All")
  
  # Create another data frame called `cities_max_bike` with each row containing city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    summarise(
      MAX_BIKE_PREDICTION = max(BIKE_PREDICTION_LEVEL),
      LNG = first(LNG),
      LAT = first(LAT),
      LABEL = first(LABEL)
    )
  
  # Observe drop-down event
  observeEvent(input$selected_city, {
    selected_city <- input$selected_city
    if (selected_city == "All") {
      # Render a leaflet map with circle markers for all cities
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~LNG, 
            lat = ~LAT, 
            radius = ~ifelse(MAX_BIKE_PREDICTION == "small", 6,
                             ifelse(MAX_BIKE_PREDICTION == "medium", 10, 12)),
            fillColor = ~color_levels(MAX_BIKE_PREDICTION),
            fillOpacity = 0.7,
            popup = ~LABEL
          )
      })
    } else {
      # Render a leaflet map with one marker for the selected city
      city_data <- filter(city_weather_bike_df, CITY_ASCII == selected_city)
      output$city_bike_map <- renderLeaflet({
        leaflet(city_data) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~LNG, 
            lat = ~LAT, 
            radius = 10,
            fillColor = ~color_levels(BIKE_PREDICTION_LEVEL),
            fillOpacity = 0.7,
            popup = ~DETAILED_LABEL
          )
      })
      
      # Plot temperature trend line for the selected city
      output$temp_line <- renderPlot({
        ggplot(city_data, aes(x = as.POSIXct(FORECASTDATETIME), y = TEMPERATURE)) +
          geom_line(color = "blue") +
          geom_point(color = "red") +
          geom_text(aes(label = TEMPERATURE), vjust = -1, size = 3) +
          labs(title = paste("Temperature Trend for", selected_city),
               x = "Datetime", y = "Temperature (°C)") +
          theme_minimal()
      })
      
      # Plot bike-sharing demand prediction trend line for the selected city
      output$bike_line <- renderPlot({
        ggplot(city_data, aes(x = as.POSIXct(FORECASTDATETIME), y = BIKE_PREDICTION)) +
          geom_line(color = "blue") +
          geom_point(color = "red") +
          geom_text(aes(label = BIKE_PREDICTION), vjust = -1, size = 3) +
          labs(title = paste("Bike-sharing Demand Prediction for", selected_city),
               x = "Datetime", y = "Bike-sharing Prediction") +
          theme_minimal()
      })
      
      # Plot humidity and bike-sharing demand prediction correlation for the selected city
      output$humidity_pred_chart <- renderPlot({
        ggplot(city_data, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point(color = "blue") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red") +
          labs(title = paste("Humidity vs Bike-sharing Demand Prediction for", selected_city),
               x = "Humidity (%)", y = "Bike-sharing Prediction") +
          theme_minimal()
      })
    }
  })
  
  # Render text output for the clicked point on the bike-sharing demand prediction plot
  output$bike_date_output <- renderText({
    req(input$plot_click)
    clicked_point <- nearPoints(city_weather_bike_df, input$plot_click, threshold = 10, maxpoints = 1)
    if (nrow(clicked_point) == 0) {
      "Click on a point to see the details"
    } else {
      paste("Datetime:", clicked_point$FORECASTDATETIME, 
            "\nBike-sharing Prediction:", clicked_point$BIKE_PREDICTION)
    }
  })
  
  # Initial rendering of the map showing all cities
  output$city_bike_map <- renderLeaflet({
    leaflet(cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LNG, 
        lat = ~LAT, 
        radius = ~ifelse(MAX_BIKE_PREDICTION == "small", 6,
                         ifelse(MAX_BIKE_PREDICTION == "medium", 10, 12)),
        fillColor = ~color_levels(MAX_BIKE_PREDICTION),
        fillOpacity = 0.7,
        popup = ~LABEL
      )
  })
})



