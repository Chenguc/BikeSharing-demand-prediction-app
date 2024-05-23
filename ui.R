# Load required libraries
library(shiny)
library(leaflet)

# Create a RShiny UI
shinyUI(
  fluidPage(
    padding = 5,
    titlePanel("Bike-sharing Demand Prediction App"),
    # Create a side-bar layout
    sidebarLayout(
      # Create a main panel to show cities on a leaflet map
      mainPanel(
        leafletOutput("city_bike_map", width = "100%", height = "800px")
      ),
      # Create a side bar to show detailed plots for a city
      sidebarPanel(
        # select drop down list to select city
        selectInput("selected_city", "Select a City:",
                    choices = NULL),
        # Add plot output for temperature trend line
        plotOutput("temp_line", height = "300px", width = "100%"),
        
        # Add plot output for bike-sharing demand prediction trend line with click event
        plotOutput("bike_line", height = "300px", width = "100%", click = "plot_click"),
        # Add text output to show clicked point information
        verbatimTextOutput("bike_date_output"),
        
        # Add plot output for humidity and bike-sharing demand prediction correlation
        plotOutput("humidity_pred_chart", height = "300px", width = "100%")
      )
    )
  )
)

