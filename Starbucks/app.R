#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)

# Load the Starbucks data
starbucks_data <- read.csv("starbucksMA.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Starbucks Locations in MA - Gross Profit"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("profitRange",
                  "Gross Profit Range:",
                  min = min(starbucks_data$gross_profit, na.rm = TRUE),
                  max = max(starbucks_data$gross_profit, na.rm = TRUE),
                  value = c(min(starbucks_data$gross_profit, na.rm = TRUE), 
                            max(starbucks_data$gross_profit, na.rm = TRUE)),
                  step = 1000)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define the server
server <- function(input, output, session) {
  filtered_data <- reactive({
    starbucks_data %>%
      filter(gross_profit >= input$profitRange[1],
             gross_profit <= input$profitRange[2])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(starbucks_data$Longitude, na.rm = TRUE), 
              lat = mean(starbucks_data$Latitude, na.rm = TRUE), 
              zoom = 10)
  })
  
  observe({
    data <- filtered_data()
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,
                       popup = ~paste0("<b>Name:</b> ", Name, "<br>",
                                       "<b>Gross Profit:</b> $", gross_profit),
                       radius = 5,
                       color = "blue",
                       fillOpacity = 0.7)
  })
}

# Run the app
shinyApp(ui, server)
