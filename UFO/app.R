#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Load the UFO dataset
ufo_data <- read_csv("UFO/complete_UFO.csv", 
                     col_types = cols(datetime = col_datetime(format = "%m/%d/%Y")))

# Extract year and filter out rows with missing countries
ufo_data <- ufo_data |>
  mutate(Year = as.integer(format(datetime, "%Y"))) |>
  filter(!is.na(country))  # Remove rows with missing country data

# Define the UI
ui <- fluidPage(
  titlePanel("UFO Sightings by Year"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("countries", 
                         "Select Countries:",
                         choices = unique(ufo_data$country),
                         selected = unique(ufo_data$country))  # All countries selected by default
    ),
    mainPanel(
      plotOutput("ufoPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  filtered_data <- reactive({
    ufo_data |> filter(country %in% input$countries)
  })
  
  output$ufoPlot <- renderPlot({
    data <- filtered_data() |>
      group_by(Year, country) |>
      summarise(Sightings = n(), .groups = "drop")
    
    ggplot(data, aes(x = Year, y = Sightings, color = country)) +
      geom_line(size = 1) +
      labs(title = "UFO Sightings by Year",
           x = "Year",
           y = "Number of Sightings",
           color = "Country") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the app
shinyApp(ui, server)