library(shiny)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)

# Read the CSV data
data <- read.csv("survey.csv")

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Mental Health Treatment by State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("states", "Pick states:", choices = unique(data$state), selected = "NY", multiple = TRUE)
    ),
    mainPanel(
      plotOutput("barPlot"),
      plotOutput("heatmap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on input$states
  filtered_data <- reactive({
    req(input$states)  # Ensure that input$states is available
    data %>%
      filter(treatment == "Yes" & state %in% input$states)
  })
  
  # Output the bar chart
  output$barPlot <- renderPlot({
    filtered <- filtered_data()
    if(nrow(filtered) == 0) {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for the selected states", size = 5, hjust = 0.5) + 
        theme_void()
    } else {
      state_counts <- filtered %>%
        group_by(state) %>%
        summarise(count = n())
      
      ggplot(state_counts, aes(x = state, y = count, fill = state)) +
        geom_bar(stat = "identity") +
        labs(title = "Number of People Seeking Mental Health Treatment by State",
             x = "State",
             y = "Number of People") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # Output the heatmap
  output$heatmap <- renderPlot({
    state_counts <- data %>%
      filter(treatment == "Yes") %>%
      group_by(state) %>%
      summarise(count = n())
    
    # Get US map data
    us_map <- map_data("state")
    
    # Ensure state names match between the data and map
    state_counts$region <- tolower(state.name[match(state_counts$state, state.abb)])
    
    # Merge map data with state counts
    map_data <- merge(us_map, state_counts, by = "region", all.x = TRUE)
    map_data <- map_data[order(map_data$order), ]  # Ensure the map data is in the correct order
    
    ggplot(map_data, aes(long, lat, group = group, fill = count)) +
      geom_polygon(color = "black", size = 0.3) +
      scale_fill_continuous(low = "white", high = "red", na.value = "grey50") +
      labs(title = "Heatmap of Mental Health Treatment by State",
           fill = "Number of People") +
      theme_void() +
      coord_fixed(1.3)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
