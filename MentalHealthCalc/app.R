library(shiny)
library(dplyr)
library(ggplot2)

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
      plotOutput("barPlot")
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
}

# Run the application
shinyApp(ui = ui, server = server)
