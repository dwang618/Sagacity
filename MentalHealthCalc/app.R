#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
data <- read.csv("survey.csv")

# Subset the data to include only specific columns
selected_columns <- data[, c("Age", "Gender", "Country")]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Survey Data"),
  
  # Main panel to show the data table
  mainPanel(
    tableOutput("dataTable")  # Add table output here
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dataTable <- renderTable({  # Render the data table
      selected_columns  # Render the subsetted data table  
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

