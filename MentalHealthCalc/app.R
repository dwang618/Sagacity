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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mental Health Calc"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("state", "Pick a state!", choices = unique(data$state), selected = "NY")
      ),
      mainPanel(
        textOutput("amntOfPpl")
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$state)
    filter(data, treatment == "Yes" & state == input$state)
  })
  output$amntOfPpl <- renderText({
    filtered <- filtered_data()
    amnt <- 0
    amnt <- nrow(filtered)
    paste("Total number of people who sought treatment in", input$state, ":", amnt)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
