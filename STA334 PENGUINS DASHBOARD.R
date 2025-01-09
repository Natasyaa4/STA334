library(shiny)
library(palmerpenguins)
library(ggplot2)

ui <- fluidPage(
  # Add custom CSS to change the background to black and text to white
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: black;
          color: white;
        }
        h3, h4, .shiny-text-output {
          color: white;
        }
      ")
    )
  ),
  
  titlePanel("Penguins Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "variable",
        "Choose a variable:",
        choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
        selected = "bill_length_mm"
      ),
      selectInput(
        "plotType",
        "Select Plot Type:",
        choices = c("Histogram", "Bar Chart", "Scatterplot"),
        selected = "Histogram"
      )
    ),
    
    mainPanel(
      h3("Name: TASYA"),
      h4("Student ID: 2022647208"),
      
      h4("Summary Statistics:"),
      tableOutput("summaryTable"),
      
      h4("Graph:"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$summaryTable <- renderTable({
    req(input$variable)
    data <- penguins[[input$variable]]
    data <- na.omit(data)
    
    summary_stats <- data.frame(
      Statistic = c("Mean", "Standard Deviation", "Frequency"),
      Value = c(mean(data), sd(data), length(data))
    )
    summary_stats
  })
  
  output$plot <- renderPlot({
    req(input$variable)
    data <- penguins
    
    if (input$plotType == "Histogram") {
      ggplot(data, aes_string(x = input$variable)) +
        geom_histogram(binwidth = 10, fill = "maroon", color = "grey") +
        theme_minimal() +
        labs(title = paste("Histogram of", input$variable), x = input$variable, y = "Frequency")
      
    } else if (input$plotType == "Bar Chart") {
      ggplot(data, aes_string(x = "species", fill = input$variable)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = "Bar Chart of Species", x = "Species", y = "Count")
      
    } else if (input$plotType == "Scatterplot") {
      ggplot(data, aes_string(x = "bill_length_mm", y = input$variable, color = "species")) +
        geom_point() +
        theme_minimal() +
        labs(title = "Scatterplot", x = "Bill Length (mm)", y = input$variable)
    }
  })
}

shinyApp(ui = ui, server = server)

