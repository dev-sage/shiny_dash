## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)

server <- function(input, output) {
  
  order_data <- readRDS("./data/order_data.rds")
  
  output$select_clients <- renderUI({
    client_list <- unique(order_data$client)
    selectInput('client_select', label = 'Select Client', choices = client_list)
  })
  
  output$plot1 <- renderPlot({
    plot_data <- order_data[order_data$client == input$client_select,]
    ggplot(plot_data, aes(x = order_date, y = order_price)) +
      geom_bar(stat = 'identity')
  })
}