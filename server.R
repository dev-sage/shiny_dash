## app.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(data.table)
library(DT)
library(leaflet)

server <- function(input, output) {
  
  order_data <- readRDS("./data/order_data.rds")

  output$order_status <- renderText({
    order_status_data <- order_data[order_data$client == input$client_select,]
    paste("Completed: ", as.character(nrow(order_status_data[order_status_data$order_status == "Completed",])),
          "Outstanding: ", as.character(nrow(order_status_data[order_status_data$order_status == "In Progress",])),
          sep = " ")
  })
  
  output$select_clients <- renderUI({
    client_list <- unique(order_data$client)
    selectInput('client_select', label = 'Select Client', choices = client_list)
  })
  
  output$orders_placed_date <- renderPlot({
    plot_data <- order_data[order_data$client == input$client_select,]
    ggplot(plot_data, aes(x = order_date)) +
      geom_bar() + xlab("Date Order Placed") + ylab("# of Orders Placed")
  })
  
  output$orders_due_date <- renderPlot({
    plot_data <- order_data[order_data$client == input$client_select,]
    ggplot(plot_data, aes(x = due_date)) +
      geom_bar() + xlab("Date Order Due") + ylab("# of Orders Due")
  })
  
  output$next_orders <- DT::renderDataTable({
    top5 <- order_data[order_data$client == input$client_select & order_data$order_status == "In Progress",]
    top5 <- top5[order(top5$due_dat, decreasing = TRUE),][1:5,]
    DT::datatable(top5, options = list(paging = FALSE, searching = FALSE))
  })
  
  output$all_orders <- renderDataTable({
    order_data[order(order_data$due_date, decreasing = TRUE), ]
  })
  
  output$client_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Terrain") %>% addCircleMarkers(data = order_data, 
                                                        lng = client_lon, lat = client_lat, radius = 10,
                                                        popup = clients, col = "blue")
  })
}

# get_palette <- function() {
#   my_pal <- coloNumeric(
#     palette = c("blue", "red")
#     domain = 
#     
# }