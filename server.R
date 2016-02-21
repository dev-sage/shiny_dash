## server.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(DT)
library(dplyr)
library(leaflet)
library(xtable)

order_data <- readRDS('./data/order_data.rds')
client_list <- c('All', unique(order_data$client))

shinyServer(function(input, output) {
  
  InputData <- reactive({
    if(!tolower(input$client_select) == 'all') return(order_data[order_data$client == input$client_select,])
    order_data
  })

  output$order_status <- renderText({
    paste('COMPLETED: ', as.character(nrow(InputData()[InputData()$order_status == 'Completed',])),
          'OUTSTANDING: ', as.character(nrow(InputData()[InputData()$order_status == 'In Progress',])),
          sep = ' ')
  })
  
  output$select_clients <- renderUI({
    selectInput('client_select', label = 'Select Client', choices = client_list)
  })
  
  output$orders_placed_date <- renderPlot({
    ggplot(InputData(), aes(x = order_date)) +
      geom_bar() + xlab('Date Order Placed') + ylab('# of Orders Placed')
  })
  
  output$orders_due_date <- renderPlot({
    ggplot(InputData(), aes(x = due_date)) +
      geom_bar() + xlab('Date Order Due') + ylab('# of Orders Due')
  })
  
  output$next_orders <- DT::renderDataTable({
    selected_data <- InputData()
    top5 <- selected_data[selected_data$order_status == 'In Progress',]
    top5 <- top5[order(top5$due_date, decreasing = TRUE),][1:5,]
    DT::datatable(top5, options = list(paging = FALSE, searching = FALSE))
  })
  
  output$all_orders <- renderDataTable({
    order_data[order(order_data$due_date, decreasing = TRUE), ]
  })
  
  output$client_map <- renderLeaflet({
    map_data <- data.frame('client' = unique(order_data$client), 'client_lat' = unique(order_data$client_lat), 'client_lon' = unique(order_data$client_lon))
    leaflet(map_data) %>% 
      addProviderTiles('Stamen.Terrain') %>% addMarkers(lat = ~client_lat, 
                                                        lng = ~client_lon,
                                                        popup = ~client)
                                                              
  })
  
  output$financial_output <- renderTable({
    xtable(InputData()[1:5,])
  })
})
