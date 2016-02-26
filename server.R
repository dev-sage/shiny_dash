## server.R ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(DT)
library(dplyr)
library(leaflet)
library(xtable)
source("helpers.R")

fieldsAll <- c('order_num', 'client', 'order_date', 'due_date', 'order_price', 'order_amount')
responseDir <- file.path("~/Desktop/shiny_save")

order_data <- readRDS('./data/order_data.rds')
client_list <- c('All', unique(order_data$client))

shinyServer(function(input, output) {
  
  InputData <- reactive({
    if(is.null(input$client_select)) return(NULL)
    if(tolower(input$client_select) == 'all') return(order_data)
    order_data[order_data$client == input$client_select,]
  })
  
  output$order_status <- renderText({
    if(!is.null(InputData())) {
      paste('COMPLETED: ', as.character(nrow(InputData()[InputData()$order_status == 'Completed',])),
            'OUTSTANDING: ', as.character(nrow(InputData()[InputData()$order_status == 'In Progress',])),
            sep = ' ')
    }
  })
  
  output$select_clients <- renderUI({
    selectInput('client_select', label = 'Select Client', choices = client_list)
  })
  
  output$orders_placed_date <- renderPlot({
    if(!is.null(InputData())) {
    ggplot(InputData(), aes(x = order_date)) +
      geom_bar() + xlab('Date Order Placed') + ylab('# of Orders Placed')
    }
  })
  
  output$orders_due_date <- renderPlot({
    if(!is.null(InputData())) {
    ggplot(InputData(), aes(x = due_date)) +
      geom_bar() + xlab('Date Order Due') + ylab('# of Orders Due')
    }
  })
  
  output$next_orders <- DT::renderDataTable({
    if(!is.null(InputData())) {
    selected_data <- InputData()
    top5 <- selected_data[selected_data$order_status == 'In Progress',]
    top5 <- top5[order(top5$due_date, decreasing = TRUE),][1:5,]
    DT::datatable(top5, options = list(paging = FALSE, searching = FALSE))
    }
  })
  
  output$all_orders <- renderDataTable({
    order_data[order(order_data$due_date, decreasing = TRUE), ]
  })
  
  output$client_map <- renderLeaflet({
    map_data <- data.frame('client' = unique(order_data$client), 
                           'client_lat' = unique(order_data$client_lat),
                           'client_lon' = unique(order_data$client_lon))
    leaflet(map_data) %>% 
      addProviderTiles('Stamen.Terrain') %>% addMarkers(lat = ~client_lat, 
                                                        lng = ~client_lon,
                                                        popup = ~client)
  })
  
  output$financial_totals <- renderTable({
    financial_data <- financial_summ(order_data)
    xtable(financial_data)
  })
  
  output$financial_by_client <- renderTable({
    financial_data <- client_agg(order_data)
    xtable(financial_data)
  })
  
  output$financial_totals_30 <- renderTable({
    financial_data <- financial_summ_30(order_data)
    xtable(financial_data)
  })

  output$financial_by_client_30 <- renderTable({
    financial_data <- client_agg_30(order_data)
    xtable(financial_data)
  })
  
  output$financials_plot <- renderPlot({
    ggplot(InputData(), aes(x = order_date, y = order_price, col = factor(client))) + 
      geom_point() + 
      xlab("Order Date") + 
      ylab("Order Price") +
      guides(col = guide_legend(title = "Client"))
  })
  
 formData <- reactive({
   data <- sapply(fieldsAll, function(x) input[[x]])
   data <- t(data)
   data
 })
 
 observeEvent(input$submit, { saveData(formData()) } )
  
})

saveData <- function(data) {
  fileName <- "my_file.csv"
  write.csv(x = data, file = file.path(responseDir, fileName), row.names = FALSE, quote = TRUE)
}