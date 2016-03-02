## server.R ##
rm(list = ls())
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(leaflet)
library(xtable)
source("helpers.R")


order_data <- read_orders()
client_data <- read_clients()
product_data <- read_products()

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
  
  output$select_client_form <- renderUI({
    selectInput('client_select_form', label = 'Select Client', choices = client_list)
  })
  
  output$orders_placed_date <- renderPlot({
    if(!is.null(InputData())) {
    ggplot(InputData(), aes(x = order_placed_date)) +
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
                           'client_lng' = unique(order_data$client_lng))
    leaflet(map_data) %>% 
      addProviderTiles('Stamen.Terrain') %>% addMarkers(lat = ~client_lat, 
                                                        lng = ~client_lng,
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
    ggplot(InputData(), aes(x = order_placed_date, y = order_price, col = factor(client))) + 
      geom_point() + 
      xlab("Order Date") + 
      ylab("Order Price") +
      guides(col = guide_legend(title = "Client"))
  })
  
  output$order_num <- renderText({
    paste('Order Number:', nrow(order_data) + 1)
  })
  
  output$order_placed_date <- renderText({
    paste('Order Placed Date:', format(Sys.Date()))
  })
  
 orderData <- reactive({
   data <- data.frame(order_num = nrow(order_data) + 1, 
                      client = input$client_select_form,
                      order_placed_date = Sys.Date(),
                      due_date = input$due_date,
                      order_price = input$order_price,
                      product = input$order_product,
                      order_quantity = input$order_quantity,
                      order_note = input$order_note,
                      order_status = 'In Progress')
   return(data)
 })
 
 clientData <- reactive({
   data <- data.frame(client_name = input$client_name, 
                      client_note = input$client_note,
                      client_lng = input$client_lng,
                      client_lat = input$client_lat)
   return(data)
 })
 
 productData <- reactive({
   data <- data.frame(product_name = input$product_name,
                      five_by_five_amt = input$five_by_five_amt,
                      half_tray_amt = input$half_tray_amt,
                      full_try_amt = input$full_tray_amt,
                      days_to_grow = input$days_to_grow,
                      product_note = input$product_note)
   return(data)
 })
 
 observeEvent(input$submit_order, { saveOrder(orderData()) } )
 observeEvent(input$submit_client, { saveClient(clientData()) })
 observeEvent(input$submit_product, { saveProduct(productData()) })
  
})

saveOrder <- function(data) {
  order_data <<- rbind(order_data, data)
  write.csv(x = order_data, file = '~/Dropbox/orders/orders.csv', row.names = FALSE, col.names = FALSE, append = FALSE)
  print(order_data)
}

saveClient <- function(data) {
  client_data <- rbind(client_data, data) 
  write.csv(x = client_data, file = '~/Dropbox/clients/clients.csv', row.names = FALSE, col.names = FALSE, append = FALSE)
}

saveProduct <- function(data) {
  product_data <<- rbind(product_data, data)
  write.csv(x = product_data, file = '~/Dropbox/products/products.csv', row.names = FALSE, col.names = FALSE, append = FALSE)
}


