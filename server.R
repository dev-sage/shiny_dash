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
  
######################################## 
# Reactive Values and Update Functions # 
########################################  
  reactive_vals <- reactiveValues()
  reactive_vals$order_data <- order_data
  reactive_vals$client_data <- client_data
  reactive_vals$product_data <- product_data
  
  InputData <- reactive({
    if(is.null(input$select_client)) return()
    else if(input$select_client == 'All') return(reactive_vals$order_data)
    else return(reactive_vals$order_data[reactive_vals$order_data$client_name == input$select_client, ])
  })
  
  output$order_status <- renderText({
    if(!is.null(InputData())) {
      paste('COMPLETED: ', as.character(nrow(InputData()[InputData()$order_status == 'Completed',])),
            'OUTSTANDING: ', as.character(nrow(InputData()[InputData()$order_status == 'In Progress',])),
            sep = ' ')
    }
  })
  
  output$select_client <- renderUI({
    selectInput('select_client', label = 'Select Client', choices = c('All', unique(client_data$client_name)))
  })
  
  output$select_client_form <- renderUI({
    selectInput('select_client_form', label = 'Select Client', choices = reactive_vals$client_data$client_name)
  })
  
  output$select_product_form <- renderUI({
    selectInput('select_product_form', label = 'Select Product', choices = reactive_vals$product_data$product_name)
  })

  output$order_quantity_class <- renderUI({
    selectInput('order_quantity_class', label = 'Order Class', choices = c('5x5', 'Half Tray', 'Full Tray'))
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
    reactive_vals$order_data
  })
  
  
  ###############
  # Map clients #
  ###############
  
  output$client_map <- renderLeaflet({
    leaflet(reactive_vals$client_data) %>% 
      addProviderTiles('Stamen.Terrain') %>% addMarkers(lat = ~client_lat, 
                                                        lng = ~client_lng,
                                                        popup = ~client_name)
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
    paste('Order Number:', nrow(reactive_vals$order_data) + 1)
  })
  
  output$order_placed_date <- renderText({
    paste('Order Placed Date:', format(Sys.Date()))
  })
  
  #####################################  
  # UPDATE ORDER/CLIENT/PRODUCT FORMS # 
  #####################################
    
 orderData <- reactive({
     data <- data.frame(order_num = nrow(reactive_vals$order_data) + 1, 
                        client = input$select_client_form,
                        order_placed_date = Sys.Date(),
                        due_date = input$due_date,
                        order_price = input$order_price,
                        product = input$select_product_form,
                        order_quantity = paste(input$order_quantity, input$order_quantity_class, sep = ' '),
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
                      full_tray_amt = input$full_tray_amt,
                      days_to_grow = input$days_to_grow,
                      product_note = input$product_note)
   return(data)
 })

 
  ##############################################  
  # Observe form input, and react accordingly. #
  ##############################################
 
 observe({
   if(input$submit_order) {
     isolate(reactive_vals$order_data <- rbind(reactive_vals$order_data, orderData()))
     saveOrder(reactive_vals$order_data)
     print(reactive_vals$order_data)
   } 
   else if(input$submit_client) {
     isolate(reactive_vals$client_data <- rbind(reactive_vals$client_data, clientData()))
     saveClient(reactive_vals$client_data)
     print(reactive_vals$client_data)
   }
   else if(input$submit_product) {
     isolate(reactive_vals$product_data <- rbind(reactive_vals$product_data, productData()))
     saveProduct(reactive_vals$product_data)
     print(reactive_vals$product_data)
   }
 })
  
})

############################################### 
# SAVING DATA FROM ORDER/CLIENT/PRODUCT FORMS # 
###############################################

saveOrder <- function(order_data) {
  write.csv(x = order_data, file = '~/Dropbox/orders/orders.csv', row.names = FALSE)
}

saveClient <- function(client_data) {
  write.csv(x = client_data, file = '~/Dropbox/clients/clients.csv', row.names = FALSE)
}

saveProduct <- function(data) {
  write.csv(x = product_data, file = '~/Dropbox/products/products.csv', row.names = FALSE)
}


