library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

shinyUI(dashboardPage(
  skin = 'green', 
  dashboardHeader(title = "Buster's Business"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      menuItem('Order List', tabName = 'order_list', icon = icon('th-list')),
      menuItem('Client Map', tabName = 'client_map', icon = icon('map')),
      menuItem('Financials', tabName = 'financials', icon = icon('money')),
      menuItem('Add Order Form', tabName = 'order_form', icon = icon('file')),
      uiOutput('select_client')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard',
              fluidRow(
                box(title = 'Order Status', status = 'info', solidHeader = TRUE, width = 12, align = 'center',
                    h1(textOutput('order_status')))
              ),
              fluidRow(
                box(title = 'Orders Placed', status = 'primary', solidHeader = TRUE,
                    plotOutput('orders_placed_date', height = 250)),
                
                box(title = 'Orders Due', status = 'primary', solidHeader = TRUE,
                    plotOutput('orders_due_date', height = 250))
              ),
              fluidRow(
                box(title = 'Next 5 Orders Due', status = 'danger', solidHeader = TRUE, width = 12,
                    dataTableOutput('next_orders'))
              )
      ),
      tabItem(tabName = 'order_list',
              fluidRow(
                box(title = 'All Orders', status = 'danger', solidHeader = TRUE, width = 12,
                    dataTableOutput('all_orders'))
              )
      ),
      tabItem(tabName = 'client_map',
              fluidRow(
                box(title = 'Client Locations', status = 'info', solidheader = FALSE, width = 12,
                    leafletOutput('client_map', height = 800))
              )
      ),
      tabItem(tabName = 'financials',
              fluidRow(
                box(title = 'Overall YTD', status = 'info', solidHeader = TRUE, width = 6, align = 'center',
                    tableOutput('financial_by_client'), tableOutput('financial_totals')),
                box(title = '30-Day Summary', status = 'info', solidHeader = TRUE, width = 6, align = 'center',
                    tableOutput('financial_by_client_30'), tableOutput('financial_totals_30'))
              ),
              fluidRow(
                box(title = 'Order Trend', status = 'info', solidHeader = TRUE, width = 12,
                    plotOutput('financials_plot', height = 400))
              )
      ),
      tabItem(tabName = 'order_form',
              fluidPage(
                box(id = 'create_order', title = 'Create New Order', width = 4, solidHeader = TRUE, status = 'info',
                    verbatimTextOutput('order_num'),
                    uiOutput('select_client_form'),
                    verbatimTextOutput('order_placed_date'),
                    dateInput('due_date', 'Date to Deliver Order'),
                    numericInput('order_price', 'Order Price', value = 0, min = 0),
                    uiOutput('select_product_form'),
                    fluidRow(column(width = 6, numericInput('order_quantity', 'Order Quantity', value = 1, min = 1)),
                             column(width = 6, uiOutput('order_quantity_class'))),
                    textInput('order_note', 'Order Note'),
                    actionButton('submit_order', 'Submit', class = 'btn-primary')
                ),
                box(id = 'create_client', title = 'Create New Client', width = 4, solidHeader = TRUE, status = 'warning',
                    textInput('client_name', 'Client Name'),
                    textInput('client_note', 'Note'),
                    numericInput('client_lng', 'Client Longitude', value = NA),
                    numericInput('client_lat', 'Client Latitutde', value = NA),
                    actionButton('submit_client', 'Submit', class = 'btn-primary')
                ),
                box(id = 'create_product', title = 'Create New Product', width = 4, solidHeader = TRUE, status = 'danger',
                    textInput('product_name', 'Product Name'),
                    textInput('five_by_five_amt', '5x5 Seed Amount'),
                    textInput('half_tray_amt', 'Half Tray Seed Amount'), 
                    textInput('full_tray_amt', 'Full Tray Amount'),
                    numericInput('days_to_grow', 'Days to Grow', value = NA),
                    textInput('product_note', 'Product Note'),
                    actionButton('submit_product', 'Submit', class = 'btn-primary', color = 'green')
                )
              )
      )
      )
)))