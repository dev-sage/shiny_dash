ui <- dashboardPage(
  skin = 'green', 
  dashboardHeader(title = 'Mt. Man Micro'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      menuItem('Order List', tabName = 'order_list', icon = icon('th-list')),
      menuItem('Client Map', tabName = 'client_map', icon = icon('map')),
      uiOutput('select_clients')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard',
              fluidRow(
                box(title = 'Order Status', status = 'danger', solidHeader = TRUE, width = 12, align = 'center',
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
      tabItem(tabName = "order_list",
              fluidRow(
                box(title = 'All Orders', status = 'danger', solidHeader = TRUE, width = 12,
                    dataTableOutput('all_orders'))
              ))
      # tabItem(tabName = 'widgets',
      #         h2('Widgets tab content')
      # )
    )
  )
)