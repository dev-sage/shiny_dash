ui <- dashboardPage(
  skin = 'green', 
  dashboardHeader(title = 'Mt. Man Micro'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('dashboard')),
      menuItem('Widgets', tabName = 'widgets', icon = icon('th')),
      uiOutput('select_clients')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard',
              fluidRow(
                box(title = 'Order Status', status = 'danger', solidHeader = TRUE,
                    h1(textOutput('order_status')))
              ),
              fluidRow(
                box(title = 'Orders by Date', status = 'primary', solidHeader = TRUE,
                    plotOutput('plot1', height = 250)),
                textOutput('client_list')
              )
      ),
      tabItem(tabName = 'widgets',
              h2('Widgets tab content')
      )
    )
  )
)