ui <- dashboardPage(
  skin = "green", 
  dashboardHeader(title = "Mt. Man Micro"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      uiOutput('select_clients')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Analysis",
                  selectInput("select_client", label = "Select Client: ", 
                              choices = c("1", "2", "3"))
                ),
                box(plotOutput("plot1", height = 250)),
                textOutput("client_list")
              )
      ),
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)