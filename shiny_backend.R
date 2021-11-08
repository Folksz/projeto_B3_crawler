construir_body<-function(){
return(dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
))
}

construir_sidebar<-function(){
return(dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
))
}


construir_header<-function(){
return(dashboardHeader(title = "Basic dashboard"))
}

construir_shiny<-function(){
dashboardPage(
  header,
  sidebar,
  body
)}



  