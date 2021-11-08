construir_sidebar<-function(){
  return(dashboardSidebar(
  sidebarMenu(
    menuItem("DRE por empresa", tabName = "dre_por_empresa", icon = icon("dashboard")),
    menuItem("BP por empresa", tabName = "bp_por_empresa", icon = icon("th")),
    menuItem("DRE entre empresas", tabName = "dre_entre_empresa", icon = icon("dashboard")),
    menuItem("BP entre empresas", tabName = "bp_entre_empresa", icon = icon("th")),
    menuItem("Multiplos por Empresa", tabName = "cotacao_entre_empresa", icon = icon("th")),
    menuItem("Variaveis Macro", tabName = "variaveis_macro", icon = icon("th"))
   #, menuItem("DRES", tabName = "dres", icon = icon("th")),
  )
))
}

