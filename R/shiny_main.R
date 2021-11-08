#' @title Criar Painel
#' @name criar_painel
#' @description Constroi painel em Rshiny
#' @return 
#' @author Pedro Cavalcante
#' @export
#' @import dplyr






server <- function(input, output) {
  #Linha
  output$dre_por_empresa_linha <- renderPlotly({
    montar_DRE_por_empresa(tbl_dre=dfs$tbl_dre,
                           tbl_ds_conta=dfs$tbl_ds_conta,
                           empresa_selec = input$input_empresa_unique_dre,
                           anual_trimestral=input$input_anual_trimestral_a,
                           contas_selec=input$input_contas_multiple_dre,
                           linha_coluna="linha")})
  
  output$bp_por_empresa_linha <- renderPlotly({
    montar_BP_por_empresa(tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                          tbl_ds_conta=dfs$tbl_ds_conta,
                          empresa_selec = input$input_empresa_unique_bp,
                          contas_selec=input$input_contas_multiple_bp,
                          linha_coluna="linha")})
  
  output$dre_entre_empresa_linha <- renderPlotly({
    montar_DRE_entre_empresa(tbl_dre=dfs$tbl_dre,
                             tbl_ds_conta=dfs$tbl_ds_conta,
                             empresas_selec = input$input_empresa_multiple_dre,
                             anual_trimestral=input$input_anual_trimestral_b,
                             conta_selec=input$input_contas_unique_dre,
                             linha_coluna="linha")})
  output$bp_entre_empresa_linha <- renderPlotly({
    montar_BP_entre_empresa(tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                            tbl_ds_conta=dfs$tbl_ds_conta,
                            conta_selec=input$input_contas_unique_bp,
                            empresas_selec = input$input_empresa_multiple_bp,
                            linha_coluna="linha")})
  #coluna
  output$dre_por_empresa_coluna <- renderPlotly({
    montar_DRE_por_empresa(tbl_dre=dfs$tbl_dre,
                           tbl_ds_conta=dfs$tbl_ds_conta,
                           empresa_selec = input$input_empresa_unique_dre,
                           anual_trimestral=input$input_anual_trimestral_a,
                           contas_selec=input$input_contas_multiple_dre,
                           linha_coluna="coluna")})
  output$bp_por_empresa_coluna <- renderPlotly({
    montar_BP_por_empresa(tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                          tbl_ds_conta=dfs$tbl_ds_conta,
                          empresa_selec = input$input_empresa_unique_bp,
                          contas_selec=input$input_contas_multiple_bp,
                          linha_coluna="coluna")})
  output$dre_entre_empresa_coluna <- renderPlotly({
    montar_DRE_entre_empresa(tbl_dre=dfs$tbl_dre,
                             tbl_ds_conta=dfs$tbl_ds_conta,
                             empresas_selec = input$input_empresa_multiple_dre,
                             anual_trimestral=input$input_anual_trimestral_b,
                             conta_selec=input$input_contas_unique_dre,
                             linha_coluna="coluna")})
  output$bp_entre_empresa_coluna <- renderPlotly({
    montar_BP_entre_empresa(tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                            tbl_ds_conta=dfs$tbl_ds_conta,
                            conta_selec=input$input_contas_unique_bp,
                            empresas_selec = input$input_empresa_multiple_bp,
                            linha_coluna="coluna")})
  output$cotacao_entre_empresa_valor <- renderPlotly({
    montar_cotacao_entre_empresa(tbl_cotacao=dfs$tbl_cotacoes,
                                 tickers_selec = input$input_tickers_multiple,
                                 qual_variavel="valor_empresa")})
  output$cotacao_entre_empresa_cotacao <- renderPlotly({
    montar_cotacao_entre_empresa(tbl_cotacao=dfs$tbl_cotacoes,
                                 tickers_selec = input$input_tickers_multiple,
                                 qual_variavel="vl_close")})
  output$multiplo_entre_empresa_linha <- renderPlotly({
    montar_multiplo_entre_empresas(tbl_dre=dfs$tbl_dre,
                                   tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                                   empresas_selec = input$input_empresa_multiple_multiplo,
                                   qual_variavel=input$input_multiplo,
                                   linha_coluna="linha")})
  output$multiplo_entre_empresa_coluna <- renderPlotly({
    montar_multiplo_entre_empresas(tbl_dre=dfs$tbl_dre,
                                   tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                                   empresas_selec = input$input_empresa_multiple_multiplo,
                                   qual_variavel=input$input_multiplo,
                                   linha_coluna="coluna")})
  output$variaveis_macro <- renderPlotly({montar_variavel_macro(input$input_variavel_macro)})
}

criar_painel<-function(diretorio=""){

dfs<<-atualizar_ponta(diretorio,gerar_cotacao=FALSE)
ui <- dashboardPage(construir_header(),
                    construir_sidebar(),
                    construir_body())

shinyApp(ui, server)
}



