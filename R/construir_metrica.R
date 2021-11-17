#' @title Criar Metricas
#' @name construir_metricas
#' @description Constroi metricas a partir de tbl_dre, tbl_bp
#' @param tbl_dre Tabela de Demonstrativo de Resultados
#' @param tbl_bp Tabela de Balanco Patrimonial
#' @param empresas_selec Selecionar Empresas para construir metricas
#' @return retorna df com as informacoes das metricas
#' @author Pedro Cavalcante
#' @export
#' @import dplyr



construir_metricas <- function(tbl_dre, tbl_bp, empresas_selec) {
  tbl_painel = data.frame()
  for (empresa in empresas_selec) {
    tbl_dre_empresa = rbind(
      tbl_dre %>% filter(regex == empresa),
      tbl_dre %>% filter(regex == empresa) %>%
        group_by(cd_conta) %>% arrange(dte_data) %>%
        mutate(
          cd_conta = paste(cd_conta, "_anual", sep = ""),
          vl_conta = vl_conta + dplyr::lag(vl_conta, 1) +
            dplyr::lag(vl_conta, 2) + dplyr::lag(vl_conta, 3)
        )
    )
    tbl_bp_empresa = tbl_bp %>% filter(regex == empresa)
    tbl_painel_empresa = merge(
      tbl_dre_empresa %>% reshape2::dcast(dte_data ~ cd_conta, value.var = "vl_conta"),
      tbl_bp_empresa %>% reshape2::dcast(dte_data ~
                                           cd_conta, value.var = "vl_conta")
    )
    tbl_painel_empresa = tbl_painel_empresa %>%
      summarise(
        dte_data,
        patrimonio_liquido = (`2.03`),
        lucro_liquido_12M = `3.11.01_anual`,
        indice_liquidez = `1.01` / `2.01`,
        #Lucro Liquido/Receita Liquida
        margem_liquida = 100 * `3.11.01` / `3.01`,
        #Ebit: Ebit/Receita Liquida
        margem_ebit = 100 * (`3.05` - `3.04.06` - `3.04.05` - `3.04.04`) /
          (`3.01`),
        #Margem Bruta:Lucro Bruto/Receita Liquida
        margem_bruta = 100 * `3.03` / `3.01`,
        #Giro Ativo: Receita Liquida (acumulado 12 meses)/Ativo Total
        giro_ativo = (`3.01_anual` / `1`),
        #Divida bruta / patrimonio liquido
        #divida_bruta_patrimonio=((`2.01.04`+`2.02.01`)/patrimonio_liquido),
        #ROE: Lucro Liquido (acumulado 12 meses)/patrimonio liqudio
        ROE = 100 * lucro_liquido_12M / patrimonio_liquido,
        #EBIT (acumulado 12 meses)/ativo
        ebit_ativo = 100 * (
          `3.05_anual` - `3.04.06_anual` - `3.04.05_anual` - `3.04.04_anual`
        ) / (`1`),
        str_empresa = empresa
        #,lucro_liquido_acao=1000*lucro_liquido_12M/vl_acao,
        #valor_patrimonial_acao=1000*patrimonio_liquido/vl_acao
      )
    tbl_painel = rbind(tbl_painel, tbl_painel_empresa)
  }
  
  
  return(tbl_painel)
  
}