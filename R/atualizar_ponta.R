#' @title Atualiza banco de dados
#' @name construir_bd
#' @description Funcao principal que atualiza o banco de dados
#' @param diretorio Local em que as tabelas serao geradas em csv
#' @param gerar_cotacao Booleana que indica se tambem deseja gerar a cotacao desses tickers
#' @details Funcao principal que atualiza o banco de dados,
#' @return Retorna uma lista com as tabelas (que tambem sao salvas em um csv)
#' @author Pedro Cavalcante
#' @export
#' @import dplyr
#' @examples
#' atualizar_ponta("",TRUE)
#'
#'
atualizar_ponta <- function(diretorio = "output/",
                            gerar_cotacao = TRUE) {
  print(getwd())
  if (!(file.exists(paste(diretorio, "tbl_dre.csv", sep = "")))) {
    print("Arquivo nao Existe")
    return()
  }
  bd = importar_bd(diretorio)
  
  
  tbl_dre = bd$tbl_dre
  tbl_bp_ativo = bd$tbl_bp_ativo
  tbl_bp_passivo = bd$tbl_bp_passivo
  tbl_empresas = bd$tbl_empresas
  tbl_ticker = bd$tbl_ticker
  tbl_divulgacoes = bd$tbl_divulgacoes
  tbl_cotacoes = bd$tbl_cotacoes
  tbl_ds_conta = bd$tbl_ds_conta
  
  bulk = serie_CVM(2021)
  tbl_dre_ponta = construir_DRE(bulk)
  tbl_bp_ativo_ponta = construir_bp_ativo(bulk)
  tbl_bp_passivo_ponta = construir_bp_passivo(bulk)
  depara_ponta = construir_depara(bulk)
  tbl_empresas_ponta = depara_ponta$tbl_empresas
  tbl_ticker_ponta = depara_ponta$tbl_ticker
  tbl_divulgacoes_ponta = rbind(bulk$itr$itr_cia_aberta_, bulk$dfp$dfp_cia_aberta_)
  tbl_divulgacoes_ponta = tbl_divulgacoes_ponta %>% filter(VERSAO == 1) %>%
    summarise(str_cia_CVM = DENOM_CIA,
              dte_refer = DT_REFER,
              dt_publi = DT_RECEB)
  tbl_dre_ponta = merge(tbl_dre_ponta,
                        tbl_empresas_ponta %>% summarise(str_cia_CVM, regex))
  tbl_bp_ativo_ponta = merge(tbl_bp_ativo_ponta,
                             tbl_empresas_ponta %>% summarise(str_cia_CVM, regex))
  tbl_bp_passivo_ponta = merge(tbl_bp_passivo_ponta,
                               tbl_empresas_ponta %>% summarise(str_cia_CVM, regex))
  regex_selecionados = unique(tbl_dre_ponta$regex)
  tbl_bp_ativo_ponta = tbl_bp_ativo_ponta %>% filter(regex %in% regex_selecionados)
  tbl_bp_passivo_ponta = tbl_bp_passivo_ponta %>% filter(regex %in% regex_selecionados)
  tbl_dre_ponta = tbl_dre_ponta %>% filter(regex %in% regex_selecionados)
  tbl_empresas_ponta = tbl_empresas_ponta %>% filter(regex %in% regex_selecionados)
  tbl_ticker_ponta = tbl_ticker_ponta %>% filter(regex %in% regex_selecionados)
  tbl_divulgacoes_ponta = tbl_divulgacoes_ponta %>% filter(str_cia_CVM %in% unique(tbl_dre_ponta$str_cia_CVM))
  tbl_dre = rbind(tbl_dre, anti_join(tbl_dre_ponta, tbl_dre))
  tbl_bp_ativo = rbind(tbl_bp_ativo, anti_join(tbl_bp_ativo_ponta, tbl_bp_ativo))
  tbl_bp_passivo = rbind(tbl_bp_passivo,
                         anti_join(tbl_bp_passivo_ponta, tbl_bp_passivo))
  tbl_empresas = rbind(tbl_empresas, anti_join(tbl_empresas_ponta, tbl_empresas))
  tbl_ticker = rbind(tbl_ticker, anti_join(tbl_ticker_ponta, tbl_ticker))
  tbl_divulgacoes = rbind(tbl_divulgacoes,
                          anti_join(tbl_divulgacoes_ponta, tbl_divulgacoes))
  if (gerar_cotacao == TRUE) {
    tbl_cotacoes_ponta = get_acoes(tbl_ticker_ponta$str_ticker, as.Date('2021-01-01'))
    tbl_cotacoes = rbind(
      tbl_cotacoes %>% mutate(dte_data = as.Date(dte_data)),
      anti_join(
        tbl_cotacoes_ponta,
        tbl_cotacoes %>% mutate(dte_data = as.Date(dte_data))
      )
    )
    utils::write.csv(tbl_cotacoes,
                     paste(diretorio, "tbl_cotacoes.csv", sep = ""),
                     row.names = FALSE)
  }
  
  utils::write.csv(tbl_dre, paste(diretorio, "tbl_dre.csv", sep = ""), row.names =
                     FALSE)
  utils::write.csv(tbl_bp_ativo,
                   paste(diretorio, "tbl_bp_ativo.csv", sep = ""),
                   row.names = FALSE)
  utils::write.csv(tbl_bp_passivo,
                   paste(diretorio, "tbl_bp_passivo.csv", sep = ""),
                   row.names = FALSE)
  utils::write.csv(tbl_empresas,
                   paste(diretorio, "tbl_empresas.csv", sep = ""),
                   row.names = FALSE)
  utils::write.csv(tbl_ticker,
                   paste(diretorio, "tbl_ticker.csv", sep = ""),
                   row.names = FALSE)
  utils::write.csv(tbl_divulgacoes,
                   paste(diretorio, "tbl_divulgacoes.csv", sep = ""),
                   row.names = FALSE)
  
  return(
    list(
      tbl_dre = tbl_dre,
      tbl_bp_ativo = tbl_bp_ativo,
      tbl_bp_passivo = tbl_bp_passivo,
      tbl_empresas = tbl_empresas,
      tbl_ticker = tbl_ticker,
      tbl_divulgacoes = tbl_divulgacoes,
      tbl_cotacoes = tbl_cotacoes,
      tbl_ds_conta = tbl_ds_conta
    )
  )
}
