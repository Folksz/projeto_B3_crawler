#' @title Constroi todo o banco de dados
#' @name construir_bd
#' @description Funcao principal que constroi todo o banco de dados
#' @param anos Quais anos dos dados deseja, o numero tem que ser maior ou igual a 2011
#' @param diretorio Local em que as tabelas serao geradas em csv
#' @param gerar_cotacao Booleana que indica se tambem deseja gerar a cotacao desses tickers
#'
#' @details Funcao principal que constroi todo o banco de dados,
#'     demora um pouco por realiza o download de varios arquivos
#' @return Retorna uma lista com as tabelas (que tambem sao salvas em um csv)
#' @author Pedro Cavalcante
#'@export
#'@examples
#'construir_bd(c(2021),"",TRUE)
#'@import dplyr




construir_bd <-
  function(anos = seq(2011, 2021),
           diretorio = "",
           gerar_cotacao = TRUE) {
    #Capturar dados da CVM
    bulk = serie_CVM(anos)
    #Construir  TBL_DRE a partir de dados da CVM
    tbl_dre = construir_DRE(bulk)
    #Construir o TBL_BP_ATIVO a partir de dados da CVM
    tbl_bp_ativo = construir_bp_ativo(bulk)
    #Construir o TBL_BP_PASSIVO a partir de dados da CVM
    tbl_bp_passivo = construir_bp_passivo(bulk)
    #Puxa dados da B3 e do Fundamentus para realizar o depara entre ticker da empresa e CNPJ
    depara = construir_depara(bulk)
    tbl_empresas = depara$tbl_empresas
    tbl_ticker = depara$tbl_ticker
    tbl_divulgacoes = rbind(bulk$itr$itr_cia_aberta_, bulk$dfp$dfp_cia_aberta_)
    tbl_divulgacoes = tbl_divulgacoes %>% filter(VERSAO == 1) %>% summarise(str_cia_CVM =
                                                                              DENOM_CIA,
                                                                            dte_refer = DT_REFER,
                                                                            dt_publi = DT_RECEB)
    
    
    
    
    
    #Fazer o Match para soh capturar empresas que se tem dos dois lados
    tbl_dre = merge(tbl_dre, tbl_empresas %>% summarise(str_cia_CVM, regex))
    tbl_bp_ativo = merge(tbl_bp_ativo, tbl_empresas %>% summarise(str_cia_CVM, regex))
    tbl_bp_passivo = merge(tbl_bp_passivo,
                           tbl_empresas %>% summarise(str_cia_CVM, regex))
    regex_selecionados = unique(tbl_dre$regex)
    tbl_bp_ativo = tbl_bp_ativo %>% filter(regex %in% regex_selecionados)
    tbl_bp_passivo = tbl_bp_passivo %>% filter(regex %in% regex_selecionados)
    tbl_dre = tbl_dre %>% filter(regex %in% regex_selecionados)
    tbl_empresas = tbl_empresas %>% filter(regex %in% regex_selecionados)
    tbl_ticker = tbl_ticker %>% filter(regex %in% regex_selecionados)
    tbl_divulgacoes = tbl_divulgacoes %>% filter(str_cia_CVM %in% unique(tbl_dre$str_cia_CVM))
    tbl_ds_conta = rbind(
      tbl_dre %>% group_by(paste(cd_conta, ds_conta)) %>%
        summarise(
          ds_conta = last(ds_conta),
          cd_conta = last(cd_conta),
          t = n(),
          str_tabela = "tbl_dre"
        ) %>%
        ungroup() %>%
        group_by(cd_conta) %>%
        filter(t == max(t)),
      tbl_bp_ativo %>% group_by(paste(cd_conta, ds_conta)) %>%
        summarise(
          ds_conta = last(ds_conta),
          cd_conta = last(cd_conta),
          t = n(),
          str_tabela = "tbl_bp_ativo"
        ) %>%
        ungroup() %>%
        group_by(cd_conta) %>%
        filter(t == max(t))
    )
    tbl_ds_conta = rbind(
      tbl_ds_conta,
      tbl_bp_passivo %>% group_by(paste(cd_conta, ds_conta)) %>%
        summarise(
          ds_conta = last(ds_conta),
          cd_conta = last(cd_conta),
          t = n(),
          str_tabela = "tbl_bp_passivo"
        ) %>%
        ungroup() %>%
        group_by(cd_conta) %>%
        filter(t == max(t))
    )
    
    tbl_ds_conta = tbl_ds_conta %>% summarise(ds_conta,
                                              cd_conta,
                                              str_tabela,
                                              nchar = stringr::str_length(cd_conta))
    if (gerar_cotacao == TRUE) {
      tbl_cotacoes = get_acoes(tickers = tbl_ticker$str_ticker, as.Date('2000-01-01'))
      write.csv(tbl_cotacoes,
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
    utils::write.csv(tbl_ds_conta,
                     paste(diretorio, "tbl_ds_conta.csv", sep = ""),
                     row.names = FALSE)
    if (gerar_cotacao == FALSE) {
      tbl_cotacoes = data.frame()
    }
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