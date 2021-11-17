#' @title Importa banco de dados
#' @name importar_bd
#' @description Funcao principal que importa o banco de dados
#' @param diretorio Local em que as tabelas serao geradas em csv
#' @details Funcao principal que importa o banco de dados,
#' @return Retorna uma lista com as tabelas
#' @author Pedro Cavalcante
#' @export
#' @import dplyr
#' @examples
#' importar_bd("")
#'
#'
importar_bd <- function(diretorio) {
  tbl_dre = utils::read.csv(paste(diretorio, "tbl_dre.csv", sep = ""))
  tbl_bp_ativo = utils::read.csv(paste(diretorio, "tbl_bp_ativo.csv", sep =
                                         ""))
  tbl_bp_passivo = utils::read.csv(paste(diretorio, "tbl_bp_passivo.csv", sep =
                                           ""))
  tbl_empresas = utils::read.csv(paste(diretorio, "tbl_empresas.csv", sep =
                                         ""))
  tbl_ticker = utils::read.csv(paste(diretorio, "tbl_ticker.csv", sep =
                                       ""))
  tbl_divulgacoes = utils::read.csv(paste(diretorio, "tbl_divulgacoes.csv", sep =
                                            ""))
  tbl_cotacoes = utils::read.csv(paste(diretorio, "tbl_cotacoes.csv", sep =
                                         ""))
  tbl_ds_conta = utils::read.csv(paste(diretorio, "tbl_ds_conta.csv", sep =
                                         ""))
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