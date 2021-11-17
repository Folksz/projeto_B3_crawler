#' @title DEPARA TICKER NOME EMPRESA
#' @name ticker_empresa
#'
#' @description A partir de um crawler oriundo do site fundamentus, cria um depara entre o ticker da empresa listada
#' na bolsa e a sau razao social
#' @return dataframe com nome do ticker, nome da companhia e nome que esta na razao social
#' @author Pedro Cavalcante
#' @import dplyr
#' @export


ticker_empresa <- function() {
  html = "https://www.fundamentus.com.br/detalhes.php?papel="
  depara = (
    xml2::read_html(html) %>% rvest::html_nodes(xpath = '//*[@id="test1"]') %>%
      rvest::html_table()
  )[[1]]
  colnames(depara) = c("str_ticker", "str_nome_cia_ticker", "str_razao_social")
  #Tira os Espacos
  depara$str_razao_social = stringi::stri_trans_general(depara$str_razao_social, "Latin-ASCII")
  #Tira o Traco
  depara$str_razao_social =  gsub("- ", "", as.character(depara$str_razao_social))
  return (depara)
}