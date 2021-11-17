#' @title CRAWLER NUMERO DE ACOES EMPRESA
#' @name buscar_num_acoes
#'
#' @description Realiza um crawler no site fundamentus para capturar o numero de acoes que uma empresa
#' listada em bolsa possui
#'
#' @param tickers texto com o ticker da empresa
#'
#' @return retorna o numero de acoes da empresa
#'
#' @author Pedro Cavalcante
#'
#' @examples
#' buscar_num_acoes("PETR3")
#' @import dplyr
#' @export

buscar_num_acoes <- function(tickers) {
  num_acoes = data.frame()
  for (ticker in tickers) {
    html = paste("https://www.fundamentus.com.br/detalhes.php?papel=",
                 ticker,
                 sep = "")
    acao = as.numeric(stringr::str_replace_all((
      xml2::read_html(html) %>% rvest::html_nodes(xpath = '//*[@class="w728"]') %>%
        rvest::html_table()
    )[[2]][4][2, 1],
    "\\.",
    ""
    ))
    num_acoes = rbind(num_acoes,
                      data.frame("str_ticker" = ticker, "vl_acao" = acao))
  }
  return(num_acoes)
}