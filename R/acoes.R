#' @title CRAWLER ACOES 
#' @name get_acoes
#' @description captura preco das acoes a partir da library BatchGetSymbols

#' @param tickers vetor com tickers das empresas
#' @param data_inicio a partir de quando capturar os dados da bolsas 
#' @param data_fim ate quando capturar os dados da bolsas
#' @examples
#' get_acoes("PETR3","2000-01-01","2021-01-01")
#' @return dataframe com as acoes da bolsa
#' @import dplyr
#' @export
#' @author Pedro Cavalcante

get_acoes<-function(tickers,data_inicio=as.Date('2000-01-01'),data_fim=Sys.Date()){
  sss=BatchGetSymbols::BatchGetSymbols(paste(tickers,".SA",sep=""),first.date=data_inicio,
                      last.date=data_fim,bench.ticker="^BVSP")
  acoes=sss$df.tickers
  acoes$ticker=substr(acoes$ticker,1,stringr::str_length(acoes$ticker)-3)
  acoes=acoes%>%summarise(dte_data=ref.date,
                          str_ticker=ticker,
                          vl_close=price.close,
                          vl_volume=volume)
  return(acoes)
}

