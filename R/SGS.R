#' @title API SIDRA
#' @name importar_sidra
#' @description Variavel que se utiliza da API do ipeadatar para capturar alguns dados relevantes à analise
#' @param info Variavel de busca que pode ser "juros","dolar"
#' @return dataframe com a data e com a varavel requisitada
#' @author Pedro Cavalcante
#' @examples
#' importar_sgs("juros")
#' @import dplyr
#' @export
importar_sgs <-
  function(info) {
    #Captura as informa?oes de Dolar e Juros
    #series_ipea <- available_series(language = "br") #Analisa todas as variaveis dissponiveis
    if (!(info %in% c("juros", "dolar"))) {
      print("Variaveis diferentes de juros e dolar")
      return()
    }
    if (info == "juros") {
      df = ipeadatar::ipeadata("BM366_TJOVER366") #Juros
      df = df[, c(2, 3)]
      colnames(df) = c("dte_data", "vl_last")
      df$dte_data = as.Date(df$dte_data)
      
    }
    if (info == "dolar") {
      df = ipeadatar::ipeadata("GM366_ERC366") #DOlar
      df = df[, c(2, 3)]
      colnames(df) = c("dte_data", "vl_last")
      df$dte_data = as.Date(df$dte_data)
    }
    df = df %>% filter(dte_data > as.Date('2000-01-01'))
    return(df)
  }
