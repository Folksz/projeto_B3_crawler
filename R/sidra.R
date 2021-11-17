#' @title API SIDRA
#' @name importar_sidra
#' @description Variavel que se utiliza da API do sidra para capturar alguns dados relevantes à analise
#' @param info Variavel de busca que pode ser "inflacao","pib","abate","PIM","PMS_VOLUME","PMS_RECEITA"#'
#' @return dataframe com a data e com a varavel requisitada
#' @author Pedro Cavalcante
#' @import dplyr
#' @examples
#' importar_sidra("inflacao")
#' @export

importar_sidra <- function(info) {
  if (!(info %in% c(
    "inflacao",
    "pib",
    "abate",
    "PIM",
    "PMS_VOLUME",
    "PMS_RECEITA"
  ))) {
    print("Variavel diferente das aceitaveis")
    return()
  }
  
  if (info == "inflacao") {
    df <- sidrar::get_sidra(
      118,
      period = c("last" = 3000),
      category = list(7169),
      geo = "Brazil"
    )
    df = df[, c(8, 5)]
    colnames(df) = c("dte_data", "vl_last")
    df$dte_data = as.Date(paste(
      substr(df$dte_data, 1, 4),
      substr(df$dte_data, 5, 7),
      "01",
      sep = "-"
    ))
  }
  if (info == "pib") {
    df <- sidrar::get_sidra(
      5932,
      variable = 6564,
      period = c("last" = 3000),
      classific = "c11255",
      category = list(90707),
      geo = "Brazil"
    )
    
    df <- df[, c(8, 5)]
    colnames(df) <- c("dte_data", "vl_last")
    df$dte_data = as.Date(paste(
      substr(df$dte_data, 1, 4),
      3 * as.numeric(substr(df$dte_data, 5, 7)),
      "01",
      sep = "-"
    ))
  }
  
  
  if (info == "abate") {
    df <- sidrar::get_sidra(
      1092,
      period = c("last" = 3000),
      geo = "Brazil",
      variable = 285,
      classific = "c18",
      category = list(992)
    )
    df <- df[, c(8, 5)]
    
    colnames(df) <- c("dte_data", "vl_last")
    
    df$dte_data = as.Date(paste(
      substr(df$dte_data, 1, 4),
      3 * as.numeric(substr(df$dte_data, 5, 7)),
      "01",
      sep = "-"
    ))
  }
  if (info == "PIM") {
    df <- sidrar::get_sidra(
      3653,
      period = c("last" = 3000),
      geo = "Brazil",
      variable = 3139,
      classific = "C544",
      category = list(129314)
    )
    df <- df[, c(8, 5)]
    colnames(df) <- c("dte_data", "vl_last")
    df$dte_data = as.Date(paste(substr(df$dte_data, 1, 4), as.numeric(substr(
      df$dte_data, 5, 7
    )), "01", sep = "-"))
  }
  if (info == "PMS_VOLUME") {
    #8676 Receita
    #8677 Volume
    df <- sidrar::get_sidra(
      6442,
      period = c("last" = 3000),
      geo = "Brazil",
      variable = 8677,
      classific = "C11046",
      category = list(33534)
    )
    df <- df[, c(8, 5)]
    colnames(df) <- c("dte_data", "vl_last")
    df$dte_data = as.Date(paste(substr(df$dte_data, 1, 4), as.numeric(substr(
      df$dte_data, 5, 7
    )), "01", sep = "-"))
  }
  if (info == "PMS_RECEITA") {
    #8676 Receita
    #8677 Volume
    df <- sidrar::get_sidra(
      6442,
      period = c("last" = 3000),
      geo = "Brazil",
      variable = 8677,
      classific = "C11046",
      category = list(33534)
    )
    df <- df[, c(8, 5)]
    colnames(df) <- c("dte_data", "vl_last")
    df$dte_data = as.Date(paste(substr(df$dte_data, 1, 4), as.numeric(substr(
      df$dte_data, 5, 7
    )), "01", sep = "-"))
  }
  
  return(df)
  
}
