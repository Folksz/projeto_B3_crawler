#' @title CONSTRUIR DEPARA
#' @name construir_depara
#'
#' @description Realiza o depara entre diferentes fontes de dados (B3, CVM e Fundamentus)

#' @param bulk lista com os dados brutos
#'
#' @return lista com a tabela de tickers disponiveis e o nome das empresas
#'
#' @author Pedro Cavalcante

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "aeiouAEIOU",
    circunflex = "âeîôuÂEÎÔU",
    tilde = "aoAOnN",
    umlaut = "äëiöüÄËIÖÜy",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

construir_depara<-function(bulk){
  tbl_CVM=data.frame(unique(bulk$itr$itr_cia_aberta_%>%select(str_cia_CVM=DENOM_CIA,CNPJ_CIA)))
  tbl_CVM$str_cia_CVM_semacento=rm_accent(tbl_CVM$str_cia_CVM)
  #puxa dados do fundamentus
  tbl_ticker_empresa=ticker_empresa()
  tbl_ticker_empresa$str_razao_social_semacento=rm_accent(tbl_ticker_empresa$str_razao_social)
  #Puxa dados da B3
  tbl_setores=buscar_setores()
  
  tbl_ticker_empresa=tbl_ticker_empresa%>%mutate(regex=stringr::str_replace_all(str_razao_social_semacento, "[^[:alnum:]]", ""),
                                                 regex=stringr::str_replace_all(regex, "BANCO", "BCO"))
  tbl_CVM=tbl_CVM%>%mutate(regex=stringr::str_replace_all(str_cia_CVM_semacento, "[^[:alnum:]]", ""),
                           regex=stringr::str_replace_all(regex, "BANCO", "BCO"))
  
  tbl_empresas=merge(tbl_ticker_empresa,tbl_CVM,by="regex")%>%dplyr::select(-regex)
  tbl_empresas=merge(tbl_empresas%>%mutate(regex=stringr::str_replace_all(str_ticker, "[^[:alnum:]]", ""),
                                           regex=gsub('[0-9]+', '', regex)),
                     tbl_setores%>%mutate(regex=str_codigo),by="regex")
  tbl_ticker=tbl_empresas%>%summarise(str_ticker,regex)
  tbl_empresas=tbl_empresas%>%select(-str_ticker)
  tbl_empresas=tbl_empresas[!duplicated(tbl_empresas),]
  return(list("tbl_ticker"=tbl_ticker,"tbl_empresas"=tbl_empresas))
}