serie_CVM<-function(anos){
  sufixos_desejaveis=c("BPA_con","BPP_con","DRE_con","DRE_ind","") #VOU ANALISAR SOH OS CONSOLIDADOS
  lista_itr=excel_por_ano(anos,sufixos_desejaveis,"itr")
  lista_dfp=excel_por_ano(anos,sufixos_desejaveis,"dfp")
  return(list("itr"=lista_itr,"dfp"=lista_dfp))  
}
excel_por_ano<-function(anos,sufixos_desejaveis,itr_dfp){
  lista=list()
  for(ano in anos){
    temp<-"a.xlsx"
    #Baixa o arquivo por ano
    site_base=paste('http://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/ITR/DADOS/','itr_cia_aberta_',ano,".zip",sep="")
    site_base=stringr::str_replace(site_base,"ITR",toupper(itr_dfp))
    site_base=stringr::str_replace(site_base,"itr",tolower(itr_dfp))
    download.file(site_base,temp)
    for(sufixo in sufixos_desejaveis){
      nome_sufixo=paste("itr_cia_aberta_",sufixo,sep="")
      nome_sufixo=stringr::str_replace(nome_sufixo,"itr",itr_dfp)
      nome_sufixo_ano=paste(nome_sufixo,"_",ano,sep="")
      nome_sufixo_ano=stringr::str_replace(nome_sufixo_ano,"__","_")
      arquivo_sufixo=paste(nome_sufixo_ano,".csv",sep="")
      #Baixa o arquivo pelo sufixo desejado
      unzip(temp,arquivo_sufixo)
      df=read.csv(arquivo_sufixo,sep=";")
      lista[[nome_sufixo]]=rbind(lista[[nome_sufixo]],df)
      unlink(arquivo_sufixo)  
    }  
    unlink(temp)  
  }
  return(lista)
}
