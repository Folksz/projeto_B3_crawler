montar_DRE_por_empresa<-function(tbl_dre=dfs$tbl_dre,
                                 tbl_ds_conta=dfs$tbl_ds_conta,
                                 anual_trimestral="trimestral",
                                 contas_selec,#=c("3.01","3.02","3.03","3.04","3.05","3.09"),
                                 empresa_selec="MGLU",
                                 linha_coluna="linha"){
  contas_selec=(tbl_ds_conta%>%filter(ds_conta %in% contas_selec))$cd_conta
  tbl_dre_empresa=tbl_dre%>%filter(regex==empresa_selec)%>%filter(cd_conta %in% contas_selec)
  if(anual_trimestral=="anual"){
    tbl_dre_empresa=tbl_dre_empresa%>%
      group_by(paste(cd_conta,year(dte_data)))%>%
      summarise(dte_data=last(year(dte_data)),ds_conta=last(ds_conta),
                vl_conta=sum(vl_conta,na.rm=TRUE))%>%
      ungroup()
  }
  if(anual_trimestral=="trimestral"){
    tbl_dre_empresa=tbl_dre_empresa%>%ungroup()%>%arrange(dte_data)%>%mutate(dte_data=as.Date(dte_data))
  }
  if(linha_coluna=="linha"){
    type="scatter"
  }
  if(linha_coluna=="coluna"){
    type="bar"
  }
#  print(min(tbl_dre_empresa$dte_data))
  return(plot_ly(tbl_dre_empresa,
                 x=~dte_data,
                 y=~vl_conta,
                 color=~ds_conta,
                 type=type,
                 mode='lines')%>%
                  layout(legend = list(x = 0,
                                       y = 1)))
}

montar_BP_por_empresa<-function(tbl_bp,
                                tbl_ds_conta,
                                contas_selec,#=c("2","2.03"),
                                empresa_selec="MGLU",
                                linha_coluna="linha"){
  contas_selec=(tbl_ds_conta%>%filter(ds_conta %in% contas_selec))$cd_conta
  tbl_bp_empresa=tbl_bp%>%
    filter(regex==empresa_selec)%>%
    filter(cd_conta %in% contas_selec)
  if(linha_coluna=="linha"){
    type="scatter"
  }
  if(linha_coluna=="coluna"){
    type="bar"
  }
  
  return(plot_ly(tbl_bp_empresa%>%arrange(dte_data),
                 x=~dte_data,
                 y=~vl_conta,
                 color=~ds_conta,
                 type=type,
                 mode='lines')%>%
           layout(legend = list(x = 0,
                                y = 1))
         )
}







montar_DRE_entre_empresa<-function(tbl_dre=dfs$tbl_dre,
                                 tbl_ds_conta=tbl_ds_conta,
                                 anual_trimestral="trimestral",
                                 conta_selec,#=c("3.01","3.02","3.03","3.04","3.05","3.09"),
                                 empresas_selec,
                                 contas,
                                 linha_coluna="linha"){
  conta_selec=(tbl_ds_conta%>%filter(ds_conta == conta_selec))$cd_conta
  tbl_dre_empresa=tbl_dre%>%filter(regex %in% empresas_selec)%>%filter(cd_conta==conta_selec)
  print(anual_trimestral)
  if(anual_trimestral=="anual"){
    tbl_dre_empresa=tbl_dre_empresa%>%
      group_by(paste(regex,year(dte_data)))%>%
      summarise(dte_data=last(year(dte_data)),
                regex=last(regex),
                vl_conta=sum(vl_conta,na.rm=TRUE))%>%
      ungroup()
  }
  if(anual_trimestral=="trimestral"){
    tbl_dre_empresa=tbl_dre_empresa%>%ungroup()%>%arrange(dte_data)%>%mutate(dte_data=as.Date(dte_data))
  }
  if(linha_coluna=="linha"){
    type="scatter"
  }
  if(linha_coluna=="coluna"){
    type="bar"
  }
  return(plot_ly(tbl_dre_empresa,x=~dte_data,y=~vl_conta,color=~regex,type=type,mode='lines')%>%
           layout(legend = list(x = 0,
                                y = 1)))
}
montar_BP_entre_empresa<-function(tbl_bp,
                                tbl_ds_conta,
                                conta_selec,#=c("2","2.03"),
                                empresas_selec,
                                linha_coluna="linha"){
  conta_selec=(tbl_ds_conta%>%filter(ds_conta == conta_selec))$cd_conta
  tbl_bp_empresa=tbl_bp%>%filter(regex %in% empresas_selec)
  tbl_bp_empresa=tbl_bp_empresa%>%filter(cd_conta == conta_selec)
  if(linha_coluna=="linha"){type="scatter"}
  if(linha_coluna=="coluna"){type="bar"}
  return(plot_ly(tbl_bp_empresa%>%arrange(dte_data),
                 x=~dte_data,
                 y=~vl_conta,
                 color=~regex,
                 type=type,
                 mode='lines')%>%
           layout(legend = list(x = 0,
                                y = 1)))
}
montar_cotacao_entre_empresa<-function(tbl_cotacao,
                                  tickers_selec,
                                  qual_variavel){
  if(!is.null(tickers_selec)){
  tbl_cotacao_empresa=tbl_cotacao%>%filter(str_ticker %in% tickers_selec)
  tbl_cotacao_empresa=merge(tbl_cotacao_empresa,buscar_num_acoes(tickers_selec))
  tbl_cotacao_empresa=tbl_cotacao_empresa%>%
                      ungroup()%>%mutate(valor_empresa=vl_acao*vl_close)
  tbl_cotacao_empresa$qual_variavel=tbl_cotacao_empresa[,qual_variavel]
  return(plot_ly(tbl_cotacao_empresa%>%arrange(dte_data),
                 x=~dte_data,
                 y=~qual_variavel,
                 color=~str_ticker,
                 type="scatter",
                 mode='lines')%>%
           layout(legend = list(x = 0,
                                y = 1)))
  }
  return()
}
montar_multiplo_entre_empresas<-function(
                                tbl_dre=dfs$tbl_dre,
                                tbl_bp=rbind(dfs$tbl_bp_ativo,dfs$tbl_bp_passivo),
                                #tbl_empresas=dfs$tbl_empresas,
                                #tbl_cotacoes=dfs$tbl_cotacoes,
                                empresas_selec,
                                qual_variavel,
                                linha_coluna){
  if(!is.null(empresas_selec)){
  tbl_dre=tbl_dre%>%filter(regex %in% empresas_selec)%>%ungroup()
  tbl_bp=tbl_bp%>%filter(regex %in% empresas_selec)%>%ungroup()
  tbl_painel=data.frame()
  print(empresas_selec)
  for(empresa in empresas_selec){
  tbl_dre_empresa=rbind(tbl_dre%>%filter(regex==empresa),
                        tbl_dre%>%filter(regex==empresa)%>%
                          group_by(cd_conta)%>%arrange(dte_data)%>%
                          mutate(
                            cd_conta=paste(cd_conta,"_anual",sep=""),
                            vl_conta=vl_conta+dplyr::lag(vl_conta,1)+dplyr::lag(vl_conta,2)+dplyr::lag(vl_conta,3)))
  tbl_bp_empresa=tbl_bp%>%filter(regex==empresa)
  tbl_painel_empresa=merge(tbl_dre_empresa%>%dcast(dte_data~cd_conta,value.var="vl_conta"),
                   tbl_bp_empresa%>%dcast(dte_data~cd_conta,value.var="vl_conta"))
  tbl_painel_empresa=tbl_painel_empresa%>%
    summarise(dte_data,
              patrimonio_liquido=(`2.03`),
              lucro_liquido_12M=`3.11.01_anual`,
              indice_liquidez=`1.01`/`2.01`,
              #Lucro Liquido/Receita Liquida
              margem_liquida=100*`3.11.01`/`3.01`,
              #Ebit: Ebit/Receita Liquida
              margem_ebit=100*(`3.05` - `3.04.06`- `3.04.05`-`3.04.04`)/(`3.01`),
              #Margem Bruta:Lucro Bruto/Receita Liquida
              margem_bruta=100*`3.03`/`3.01`,
              #Giro Ativo: Receita Liquida (acumulado 12 meses)/Ativo Total
              giro_ativo=(`3.01_anual`/`1`),
              #Divida bruta / patrimonio liquido
              #divida_bruta_patrimonio=((`2.01.04`+`2.02.01`)/patrimonio_liquido),
              #ROE: Lucro Liquido (acumulado 12 meses)/patrimonio liqudio
              ROE=100*lucro_liquido_12M/patrimonio_liquido,
              #EBIT (acumulado 12 meses)/ativo
              ebit_ativo=100*(`3.05_anual` - `3.04.06_anual`- `3.04.05_anual`-`3.04.04_anual`)/(`1`),
              str_empresa=empresa
              #,lucro_liquido_acao=1000*lucro_liquido_12M/vl_acao,
              #valor_patrimonial_acao=1000*patrimonio_liquido/vl_acao
              )
  tbl_painel=rbind(tbl_painel,tbl_painel_empresa)
  }
  tbl_painel$qual_variavel=tbl_painel[,qual_variavel]
  if(linha_coluna=="linha"){
    type="scatter"
  }
  if(linha_coluna=="coluna"){
    type="bar"
  }
  return(plot_ly(tbl_painel%>%arrange(dte_data),
                 x=~dte_data,
                 y=~qual_variavel,
                 color=~str_empresa,
                 type=type,
                 mode='lines')%>%
           layout(legend = list(x = 0,
                                y = 1)))
  }
  return()
}
montar_variavel_macro<-function(variavel_macro){
  if(variavel_macro !=""){  
  if(variavel_macro %in% c("dolar","juros")){
    df=importar_sgs(variavel_macro)
    colnames(df)=c("dte_data","vl_valor")
  }else{
    df=importar_sidra(variavel_macro)
    colnames(df)=c("dte_data","vl_valor")
  }
  return(plot_ly(df,
                 x=~dte_data,
                 y=~vl_valor,
                 type='scatter',
                 mode='lines'))}
  return()
}