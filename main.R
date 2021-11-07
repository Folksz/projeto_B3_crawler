rm(list=ls())
source("libraries.R")

#Inicializar com bulk()
#Construcao do BD
anos=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
dfs=construir_bd(anos,"output/",gerar_cotacao=FALSE)

#Atualizar Ponta, soh o ultimo ano
dfs=atualizar_ponta(diretorio="output/",gerar_cotacao=FALSE)

x=info_financeira_brasil("PMS_RECEITA")
y=importar_sgs("juros")

diretorio="output/"
tbl_dre<-read.csv(paste(diretorio,"tbl_dre.csv",sep=""))
tbl_bp_ativo<-read.csv(paste(diretorio,"tbl_bp_ativo.csv",sep=""))
tbl_bp_passivo<-read.csv(paste(diretorio,"tbl_bp_passivo.csv",sep=""))
tbl_empresas<-read.csv(paste(diretorio,"tbl_empresas.csv",sep=""))
tbl_ticker<-read.csv(paste(diretorio,"tbl_ticker.csv",sep=""))
tbl_divulgacoes<-read.csv(paste(diretorio,"tbl_divulgacoes.csv",sep=""))

#Construcao de Metricas Empresas por Data
empresas_analisadas=c("ABEV3","RADL3","MGLU3")
empresas_por_data=metricas_empresas_por_data(empresas_analisadas,tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker,data='2021-03-31')
#Construcao de Metricas Evolucao de empresas
dre_evolucao=evolucao_dre_por_empresa(ticker="PETR3",tbl_dre,tbl_empresas,tbl_ticker)
bp_evolucao=evolucao_bp_por_empresa(ticker="ABEV3",tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker)
metrica_evolucao=evolucao_metrica_por_empresa(ticker="ABEV3",tbl_dre,tbl_bp_ativo,tbl_bp_passivo,tbl_empresas,tbl_ticker)
#Construcao Metriaca preco
metrica_preco=metrica_via_preco(tickers=c("ABEV3","RADL3","ITUB4"),data_inicio=as.Date('2017-01-01'),data_fim=Sys.Date())
metrica_preco%>%select(str_ticker,dte_data,P_L)%>%ggplot()+geom_line(aes(x=dte_data,y=P_L,colour=str_ticker))
t=tbl_empresas%>%group_by(str_setor)%>%summarise(valor=n())
ggplot()+geom_bar(data=t,aes(x=str_setor,y=valor),stat="identity")+coord_flip()+xlab("")+ggtitle("Setores de Empresas Listadas na B3")

exportar_ctrl <- function(df, sep="\t", dec=".", max.size=(200*1000)){
  # Copy a data.frame to clipboard
  write.table(df, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep,row.names=FALSE, dec=dec)
}
exportar_ctrl(unique(tbl_dre%>%select(cd_conta,ds_conta)))