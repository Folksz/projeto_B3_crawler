#BP Empresa
dre_por_empresa= tabItem(tabName="dre_por_empresa",
                     fluidRow(tabBox(title="Dre Por Empresa",id="tabset1",height="250px",
                              tabPanel("Linha",plotlyOutput("dre_por_empresa_linha")),
                              tabPanel("Coluna",plotlyOutput("dre_por_empresa_coluna"))),
                              box(selectInput("input_empresa_unique_dre", "Empresa", choices = dfs$tbl_empresas$regex, selected = "MGLU")),
                       box(selectInput("input_contas_multiple_dre", "Contas",
                          choices = unique((dfs$tbl_ds_conta%>%filter(nchar<=4)%>%filter(str_tabela=="tbl_dre"))$ds_conta),
                          multiple=TRUE, selected = TRUE)),
                       box(selectInput("input_anual_trimestral_a", "Anual ou Trimestral", choices = c("anual","trimestral"), selected = "trimestral"))
                     ))
                     
bp_por_empresa= tabItem(tabName="bp_por_empresa",
                     fluidRow(
                       tabBox(title="BP por Empresa",id="tabset2",height="250px",
                              tabPanel("Linha",plotlyOutput("bp_por_empresa_linha")),
                              tabPanel("Coluna",plotlyOutput("bp_por_empresa_coluna"))),
                       box(selectInput("input_empresa_unique_bp", "Empresa", choices = dfs$tbl_empresas$regex, selected = "MGLU")),
                       box(selectInput("input_contas_multiple_bp", "Contas",
                                       choices = unique((dfs$tbl_ds_conta%>%filter(nchar<=4)%>%
                                       filter(str_tabela=="tbl_bp_ativo" | str_tabela=="tbl_bp_passivo"))$ds_conta),
                                       multiple=TRUE, selected = TRUE))))



#BP Empresa
dre_entre_empresa= tabItem(tabName="dre_entre_empresa",
                         fluidRow(
                           tabBox(title="DRE entre Empresa",id="tabset3",height="250px",
                                  tabPanel("Linha",plotlyOutput("dre_entre_empresa_linha")),
                                  tabPanel("Coluna",plotlyOutput("dre_entre_empresa_coluna"))),
                           box(selectInput("input_empresa_multiple_dre", "Empresa", choices = dfs$tbl_empresas$regex, selected = TRUE,multiple=TRUE)),
                           box(selectInput("input_contas_unique_dre", "Contas",
                                           choices = unique((dfs$tbl_ds_conta%>%filter(nchar<=4)%>%filter(str_tabela=="tbl_dre"))$ds_conta),
                                           multiple=FALSE)),
                           box(selectInput("input_anual_trimestral_b", "Anual ou Trimestral", choices = c("anual","trimestral"), selected = "trimestral"))
                         ))
bp_entre_empresa= tabItem(tabName="bp_entre_empresa",
                        fluidRow(
                          tabBox(title="BP entre Empresa",id="tabset4",height="250px",
                                 tabPanel("Linha",plotlyOutput("bp_entre_empresa_linha")),
                                 tabPanel("Coluna",plotlyOutput("bp_entre_empresa_coluna"))),
                          
                          box(selectInput("input_empresa_multiple_bp", "Empresa", choices = dfs$tbl_empresas$regex, selected =TRUE,multiple=TRUE)),
                          box(selectInput("input_contas_unique_bp", "Contas",
                          choices = unique((dfs$tbl_ds_conta%>%filter(nchar<=4)%>%
                          filter(str_tabela=="tbl_bp_ativo" | str_tabela=="tbl_bp_passivo"))$ds_conta),
                          multiple=FALSE))))


# cotacao_entre_empresa= tabItem(tabName="cotacao_entre_empresa",
#                           fluidRow(
#                             box(plotlyOutput("cotacao_entre_empresa")),
#                             box(selectInput("input_tickers_multiple", "Ticker", 
#                                             choices = unique(dfs$tbl_cotacoes$str_ticker), selected =TRUE,multiple=TRUE))))



cotacao_entre_empresa= tabItem(tabName="cotacao_entre_empresa",
                          fluidRow(
                            box(selectInput("input_empresa_multiple_multiplo", "Empresa",
                                            choices =unique(dfs$tbl_empresas$regex), 
                                            selected =TRUE,multiple=TRUE),br(),
                                
                                selectInput("input_multiplo", "Variavel",
                                            choices =c("lucro_liquido_12M",
                                                       "indice_liquidez",
                                                       "margem_liquida",
                                                       "margem_ebit",
                                                       "margem_bruta",
                                                       "giro_ativo",
                                                       "divida_bruta_patrimonio",
                                                       "ROE",
                                                       "ebit_ativo"))),
                            box(selectInput("input_tickers_multiple", "Ticker",
                                            choices =unique(dfs$tbl_cotacoes$str_ticker), 
                                            selected =TRUE,multiple=TRUE))
                            
                            ),
                            fluidRow(
                            tabBox(title="Multiplos entre Empresa",id="tabset4",height="250px",
                                   tabPanel("Linha",plotlyOutput("multiplo_entre_empresa_linha")),
                                   tabPanel("Coluna",plotlyOutput("multiplo_entre_empresa_coluna"))),
                            tabBox(title="Analise entre Empresa",id="tabset4",height="250px",
                                   tabPanel("Cotacao",plotlyOutput("cotacao_entre_empresa_cotacao")),
                                   tabPanel("Valor de Empresa",plotlyOutput("cotacao_entre_empresa_valor")))))
                            


variaveis_macro= tabItem(tabName="variaveis_macro",
                         fluidRow(tabBox(title="Variavel Macro",id="tabset1",height="250px",
                                  tabPanel("Coluna",plotlyOutput("variaveis_macro"))),
                                  box(selectInput("input_variavel_macro", "Variavel Macro", 
                                  choices = c("PMS_RECEITA","abate","juros","dolar",
                                              "inflacao","pib","PIM","PMS_VOLUME",
                                              "PMS_RECEITA")))))
                                        



body=dashboardBody(
  tabItems(
    dre_por_empresa,
    bp_por_empresa,
    dre_entre_empresa,
    bp_entre_empresa,
    cotacao_entre_empresa,
    variaveis_macro
  )
)















