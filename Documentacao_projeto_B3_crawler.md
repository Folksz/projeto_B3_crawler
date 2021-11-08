<!-- toc -->

novembro 07, 2021

# DESCRIPTION

```
Package: b3crawler
Title: Crawler B3
Version: 0.0.0.9000
Authors@R: 
    person(given = "Pedro",
           family = "Cavalcante",
           role = c("aut","cre"),
           email = "pedrohen.fc@gmail.com")
Description: Crawler dados da B3.
License: MIT + file LICENSE
Encoding: UTF-8
LazyData: true
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.1.1
Imports: 
    stringr,
    dplyr,
    rvest,
    xml2,
    BatchGetSymbols,
    xlsx,
    zoo,
    stringi,
    sidrar,
    ipeadatar,
    utils,
    lubridate,
    plotly,
    reshape2,
    shinydashboard,
    shiny
    ```


# `buscar_num_acoes`

CRAWLER NUMERO DE ACOES EMPRESA


## Description

Realiza um crawler no site fundamentus para capturar o numero de acoes que uma empresa listada em bolsa possui


## Usage

```r
buscar_num_acoes(tickers)
```


## Arguments

Argument      |Description
------------- |----------------
`tickers`     |     texto com o ticker da empresa


## Value

retorna o numero de acoes da empresa


## Author

Pedro Cavalcante


## Examples

```r
buscar_num_acoes("PETR3")
```


# `construir_bd`

Atualiza banco de dados


## Description

Funcao principal que atualiza o banco de dados
 
 Funcao principal que constroi todo o banco de dados


## Usage

```r
atualizar_ponta(diretorio = "output/", gerar_cotacao = TRUE)
construir_bd(anos = seq(2011, 2021), diretorio = "", gerar_cotacao = TRUE)
```


## Arguments

Argument      |Description
------------- |----------------
`diretorio`     |     Local em que as tabelas serao geradas em csv
`gerar_cotacao`     |     Booleana que indica se tambem deseja gerar a cotacao desses tickers
`anos`     |     Quais anos dos dados deseja, o numero tem que ser maior ou igual a 2011


## Details

Funcao principal que atualiza o banco de dados,
 
 Funcao principal que constroi todo o banco de dados, demora um pouco por realiza o download de varios arquivos


## Value

Retorna uma lista com as tabelas (que tambem sao salvas em um csv)
 
 Retorna uma lista com as tabelas (que tambem sao salvas em um csv)


## Author

Pedro Cavalcante
 
 Pedro Cavalcante


## Examples

```r
atualizar_ponta("",TRUE)
construir_bd(c(2021),"",TRUE)
```


# `construir_bp_ativo`

CONSTRUIR BALANCO PATRIMONIAL ATIVO


## Description

Constroi o balanço patrimonial ativo apos os dados terem sido devidamente capturados


## Usage

```r
construir_bp_ativo(bulk)
```


## Arguments

Argument      |Description
------------- |----------------
`bulk`     |     lista com os dados brutos


## Value

balanco patrimonial ativo


## Author

Pedro Cavalcante


# `construir_bp_passivo`

CONSTRUIR BALANCO PATRIMONIAL PASSIVO


## Description

Constroi o balanço patrimonial passivo apos os dados terem sido devidamente capturados


## Usage

```r
construir_bp_passivo(bulk)
```


## Arguments

Argument      |Description
------------- |----------------
`bulk`     |     lista com os dados brutos


## Value

balanco patrimonial passivo


## Author

Pedro Cavalcante


# `construir_depara`

CONSTRUIR DEPARA


## Description

Realiza o depara entre diferentes fontes de dados (B3, CVM e Fundamentus)


## Usage

```r
rm_accent(str, pattern = "all")
```


## Arguments

Argument      |Description
------------- |----------------
`bulk`     |     lista com os dados brutos


## Value

lista com a tabela de tickers disponiveis e o nome das empresas


## Author

Pedro Cavalcante


# `construir_DRE`

CONSTRUIR DRE


## Description

Constroi o demonstrativo de resultados apos os dados terem sido devidamente capturados


## Usage

```r
construir_DRE(bulk)
```


## Arguments

Argument      |Description
------------- |----------------
`bulk`     |     lista com os dados brutos


## Value

demonstrativo de resultados


## Author

Pedro Cavalcante


# `construir_metricas`

Criar Metricas


## Description

Constroi metricas a partir de tbl_dre, tbl_bp


## Usage

```r
construir_metricas(tbl_dre, tbl_bp, empresas_selec)
```


## Arguments

Argument      |Description
------------- |----------------
`tbl_dre`     |     Tabela de Demonstrativo de Resultados
`tbl_bp`     |     Tabela de Balanco Patrimonial
`empresas_selec`     |     Selecionar Empresas para construir metricas


## Value

retorna df com as informacoes das metricas


## Author

Pedro Cavalcante


# `criar_painel`

Criar Painel


## Description

Constroi painel em Rshiny


## Usage

```r
server(input, output)
```


## Value




## Author

Pedro Cavalcante


# `get_acoes`

CRAWLER ACOES


## Description

captura preco das acoes a partir da library BatchGetSymbols


## Usage

```r
get_acoes(tickers, data_inicio = as.Date("2000-01-01"), data_fim = Sys.Date())
```


## Arguments

Argument      |Description
------------- |----------------
`tickers`     |     vetor com tickers das empresas
`data_inicio`     |     a partir de quando capturar os dados da bolsas
`data_fim`     |     ate quando capturar os dados da bolsas


## Value

dataframe com as acoes da bolsa


## Author

Pedro Cavalcante


## Examples

```r
get_acoes("PETR3","2000-01-01","2021-01-01")
```


# `importar_sidra`

API SIDRA


## Description

Variavel que se utiliza da API do ipeadatar para capturar alguns dados relevantes à analise
 
 Variavel que se utiliza da API do sidra para capturar alguns dados relevantes à analise


## Usage

```r
importar_sgs(info)
importar_sidra(info)
```


## Arguments

Argument      |Description
------------- |----------------
`info`     |     Variavel de busca que pode ser "inflacao","pib","abate","PIM","PMS_VOLUME","PMS_RECEITA"#'


## Value

dataframe com a data e com a varavel requisitada
 
 dataframe com a data e com a varavel requisitada


## Author

Pedro Cavalcante
 
 Pedro Cavalcante


## Examples

```r
importar_sgs("juros")
importar_sidra("inflacao")
```


# `ticker_empresa`

DEPARA SETOR NOME EMPRESA


## Description

A partir de um crawler oriundo do site da b3, cria um depara entre a empresa e setor atuante
 
 A partir de um crawler oriundo do site fundamentus, cria um depara entre o ticker da empresa listada
 na bolsa e a sau razao social


## Usage

```r
buscar_setores()
ticker_empresa()
```


## Value

dataframe com nome da empresa e seus setores
 
 dataframe com nome do ticker, nome da companhia e nome que esta na razao social


## Author

Pedro Cavalcante
 
 Pedro Cavalcante


## Examples

```r
buscar_setores()
```


