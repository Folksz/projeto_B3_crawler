% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atualizar_ponta.R, R/construir_bd.R
\name{construir_bd}
\alias{construir_bd}
\alias{atualizar_ponta}
\title{Atualiza banco de dados}
\usage{
atualizar_ponta(diretorio = "output/", gerar_cotacao = TRUE)

construir_bd(anos = seq(2011, 2021), diretorio = "", gerar_cotacao = TRUE)
}
\arguments{
\item{diretorio}{Local em que as tabelas serao geradas em csv}

\item{gerar_cotacao}{Booleana que indica se tambem deseja gerar a cotacao desses tickers}

\item{anos}{Quais anos dos dados deseja, o numero tem que ser maior ou igual a 2011}
}
\value{
Retorna uma lista com as tabelas (que tambem sao salvas em um csv)

Retorna uma lista com as tabelas (que tambem sao salvas em um csv)
}
\description{
Funcao principal que atualiza o banco de dados

Funcao principal que constroi todo o banco de dados
}
\details{
Funcao principal que atualiza o banco de dados,

Funcao principal que constroi todo o banco de dados,
demora um pouco por realiza o download de varios arquivos
}
\examples{
atualizar_ponta("",TRUE)


construir_bd(c(2021),"",TRUE)
}
\author{
Pedro Cavalcante

Pedro Cavalcante
}
