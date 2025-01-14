% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acoes.R
\name{get_acoes}
\alias{get_acoes}
\title{CRAWLER ACOES}
\usage{
get_acoes(tickers, data_inicio = as.Date("2000-01-01"), data_fim = Sys.Date())
}
\arguments{
\item{tickers}{vetor com tickers das empresas}

\item{data_inicio}{a partir de quando capturar os dados da bolsas}

\item{data_fim}{ate quando capturar os dados da bolsas}
}
\value{
dataframe com as acoes da bolsa
}
\description{
captura preco das acoes a partir da library BatchGetSymbols
}
\examples{
get_acoes("PETR3","2000-01-01","2021-01-01")
}
\author{
Pedro Cavalcante
}
