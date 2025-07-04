% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_simplificacao_nomes.R
\name{segmentar_nomes}
\alias{segmentar_nomes}
\alias{add_string_w1_w2_w3_and_w2p}
\title{Adiciona Colunas com Partes do Nome (w1, w2, w3, w2p, w12p)}
\usage{
segmentar_nomes(dt, s)

add_string_w1_w2_w3_and_w2p(dt, s)
}
\arguments{
\item{dt}{Um `data.table`.}

\item{s}{Nome da coluna (string) em `dt` contendo os nomes.}
}
\value{
O `data.table` `dt` modificado por referência, com novas colunas.
}
\description{
Adiciona Colunas com Partes do Nome (w1, w2, w3, w2p, w12p)
}
\examples{
dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA","ANA PAULA DE OLIVEIRA"))
dt_nomes <- segmentar_nomes(dt_nomes,"nome")
print(dt_nomes)

}
