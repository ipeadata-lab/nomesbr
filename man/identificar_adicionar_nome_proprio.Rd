% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_simplificacao_nomes.R
\name{identificar_adicionar_nome_proprio}
\alias{identificar_adicionar_nome_proprio}
\alias{add_nome_proprio_to_word1_and_word2p}
\title{Adiciona Nome Próprio Validado com Base em `nomes_proprios_compostos` `np2`}
\usage{
identificar_adicionar_nome_proprio(dt, s)

add_nome_proprio_to_word1_and_word2p(dt, s)
}
\arguments{
\item{dt}{Um `data.table`.}

\item{s}{Nome da coluna (string) base para derivação das colunas de palavras (ex: se `s = "nome_simpl"`, espera `nome_simpl1`, `nome_simpl2p`).}
}
\value{
O `data.table` `dt` com colunas `_v2` adicionadas.
}
\description{
Adiciona Nome Próprio Validado com Base em `nomes_proprios_compostos` `np2`
}
\examples{
\dontrun{
dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA",
"ANA PAULA DE OLIVEIRA","JOSE DAS FLORES"))
dt_nomes <- identificar_adicionar_nome_proprio(dt_nomes,"nome")
print(dt_nomes)
}
}
