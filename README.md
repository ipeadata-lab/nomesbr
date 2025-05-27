
## nomesbr

**nomesbr** é um pacote de R que limpa e simplifica nomes de pessoas
para auxiliar no pareamento de banco de dados na ausência de chaves
únicas não ambíguas. Detecta e corrige erros tipográficos mais comuns,
simplifica opcionalmente termos sujeitos eventualmente a omissão em
cadastros, e simplifica foneticamente suas palavras, aplicando variação
própria do algoritmo metaphoneBR.

R package for tidying and simplifying phonetically brazilian names using
a custom metaphoneBR algorithm. Created for aiding in dataset pairing in
the absence of unambiguous keys. It detects and corrects common typos,
optionally simplifies terms prone to omission in records, and applies
phonetic simplification using a custom variation of the metaphoneBR
algorithm.)

## Instalação

A versão em desenvolvimento pode ser instalada com o seguinte comando :

``` r
# install.packages("remotes")
remotes::install_github("ipeadata-lab/nomesbr")
```

## Utilização

## Nota <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="Ipea" align="right" width="300"/></a>

**nomesbr** é desenvolvido por uma equipe de pesquisadores do Instituto
de Pesquisa Econômica Aplicada (Ipea).
