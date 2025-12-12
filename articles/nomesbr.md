# nomesbr

## nomesbr

**nomesbr** é um pacote de R que limpa e simplifica nomes de pessoas
para auxiliar no pareamento de banco de dados na ausência de chaves
únicas não ambíguas. Detecta e corrige erros tipográficos mais comuns,
simplifica opcionalmente termos sujeitos eventualmente a omissão em
cadastros, e simplifica .

(R package for tidying and simplifying. Created for aiding in dataset
pairing in the absence of unambiguous keys. It detects and corrects
common typos, optionally simplifies terms prone to omission in records.)

``` r
library(nomesbr)
```

## Utilização

O pacote **nomesbr** torna disponíveis funções para limpar e simplificar
nomes.
[`limpar_nomes()`](https://ipeadata-lab.github.io/nomesbr/reference/limpar_nomes.md)
e `simplifica_PARTICULAS_AGNOMES_PATENTES`, principais funções do
pacote, foram criadas para seu uso em sequência nessa ordem.

[`limpar_nomes()`](https://ipeadata-lab.github.io/nomesbr/reference/limpar_nomes.md)
recebe como parâmetros d,um data.frame, e s, nome da coluna com os nomes
a processar. A função cria uma nova coluna, com sufixo ’\_clean’, e gera
novas colunas com informações dos tipos de limpeza detectados como
necessários e realizados.

[`simplifica_PARTICULAS_AGNOMES_PATENTES()`](https://ipeadata-lab.github.io/nomesbr/reference/simplifica_PARTICULAS_AGNOMES_PATENTES.md)
recebe também, d e s (por padrão para s, ‘nome_clean’) como parâmetros,
e simplifica partículas repetidas, agnomes e alguma patentes.

A informação (novas colunas) gerada pela função
[`limpar_nomes()`](https://ipeadata-lab.github.io/nomesbr/reference/limpar_nomes.md)
servem como base para a função
[`tabular_problemas_em_nomes()`](https://ipeadata-lab.github.io/nomesbr/reference/tabular_problemas_em_nomes.md)
, que retorna uma tabela resumo dos problemas detectados e ações tomadas
no sentido da limpeza de nomes.

## Nota [![Ipea](reference/figures/ipea_logo.png)](https://www.ipea.gov.br)

**nomesbr** é desenvolvido por uma equipe de pesquisadores do Instituto
de Pesquisa Econômica Aplicada (Ipea).
