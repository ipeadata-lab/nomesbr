# Limpa e Analisa Nomes em um data.table

Processa uma coluna de nomes em um \`data.table\`, aplicando uma série
de regras de limpeza para identificar e corrigir/marcar problemas comuns
como menções a "FALECIDO", "CARTORIO", erros de digitação, espaços
indevidos, etc.

## Usage

``` r
limpar_nomes(d, s)

find_and_clean_NAnames_and_extra_spaces(d, s)
```

## Arguments

- d:

  Um objeto \`data.table\`.

- s:

  O nome da coluna (em string) dentro de \`d\` que contém os nomes a
  serem processados.

## Value

Um \`data.table\` modificado, contendo a coluna original, uma nova
coluna com sufixo "\_clean" com os nomes limpos, e colunas booleanas
indicando a detecção de cada tipo de problema (ex: \`falecido\`,
\`cartorio\`).

## Details

A função executa os seguintes passos principais:

1.  Cria uma cópia da coluna de nomes para limpeza.

2.  Detecta e trata menções a "FALECIDO(A)".

3.  Detecta e trata menções a "CARTORIO" e nomes de cidades comuns em
    registros.

4.  Corrige espaçamento perto de caracteres especiais com
    \`limpa_espaco_acento_til_apostrofe\`.

5.  Identifica e trata nomes contendo termos problemáticos como "PAI",
    "MAE", "SEM", "NAO", exceto em contextos aceitáveis.

6.  Identifica e trata casos de "NADA CONSTA" e variações.

7.  Corrige E, DA, DE e variantes com caracter prévio indevido (ex:
    "EDAS" para "DAS" se aplicável).

8.  Remove saudações como "SR.", "SRA.".

9.  Remove termos como "IGNORADO", "DESCONHECIDO".

10. Remove repetições de partículas de ligação (ex: "DE DE").

11. Limpa letras repetidas no início ou meio de palavras.

## Examples

``` r
# Supondo que 'meu_DT' é um data.table com uma coluna 'nome_sujo'
DT_exemplo <- data.table::data.table(
id = 1:3,
nome_sujo = c("MARIA FALECIDA SSILVA", "CARTORIO DE PAZ", "JOAO D ARC")
)
DT_limpo <- limpar_nomes(DT_exemplo, "nome_sujo")
#> 0. Making copy of dataset and add the s2(the var to be cleaned): 0.001 sec elapsed
#> 1. Detect and clean "FALECIDO/A" (and variants): 0.017 sec elapsed
#> 2. Detect and clean "CARTORIO" cases: 0.002 sec elapsed
#> 3. Identify cases with extra spaces before accented letters, tilde and apostrophe: 0.005 sec elapsed
#> 4. Detect: PAI MAE SEM NAO and fix: 0.003 sec elapsed
#> 5. Detect "NADA_NAO" and "CONSTA" cases, and make their combo NA: 0.005 sec elapsed
#> 6. Detect Xartigo and replace when unambigous (ex: DDE to DE): 0.001 sec elapsed
#> 7. Detect and clean SR_SRA and variants: 0.001 sec elapsed
#> 8. Detect and clean desconhecido ignorado and variants: 0.001 sec elapsed
#> 9. Detect and clean repeated de de da da do do : 0.003 sec elapsed
#> 10. Detect and clean repeated letters with specific rules whether at beginning or middle of the word: 0.003 sec elapsed
#> 11. Force NA if resulting cleaned name is empty or spaces: 0.001 sec elapsed
#> All substeps: 0.044 sec elapsed
print(DT_limpo)
#> Index: <espaco_TilAcentoApostrofe>
#>       id             nome_sujo      nome_sujo_clean falecido cartorio
#>    <int>                <char>               <char>   <lgcl>   <lgcl>
#> 1:     1 MARIA FALECIDA SSILVA MARIA FALECIDA SILVA     TRUE    FALSE
#> 2:     2       CARTORIO DE PAZ                 <NA>    FALSE     TRUE
#> 3:     3            JOAO D ARC            JOAO DARC    FALSE    FALSE
#>    espaco_TilAcentoApostrofe nome_P_M_S_N nada_nao consta nada_nao_consta
#>                       <lgcl>       <lgcl>   <lgcl> <lgcl>          <lgcl>
#> 1:                     FALSE        FALSE    FALSE  FALSE              NA
#> 2:                        NA           NA       NA     NA              NA
#> 3:                      TRUE        FALSE    FALSE  FALSE              NA
#>    nada_nao_consta2 Xartigo sr_sra ignorado dede_dada letra_repetida
#>              <lgcl>  <lgcl> <lgcl>   <lgcl>    <lgcl>         <lgcl>
#> 1:               NA   FALSE  FALSE    FALSE     FALSE           TRUE
#> 2:               NA      NA     NA       NA     FALSE             NA
#> 3:               NA   FALSE  FALSE    FALSE     FALSE          FALSE
```
