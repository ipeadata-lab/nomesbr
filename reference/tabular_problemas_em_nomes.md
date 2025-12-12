# Tabula Problemas Detectados nos Nomes

Cria uma tabela resumo contabilizando o número de ocorrências para cada
tipo de problema detectado pela função
\`marcar_problemas_e_limpar_nomes\`.

## Usage

``` r
tabular_problemas_em_nomes(d, s)

tabulate_name_poblems(d, s)
```

## Arguments

- d:

  O \`data.table\` retornado por \`marcar_problemas_e_limpar_nomes\`.

- s:

  O nome da coluna original (string) que foi processada.

## Value

Um \`data.table\` com as colunas:

- \`condition\`: O nome da condição/problema verificado.

- \`N_detected\`: Número de vezes que a condição foi detectada.

- \`N_made_NA\`: Número de detecções que resultaram na limpeza para
  \`NA\`.

- \`N_replaced\`: Número de detecções onde o nome foi alterado (não para
  \`NA\`).

## Examples

``` r
DT_limpo <- data.table::data.table(nome = c("JOSEE SILVA", 
"RAIMUNDA DA DA SILVA"), nome_clean = c("JOSE SILVA",
"RAIMUNDA DA SILVA"),
falecido = NA, cartorio = NA, 
espaco_TilAcentoApostrofe = NA, 
nome_P_M_S_N = NA, nada_nao = NA, 
nada_nao_consta2 = NA, final_missing = NA, Xartigo = NA, sr_sra = NA,
ignorado = NA, dededada = 1, letra_repetida = 1)
sumario <- tabular_problemas_em_nomes(DT_limpo, "nome")
print(sumario)
#>                     condition N_detected N_made_NA N_replaced
#>                        <char>      <int>     <int>      <int>
#>  1:                  falecido          0         0          0
#>  2:                  cartorio          0         0          0
#>  3: espaco_TilAcentoApostrofe          0         0          0
#>  4:              nome_P_M_S_N          0         0          0
#>  5:                  nada_nao          0         0          0
#>  6:                    consta          0         0          0
#>  7:           nada_nao_consta          0         0          0
#>  8:          nada_nao_consta2          0         0          0
#>  9:             final_missing          0         0          0
#> 10:                   Xartigo          0         0          0
#> 11:                    sr_sra          0         0          0
#> 12:                  ignorado          0         0          0
#> 13:                 dede_dada          0         0          0
#> 14:            letra_repetida          2         0          2
```
