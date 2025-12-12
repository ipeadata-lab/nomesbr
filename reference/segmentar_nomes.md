# Adiciona Colunas com Partes do Nome (w1, w2, w3, w2p, w12p)

Adiciona Colunas com Partes do Nome (w1, w2, w3, w2p, w12p)

## Usage

``` r
segmentar_nomes(dt, s)

add_string_w1_w2_w3_and_w2p(dt, s)
```

## Arguments

- dt:

  Um \`data.table\`.

- s:

  Nome da coluna (string) em \`dt\` contendo os nomes.

## Value

O \`data.table\` \`dt\` modificado por referência, com novas colunas.

## Examples

``` r
dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA",
"ANA PAULA DE OLIVEIRA"))
dt_nomes <- segmentar_nomes(dt_nomes,"nome")
print(dt_nomes)
#>                      nome nome_w1 nome_w2 nome_w3   nome_w2p        nome_w12p
#>                    <char>  <char>  <char>  <char>     <char>           <char>
#> 1: MARIA DO SOCORRO SILVA   MARIA      DO SOCORRO DO SOCORRO MARIA DO SOCORRO
#> 2:  ANA PAULA DE OLIVEIRA     ANA   PAULA      DE      PAULA        ANA PAULA
```
