# Adiciona Nome Próprio Validado de \`nomes_proprios_compostos\` .

Adiciona Nome Próprio Validado de \`nomes_proprios_compostos\` .

## Usage

``` r
identificar_adicionar_nome_proprio(dt, s)

add_nome_proprio_to_word1_and_word2p(dt, s)
```

## Arguments

- dt:

  Um \`data.table\`.

- s:

  Nome da coluna (string) base para derivação das colunas de palavras
  (por exemplo, se \`s = "nome_simpl"\`, espera \`nome_simpl1\`,
  \`nome_simpl2p\`).

## Value

O \`data.table\` \`dt\` com colunas \`\_v2\` adicionadas.

## Examples

``` r
# \donttest{
dt_nomes <- data.table::data.table(nome=c("MARIA DO SOCORRO SILVA",
"ANA PAULA DE OLIVEIRA","JOSE DAS FLORES"))
dt_nomes <- identificar_adicionar_nome_proprio(dt_nomes,"nome")
#> Baixando o arquivo de dados nomes_proprios_compostos.rds (aprox. 109MB).
#> Isso vai acontecer apenas uma vez.
#> Salvando em: /home/runner/.cache/R/nomesbr/nomes_proprios_compostos.rds
#> Iniciando download...
#> Download finalizado e arquivo salvo no cache.
print(dt_nomes)
#>     nome1           nome2p     nome_proprio                   nome
#>    <char>           <char>           <char>                 <char>
#> 1:  MARIA MARIA DO SOCORRO MARIA DO SOCORRO MARIA DO SOCORRO SILVA
#> 2:    ANA        ANA PAULA        ANA PAULA  ANA PAULA DE OLIVEIRA
#> 3:   JOSE  JOSE DAS FLORES             JOSE        JOSE DAS FLORES
#>            nome1_v2        nome2p_v2
#>              <char>           <char>
#> 1: MARIA DO SOCORRO MARIA DO SOCORRO
#> 2:        ANA PAULA        ANA PAULA
#> 3:             JOSE             JOSE
# }
```
