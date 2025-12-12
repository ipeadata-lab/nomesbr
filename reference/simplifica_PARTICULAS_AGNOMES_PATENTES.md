# Cria coluna com agnomes, algumas patentes/cargos as remove, remove partículas

Cria coluna com agnomes, algumas patentes/cargos as remove, remove
partículas

## Usage

``` r
simplifica_PARTICULAS_AGNOMES_PATENTES(d, s = "nome_clean")
```

## Arguments

- d:

  um objeto \`data.table\`

- s:

  string com nome da coluna de caracteres contendo nomes para
  simplificar. Por padrão, "nome_clean".

## Value

data.table com novas colunas de nome simplificado e de marca
agnomes_titulos

## Examples

``` r
dt_nomes <- data.table::data.table(nome = c("JOAO DA SILVA FILHO",
"CORONEL JACINTO"))
dt_nomes <- simplifica_PARTICULAS_AGNOMES_PATENTES(d=dt_nomes,s="nome")
#> 0. Making copy of dataset and add the s2(the var to be cleaned): 0.001 sec elapsed
#> 1. remove particles e da de do(s): 0.001 sec elapsed
#> 2. Detect and remove AGNOMES and titles: 0.002 sec elapsed
#> Starting - All substeps: 0.005 sec elapsed
print(dt_nomes)
#>                   nome  nome_simp agnomes_titulos
#>                 <char>     <char>          <char>
#> 1: JOAO DA SILVA FILHO JOAO SILVA           FILHO
#> 2:     CORONEL JACINTO    JACINTO         CORONEL
```
