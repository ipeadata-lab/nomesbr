# Remove Partículas, Agnomes e algumas Patentes de Nomes

Remove Partículas, Agnomes e algumas Patentes de Nomes

## Usage

``` r
remove_PARTICULAS_AGNOMES(s)
```

## Arguments

- s:

  Vetor de caracteres contendo nomes.

## Value

Vetor de caracteres com nomes simplificados.

## Examples

``` r
vct_nomes <- c("JOAO DA SILVA FILHO","CORONEL JACINTO")
remove_PARTICULAS_AGNOMES(vct_nomes)
#> [1] "JOAO SILVA" " JACINTO"  
```
