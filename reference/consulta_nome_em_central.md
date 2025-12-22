# Consulta Nomes em uma Base de Dados DuckDB

Realiza uma consulta a uma tabela de nomes em um banco de dados DuckDB,
retornando todas as colunas para os nomes que correspondem à lista de
entrada.

## Usage

``` r
consulta_nome_em_central(nomes, mestre, usar_hash = TRUE)
```

## Arguments

- nomes:

  Um vetor de caracteres (character vector) contendo os nomes ou hashes
  a serem consultados.

- mestre:

  Uma string com o caminho para o banco de dados DuckDB (arquivo
  \`.duckdb\`).

- usar_hash:

  Logico. Se `TRUE` (default), a consulta vai ser feita na coluna
  `'nome_original_hash'`. Se `FALSE`, a consulta vai ser feita na coluna
  `'nome_original'`.

## Value

Um `data.frame` contendo os resultados da consulta. Se nenhum nome for
encontrado, retorna um `data.frame` com zero linhas e as colunas da
tabela `nomes_limpos`.

## Details

A função se conecta a um banco de dados DuckDB especificado pelo caminho
em \`mestre\`. A consulta é otimizada para buscar múltiplos nomes de uma
vez, gerando uma instrução SQL com parâmetros para evitar injeção de
SQL.

O parâmetro \`usar_hash\` permite escolher a coluna para a busca:

- Se `TRUE` (padrão), a busca é feita na coluna `'nome_original_hash'`.
  Isso é ideal se os nomes na tabela estão armazenados como hashes (ex:
  SHA-256), pois pode ser mais rápido e seguro para comparações exatas.

- Se `FALSE`, a busca é feita na coluna `'nome_original'`, que deve
  conter os nomes em formato de texto.

A fun\ç\ão gerencia automaticamente a conex\ão com o banco de dados,
garantindo que ela seja fechada ao final da execu\ç\ão, mesmo que ocorra
um erro.

## Examples

``` r
if (FALSE) { # \dontrun{
# Exemplo de uso com hash (padrão)
# Suponha que 'caminho/para/meu_banco.duckdb' existe e tem a tabela 'nomes_limpos'
# com uma coluna 'nome_original_hash'.
hashes_para_buscar <- c("a1b2c3...", "d4e5f6...")
resultados_hash <- consulta_nome_em_central(
  nomes = hashes_para_buscar,
  mestre = "caminho/para/meu_banco.duckdb"
)

# Exemplo de uso com texto
# Suponha que a tabela 'nomes_limpos' também tem uma coluna 'nome_original'.
nomes_para_buscar <- c("João da Silva", "Maria Oliveira")
resultados_texto <- consulta_nome_em_central(
  nomes = nomes_para_buscar,
  mestre = "caminho/para/meu_banco.duckdb",
  usar_hash = FALSE
)
} # }
```
