#' Consulta Nomes em uma Base de Dados DuckDB
#'
#' Realiza uma consulta a uma tabela de nomes em um banco de dados DuckDB,
#' retornando todas as colunas para os nomes que correspondem à lista de
#' entrada.
#'
#' @details A função se conecta a um banco de dados DuckDB especificado pelo
#'   caminho em `mestre`. A consulta é otimizada para buscar múltiplos nomes
#'   de uma vez, gerando uma instrução SQL com parâmetros para evitar
#'   injeção de SQL.
#'
#'   O parâmetro `usar_hash` permite escolher a coluna para a busca:
#'   \itemize{
#'     \item{Se \code{TRUE} (padrão), a busca é feita na coluna
#'     \code{'nome_original_hash'}. Isso é ideal se os nomes na tabela
#'     estão armazenados como hashes (ex: SHA-256), pois pode ser mais rápido
#'     e seguro para comparações exatas.}
#'     \item{Se \code{FALSE}, a busca é feita na coluna \code{'nome_original'},
#'     que deve conter os nomes em formato de texto.}
#'   }
#'
#'   A função gerencia automaticamente a conexão com o banco de dados,
#'   garantindo que ela seja fechada ao final da execução, mesmo que ocorra
#'   um erro.
#'
#' @param nomes Um vetor de caracteres (character vector) contendo os nomes
#'   ou hashes a serem consultados.
#' @param mestre Uma string com o caminho para o diretório do banco de dados
#'   DuckDB (arquivo `.duckdb`).
#' @param usar_hash Lógico. Se \code{TRUE} (padrão), a consulta será feita na
#'   coluna \code{'nome_original_hash'}. Se \code{FALSE}, a consulta será
#'   feita na coluna \code{'nome_original'}.
#'
#' @return Um \code{data.frame} contendo os resultados da consulta. Se nenhum
#'   nome for encontrado, retorna um \code{data.frame} com zero linhas e as
#'   colunas da tabela \code{nomes_limpos}.
#'
#' @examples
#' \dontrun{
#' # Exemplo de uso com hash (padrão)
#' # Suponha que 'caminho/para/meu_banco.duckdb' existe e tem a tabela 'nomes_limpos'
#' # com uma coluna 'nome_original_hash'.
#' hashes_para_buscar <- c("a1b2c3...", "d4e5f6...")
#' resultados_hash <- consulta_nome_em_central(
#'   nomes = hashes_para_buscar,
#'   mestre = "caminho/para/meu_banco.duckdb"
#' )
#'
#' # Exemplo de uso com texto
#' # Suponha que a tabela 'nomes_limpos' também tem uma coluna 'nome_original'.
#' nomes_para_buscar <- c("João da Silva", "Maria Oliveira")
#' resultados_texto <- consulta_nome_em_central(
#'   nomes = nomes_para_buscar,
#'   mestre = "caminho/para/meu_banco.duckdb",
#'   usar_hash = FALSE
#' )
#' }
#'
#' @export

consulta_nome_em_central <- 
  \(nomes, 
    mestre ,
    usar_hash = TRUE) {
    
    # --- INÍCIO DA VERIFICAÇÃO DE DEPENDÊNCIA ---
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop(
        "O pacote 'duckdb' é necessário para esta função, mas não está instalado. ",
        "Por favor, instale-o com: install.packages('duckdb')",
        call. = FALSE # Evita mostrar a chamada da função na mensagem de erro
      )
    }
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop(
        "O pacote 'DBI' é necessário para esta função, mas não está instalado. ",
        "Por favor, instale-o com: install.packages('DBI')",
        call. = FALSE # Evita mostrar a chamada da função na mensagem de erro
      )
    }
    # --- FIM DA VERIFICAÇÃO ---
    
    # Conecta ao banco de dados DuckDB
    conexao <- duckdb::dbConnect(duckdb::duckdb(), dbdir = mestre)
    
    # Garante que a conexão será fechada ao sair da função, mesmo com erros
    on.exit(DBI::dbDisconnect(conexao, shutdown = TRUE))
    
    # Define a coluna de busca com base no parâmetro usar_hash
    if (usar_hash) {
      coluna <- 'nome_original_hash'
    } else {
      coluna <- 'nome_original'
    }
    
    # Função interna para construir e executar a consulta SQL
    consulta1 <- \(w) { 
      # Cria a lista de placeholders (?, ?, ...) para a cláusula IN
      elementos <- paste(rep("?", length(w)), collapse = ", ")
      
      # Monta a string da consulta SQL de forma segura
      consulta <- 
        paste0(
          "SELECT * from nomes_limpos   
          WHERE ", coluna, " IN (",
          elementos, ")"
        )
      
      # Executa a consulta passando os parâmetros de forma segura
      a <- DBI::dbGetQuery(conexao,
                           consulta,
                           params = as.list(w))
      
      return(a)
    }
    
    
    encontrado <- consulta1(nomes)
    
    
    return(encontrado)
  }