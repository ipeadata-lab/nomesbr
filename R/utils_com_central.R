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
#'   A fun\ç\ão gerencia automaticamente a conex\ão com o banco de dados,
#'   garantindo que ela seja fechada ao final da execu\ç\ão, mesmo que ocorra
#'   um erro.
#'
#' @param nomes Um vetor de caracteres (character vector) contendo os nomes
#'   ou hashes a serem consultados.
#' @param mestre Uma string com o caminho para o banco de dados
#'   DuckDB (arquivo `.duckdb`).
#' @param usar_hash Logico. Se \code{TRUE} (default), a consulta vai ser feita na
#'   coluna \code{'nome_original_hash'}. Se \code{FALSE}, a consulta vai ser
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
    
    
    if ( length(nomes)==0) {
      return(data.frame())
    }
    # --- IN\u00cdCIO DA VERIFICA\u00c7\u00c3O DE DEPEND\u00caNCIA ---
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop(
        "O pacote 'duckdb' \u00e9 necess\u00e1rio para esta fun\u00e7\u00e3o, mas n\u00e3o est\u00e1 instalado. ",
        "Por favor, instale-o com: install.packages('duckdb')",
        call. = FALSE # Evita mostrar a chamada da fun\u00e7\u00e3o na mensagem de erro
      )
    }
    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop(
        "O pacote 'DBI' \u00e9 necess\u00e1rio para esta fun\u00e7\u00e3o, mas n\u00e3o est\u00e1 instalado. ",
        "Por favor, instale-o com: install.packages('DBI')",
        call. = FALSE # Evita mostrar a chamada da fun\u00e7\u00e3o na mensagem de erro
      )
    }
    # --- FIM DA VERIFICA\u00c7\u00c3O ---
    
    # Conecta ao banco de dados DuckDB
    conexao <- duckdb::dbConnect(duckdb::duckdb(), dbdir = mestre)
    
    # Garante que a conex\u00e3o ser\u00e1 fechada ao sair da fun\u00e7\u00e3o, mesmo com erros
    on.exit(DBI::dbDisconnect(conexao, shutdown = TRUE))
    
    # Define a coluna de busca com base no par\u00e2metro usar_hash
    if (usar_hash) {
      coluna <- 'nome_original_hash'
    } else {
      coluna <- 'nome_original'
    }
    
    # Fun\u00e7\u00e3o interna para construir e executar a consulta SQL
    consulta1 <- \(w) { 
      # Cria a lista de placeholders (?, ?, ...) para a cl\u00e1usula IN
      elementos <- paste(rep("?", length(w)), collapse = ", ")
      
      # Monta a string da consulta SQL de forma segura
      consulta <- 
        paste0(
          "SELECT * from nomes_limpos   
          WHERE ", coluna, " IN (",
          elementos, ")"
        )
      
      # Executa a consulta passando os par\u00e2metros de forma segura
      a <- DBI::dbGetQuery(conexao,
                           consulta,
                           params = as.list(w))
      
      return(a)
    }
    
    
    encontrado <- consulta1(nomes)
    
    
    return(encontrado)
  }




#' Calcular similaridade entre nomes
#'
#' Esta função calcula a similaridade entre dois nomes utilizando uma combinação
#' ponderada de algoritmos de distância de strings (Jaro-Winkler e Damerau-Levenshtein)
#' após pré-processamento fonético.
#'
#' @param nome1 Primeiro nome para comparação (character)
#' @param nome2 Segundo nome para comparação (character)
#'
#' @return Um valor numérico entre 0 e 1 representando a similaridade entre os nomes
#'
#' @details
#' A função realiza os seguintes passos:
#' \itemize{
#'   \item Limpeza dos nomes usando nomesbr::limpar_nomes
#'   \item Codificação fonética usando metaphonebr::metaphonebr
#'   \item Cálculo da similaridade usando Jaro-Winkler (peso 0.7) e Damerau-Levenshtein (peso 0.3)
#' }
#'
#' @examples
#' calcular_similaridade_nomes("Maria", "Mary")
#' calcular_similaridade_nomes("José", "Jose")
#'
#' @import stringdist
#' @import nomesbr
#' @import data.table
#' @importFrom dplyr arrange desc
#' 
#' @export
#' 


calcular_similaridade_nomes <- \(nome1, nome2) {
  
  nome1_clean <- nomesbr::limpar_nomes(data.table::data.table(nome=nome1),'nome')$nome_clean
  
  nome2_clean <-nomesbr::limpar_nomes(data.table::data.table(nome=nome2),'nome')$nome_clean
  # Verifica se o pacote metaphonebr está disponível
  if (requireNamespace("metaphonebr", quietly = TRUE)) {
    nome1_clean <- metaphonebr::metaphonebr(nome1_clean,verbose = F)
    nome2_clean <- metaphonebr::metaphonebr(nome2_clean,verbose = F)
  # Pré-processamento com seu pacote
  
  
  } else {
    print("Pacote metaphonebr inexistente localmente, aplicando apenas limpeza de nomesbr")
  }
  
  #print(paste('nomes limpos',nome1_clean,'comparado com',nome2_clean))
  
  # Combinação ponderada de distâncias
  jarowinkler <- stringdist::stringsim(nome1_clean, nome2_clean, method = "jw")
  
  damerau_levenshtein <- stringdist::stringsim(nome1_clean, nome2_clean, method = "dl")
  
  
  
  # Peso maior para Jaro-Winkler (melhor para nomes)
  similaridade <- 0.7 * jarowinkler + 0.3 * damerau_levenshtein
  
  
  
  return(similaridade) 
  
  
}


#' Sugerir correções para um nome alvo
#'
#' Esta função sugere correções para um nome alvo com base em uma lista de nomes candidatos,
#' utilizando um limiar adaptativo baseado no comprimento do nome.
#'
#' @param nome_alvo Nome para o qual se buscam correções (character)
#' @param lista_nomes Vetor de nomes candidatos (character vector)
#' @param threshold_adaptativo Lógico indicando se deve usar limiar adaptativo (default = TRUE)
#'
#' @return Um data.frame com colunas 'sugestao' e 'similaridade' contendo as sugestões
#'         que superaram o limiar mínimo
#'
#' @details
#' O limiar adaptativo funciona da seguinte forma:
#' \itemize{
#'   \item Nomes com até 5 caracteres: limiar de 0.85
#'   \item Nomes entre 6 e 10 caracteres: limiar de 0.80
#'   \item Nomes com mais de 10 caracteres: limiar de 0.75
#' }
#'
#' @examples
#' sugerir_correcao_nomes("Jão", c("João", "Jonas", "Juan", "Joaquim"))
#' sugerir_correcao_nomes("Ana", c("Anna", "Hana", "Ana Paula"), threshold_adaptativo = FALSE)
#'
#' @export
sugerir_correcao_nomes <- \(nome_alvo, lista_nomes, threshold_adaptativo = TRUE) {
  
  
  
  
  similaridades <- sapply(lista_nomes, \(x) {
    
    calcular_similaridade_nomes(nome_alvo, x)
    
  })
  
  
  
  if (threshold_adaptativo) {
    
    # Threshold baseado no comprimento do nome
    nome_alvo <- data.table::data.table("nome"=nome_alvo)
    comprimento <- nchar(nomesbr::limpar_nomes(nome_alvo,"nome"))
    
    threshold <- ifelse(comprimento <= 5, 0.85, 
                        
                        ifelse(comprimento <= 10, 0.80, 0.75))
    
  } else {
    
    threshold <- 0.85 # Default conservador
    
  }
  
  
  
  sugestoes <- lista_nomes[similaridades >= threshold]
  
  scores <- similaridades[similaridades >= threshold]
  
  
  
  return(
    data.table::setorder(
      data.table::data.table(sugestao = sugestoes, similaridade = scores),
      -similaridade)
  )
  
}



#' Busca otimizada de nomes similares com uso de índice reverso
#' Realiza uma busca eficiente por nomes similares em uma grande base de dados,
#' utilizando uma abordagem de duas etapas: primeiro seleciona candidatos via
#' índice invertido no DuckDB e, em seguida, refina os resultados usando
#' cálculos de distância de strings fonéticas.
#'
#' @param nome Character. O nome (ou parte de nome) para o qual se deseja encontrar
#'   similares.
#' @param n_candidatos Integer. O número máximo de candidatos a serem recuperados
#'   do índice invertido na primeira etapa. Padrão é 2000. Aumentar este valor
#'   pode melhorar a precisão (encontrando nomes mais raros), mas diminui a performance.
#' @param limite_similaridade Numeric (0.0 a 1.0). O limiar mínimo de similaridade
#'   para que um nome seja incluído no resultado final. Padrão é 0.85.
#' @param indice Character. Caminho para o arquivo do banco de dados DuckDB
#'   contendo o índice invertido de palavras fonéticas (ex: 'dic_palavras_metaphone.duckdb').
#' @param central Character. Caminho para o arquivo do banco de dados DuckDB
#'   contendo a tabela central de nomes limpos (ex: 'nomes_limpos_master.duckdb').
#'
#' @return Um \code{data.table} ordenado por similaridade decrescente, contendo as colunas:
#'   \itemize{
#'     \item{\code{id}: O hash identificador único do nome encontrado.}
#'     \item{\code{nome_original}: O nome completo original encontrado na base central.}
#'     \item{\code{nome_metaphonebr}: A representação fonética pré-calculada do nome encontrado.}
#'     \item{\code{id_str}: O ID original convertido para string (para junção).}
#'     \item{\code{palavras_encontradas}: Inteiro indicando quantos tokens fonéticos do nome de entrada coincidiram com este candidato.}
#'     \item{\code{similaridade}: Score numérico (0-1) indicando o grau de similaridade final.}
#'   }
#'   Retorna um \code{data.table} vazio se nenhum candidato for encontrado ou se o nome de entrada for inválido.
#'
#' @details
#' Esta função é projetada para performar em bases com milhões de nomes, evitando
#' varreduras completas (full table scans) e cálculos de distância de string em toda a base.
#'
#' O processo ocorre nas seguintes etapas:
#' \enumerate{
#'   \item \strong{Configuração:} Conecta ao banco de índice e anexa o banco central em modo somente leitura.
#'   \item \strong{Pré-processamento da Entrada:} O \code{nome} de entrada é convertido para sua forma fonética (usando \code{metaphonebr}) e dividido em tokens (palavras).
#'   \item \strong{Seleção de Candidatos (Índice Invertido):} Uma consulta SQL busca no banco de \code{indice} quaisquer IDs de nomes que contenham pelo menos um dos tokens de entrada. Os resultados são agrupados por ID, e os \code{n_candidatos} com maior número de tokens coincidentes são selecionados.
#'   \item \strong{Recuperação de Dados Brutos:} Uma segunda consulta SQL busca os dados completos (nome original, metaphone pré-calculado) no banco \code{central} apenas para os IDs candidatos selecionados.
#'   \item \strong{Re-rankeamento Fino:} Para o conjunto reduzido de candidatos, a função calcula a similaridade exata entre a forma fonética da entrada e a forma fonética do candidato usando a função auxiliar \code{\link{calcular_similaridade_nomes}}.
#'   \item \strong{Filtragem:} Os resultados abaixo do \code{limite_similaridade} são descartados e o restante é ordenado.
#' }
#'
#' \strong{Pré-requisitos:}
#' A função assume a existência de dois arquivos DuckDB estruturados especificamente:
#' \itemize{
#'   \item \code{indice}: Deve conter a tabela \code{indice_palavras_metaphone} com mapeamento de palavras fonéticas para listas de IDs.
#'   \item \code{central}: Deve conter a tabela \code{nomes_limpos} com as colunas \code{nome_original_hash}, \code{nome_original} e \code{nome_metaphonebr}.
#' }
#'
#' @seealso \code{\link{calcular_similaridade_nomes}} para detalhes sobre o cálculo do score final.
#'
#' @import data.table
#' @import duckdb
#' @import DBI
#' @import stringdist
#' @import stringi
#' @importFrom metaphonebr metaphonebr
#'
#' @export
buscar_similares_indice <- \(nome,n_candidatos = 2000,
                             limite_similaridade=0.85,
                             indice='dic_palavras_metaphone.duckdb',
                             central='nomes_limpos_master.duckdb') {
  
  #A) CONFIGURAÇÃO INICIAL
  
  #1. Conexão ao índice de 'palavras' metaphone
  con <- DBI::dbConnect(duckdb::duckdb(),indice)
  
  
  
  #2. Anexa Dados de central de nomes (somente leitura por segurança)
  DBI::dbExecute(con,paste0("ATTACH '",central,"' AS central_de_nomes_db (READ_ONLY)"))
  
  on.exit({
    DBI::dbExecute(con,'DETACH central_de_nomes_db')
    DBI::dbDisconnect(con, shutdown = TRUE)
    })
  #3. Validação (opcional)
  #print(DBI::dbGetQuery(con,"SELECT table_name FROM information_schema.tables"))
  
  #B) limpeza de nome
  
  nome_limpo <- metaphonebr::metaphonebr(nome)
  
  
  nome_palavras <- unlist(stringi::stri_extract_all_words(nome_limpo))
  
  ##Se vazio, parar a retornar nada
  if(length(nome_palavras) == 0) return(data.table::data.table())
  
  #C) Obtenção de candidatos
  #Consulta ao índice reverso para qualquer ID com os tokens/palavras
  sql_palavras <- paste(paste0("'",nome_palavras,"'"), collapse = ",")
  
  
  ## Consulta para:
  #  1. Filtrar palavras
  #  2. UNNEST -> para formato long , permitindo contagem de ocorrências de id
  #  3. GROUP BY id -> agrupa pelo hash do nome
  #  4. COUNT(*)
  #  5. ORDER DESC -> prioridade para aqueles com maior número de matches
  ##IMPORTANTE - transformar ints de 128bit para string antes de chegar ao R
  ## com x::VARCHAR
  
  consulta_candidatos <- sprintf("
                                 WITH ocorrencias_brutas AS (
                                 SELECT unnest(ids) as id
                                 FROM indice_palavras_metaphone
                                 WHERE palavra IN (%s)
                                 )
                                 SELECT id::VARCHAR as id_str,
                                 COUNT(*) palavras_encontradas
                                 FROM ocorrencias_brutas
                                 GROUP BY id
                                 ORDER BY palavras_encontradas DESC
                                 LIMIT %d
                                 ", sql_palavras,n_candidatos)
  
  candidatos_classificados <- data.table::setDT(DBI::dbGetQuery(con,consulta_candidatos))
  
  ##Se vazio, parar a retornar nada
  if(nrow(candidatos_classificados) == 0) return(data.table::data.table())
  
  
  #D) recuperar nomes completos dos candidatos
  ids_candidatos <- candidatos_classificados$id_str
  
  consulta_ids <- paste(paste0("'",ids_candidatos,"'"),collapse=",")
  
  consulta_nomes <- sprintf("
                            SELECT nome_original_hash::VARCHAR as id, nome_original, nome_metaphonebr
                            FROM central_de_nomes_db.nomes_limpos
                            WHERE nome_original_hash IN (%s)
                            ", consulta_ids)
  
  #Resultado é um data.table com id, nome completo e nome metaphone
  dt_candidatos <- data.table::setDT(DBI::dbGetQuery(con,consulta_nomes))
  
  ##Para debug juntar com score de tokens
  dt_candidatos <- merge(
    dt_candidatos,
    candidatos_classificados,
    by.x="id",
    by.y="id_str"
  )
  
  #E) Re-rankeamento por similaridade
  
  
  #encontrados <- sugerir_correcao_nomes(nome_limpo,dt_candidatos$nome_metaphonebr)
  dt_candidatos[,similaridade:= calcular_similaridade_nomes(nome_limpo,nome_metaphonebr)]
  
  ##Filtrar pelo threshold
  encontrados <-  dt_candidatos[similaridade>=limite_similaridade]
  
  data.table::setorder(encontrados,-similaridade)
  
  return(encontrados)
  
  
}








#' Processamento em lote de nomes
#'
#' Processa um vetor de nomes em lotes para encontrar nomes similares,
#' otimizado para grandes volumes de dados.
#'
#' @param vetor_nomes Vetor de nomes para processar (character vector)
#' @param chunk_size Tamanho de cada lote para processamento (default = 10000)
#'
#' @return Um data.frame combinado com todos os resultados dos lotes
#'
#' @details
#' A função:
#' \itemize{
#'   \item Remove duplicatas exatas primeiro
#'   \item Processa os dados em blocos (chunks) para otimizar memória
#'   \item Pode ser facilmente paralelizada modificando o loop interno
#' }
#'
#' @examples
#' processamento_lote(c("Maria", "João", "Ana", "Pedro", "Francisco"))
#' nomes_grande_vetor <- sample(c("Maria", "João", "Ana", "Pedro", "Francisco"),100,replace = TRUE)
#' processamento_lote(nomes_grande_vetor, chunk_size = 50)
#'
#' @export



#4. Implementação Escalável

processamento_lote <- \(vetor_nomes, chunk_size = 10000) {
  
  # Remove duplicatas exatas primeiro
  nomes_unicos <- unique(vetor_nomes)
  
  
  
  # Processa em chunks
  resultados <- list()
  
  
  
  for (i in seq(1, length(nomes_unicos), chunk_size)) {
    
    chunk <- nomes_unicos[i:min(i + chunk_size - 1, length(nomes_unicos))]
    
    
    
    # Aqui você pode paralelizar
    
    chunk_result <- lapply(chunk, \(nome) {
      
      buscar_similares_indice(nome, nomes_unicos)
      
    })
    
    
    
    resultados <- c(resultados, chunk_result)
    
  }
  
  
  
  return(dplyr::bind_rows(resultados))
  
}








