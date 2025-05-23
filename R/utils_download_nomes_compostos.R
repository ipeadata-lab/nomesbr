#' Recupera o dataset nomes_proprios_compostos, baixando-o se necessário
#'
#' Verifica se o dataset nomes_proprios_compostos existe em uma pasta de cache local.
#' Se inexistir, baixa-o de um release do GitHub e o salva no cache.
#'
#' @return Um data.table contendo os dados de nomes_proprios_compostos.
#' @keywords internal
#' @importFrom tools R_user_dir
#' @importFrom utils download.file
#' @importFrom data.table fread
obter_dic_nomes_proprios_compostos <- function() {
  pkg_name <- "nomesbr" 
  cache_dir <- tools::R_user_dir(package = pkg_name, which = "cache")
  nomes_proprios_compostos_file_path <- file.path(cache_dir, "nomes_proprios_compostos.rds")
  
  # URL do arquivo nomes_proprios_compostos.rds no GitHub Release
  # Substitua pela URL real do seu release!
  nomes_proprios_compostos_url <- "https://github.com/ipeadata-lab/nomesbr/releases/download/v0.0.1-alpha/nomes_proprios_compostos.rds"
  
  if (!file.exists(nomes_proprios_compostos_file_path)) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    message(paste0("Baixando o arquivo de dados nomes_proprios_compostos.rds (aprox. 109MB).\n",
                   "Isso vai acontecer apenas uma vez.\n",
                   "Salvando em: ", nomes_proprios_compostos_file_path))
    tryCatch({
      # Usar tempfile para download seguro
      temp_file <- tempfile(fileext = ".rds")
      
      # Usando httr para mais controle e robustez (requer httr em Imports ou Suggests)
      if (!requireNamespace("httr", quietly = TRUE)) {
        stop("Necessita-se do pacote 'httr' para baixar os dados. Por favor, instale-o.", call. = FALSE)
      }
      resp <- httr::GET(nomes_proprios_compostos_url, httr::write_disk(temp_file, overwrite = TRUE), httr::progress())
      httr::stop_for_status(resp) # Verifica se o download foi bem-sucedido
      
      # Mover o arquivo baixado para o local de cache
      file.rename(temp_file, nomes_proprios_compostos_file_path)
      message("Download finalizado.")
    }, error = function(e) {
      stop(paste0("Falha ao baixar o arquivo nomes_proprios_compostos.rds de ", nomes_proprios_compostos_url, "\nErro: ", e$message), call. = FALSE)
    })
  } else {
    message(paste0("Usando arquivo nomes_proprios_compostos.rds do cache: ", nomes_proprios_compostos_file_path))
  }
  
  # Ler o arquivo RDS
  nomes_proprios_compostos_data <- readRDS(nomes_proprios_compostos_file_path)
  
  
  return(nomes_proprios_compostos_data)
}