#' Limpa e Analisa Nomes em um data.table
#'
#' @description
#' Processa uma coluna de nomes em um `data.table`, aplicando uma série de
#' regras de limpeza para identificar e corrigir/marcar problemas comuns como
#' menções a "FALECIDO", "CARTORIO", erros de digitação, espaços indevidos, etc.
#'
#' @param d Um objeto `data.table`.
#' @param s O nome da coluna (em string) dentro de `d` que contém os nomes a serem processados.
#'
#' @return Um `data.table` modificado, contendo a coluna original, uma nova coluna
#'   com sufixo "_clean" com os nomes limpos, e colunas booleanas indicando
#'   a detecção de cada tipo de problema (ex: `falecido`, `cartorio`).
#'
#' @details
#' A função executa os seguintes passos principais:
#' \enumerate{
#'   \item Cria uma cópia da coluna de nomes para limpeza.
#'   \item Detecta e trata menções a "FALECIDO(A)".
#'   \item Detecta e trata menções a "CARTORIO" e nomes de cidades comuns em registros.
#'   \item Corrige problemas de espaçamento perto de acentos, til e apóstrofes usando `limpa_espaco_acento_til_apostrofe`.
#'   \item Identifica e trata nomes contendo termos problemáticos como "PAI", "MAE", "SEM", "NAO",
#'         exceto em contextos aceitáveis.
#'   \item Identifica e trata casos de "NADA CONSTA" e variações.
#'   \item Corrige E, DA, DE e variantes com caracter prévio indevido (ex: "EDAS" para "DAS" se aplicável).
#'   \item Remove saudações como "SR.", "SRA.".
#'   \item Remove termos como "IGNORADO", "DESCONHECIDO".
#'   \item Remove títulos específicos como "DOUTOR", "CORONEL".
#'   \item Remove repetições de partículas de ligação (ex: "DE DE").
#'   \item Remove a palavra "TESTE".
#'   \item Limpa letras repetidas no início ou meio de palavras.
#' }
#'
#' @export
#' @import data.table
#' @importFrom stringr str_detect str_remove str_replace str_replace_all str_trim
#' @importFrom dplyr na_if
#' @examples
#' # Supondo que 'meu_DT' é um data.table com uma coluna 'nome_sujo'
#' # DT_exemplo <- data.table::data.table(
#' #   id = 1:3,
#' #   nome_sujo = c("MARIA FALECIDA SSILVA", "CARTORIO DE PAZ", "JOAO D ARC")
#' # )
#' # DT_limpo <- marcar_problemas_e_limpar_nomes(DT_exemplo, "nome_sujo")
#' # print(DT_limpo)
#' 
marcar_problemas_e_limpar_nomes <- \(d, s) {
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('0. Making copy of dataset and add the s2(the var to be cleaned)')
  s2 <- paste0(s, "_clean")
  d <- data.table::copy(d)
  d[, (s2) := get(s)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('1. Detect and clean "FALECIDO/A" (and variants)')
  d[, falecido := stringr::str_detect(get(s2), 'FALES?CI')]
  d[falecido == 1, (s2) := ifelse(stringr::str_detect(get(s2), regex_FALECIDO),
                                  NA_character_,
                                  stringr::str_remove(get(s2), regex_nome_FALECIDO))]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('2. Detect and clean "CARTORIO" cases')
  d[, cartorio := stringr::str_detect(get(s2), 'CARTORIO|REGISTRO CIVIL')]
  d[cartorio == 1, (s2) := get(s2) |>
      stringr::str_remove(regex_CARTORIO) |>
      stringr::str_remove(regex_CARTORIODE) |>
      stringr::str_remove('^CARTORIO|(DE )?CARTORIO$') |>
      stringr::str_remove(regex_CIDADES) |>
      stringr::str_trim() |>
      na_if('')]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('3. Identify cases with extra spaces before accented letters, tilde and apostrophe')
  d[, espaco_TilAcentoApostrofe := stringr::str_detect(get(s2), regex_cAO) |
      stringr::str_detect(get(s2), regex__AO) |
      stringr::str_detect(get(s2), regex_J_AO) |
      stringr::str_detect(get(s2), regex_apostrofe) |
      stringr::str_detect(get(s2), regex_d_vogal_candidato_apostrofo) |
      stringr::str_detect(get(s2), regex_nome_acento)]
  d[espaco_TilAcentoApostrofe == TRUE, (s2) := get(s2) |> limpa_espaco_acento_til_apostrofe()]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('4. Detect: PAI MAE SEM NAO and fix')
  d[, nome_P_M_S_N := stringr::str_detect(get(s2), regex_P_M_S_N) &
      !stringr::str_detect(get(s2), regex_nome_aceito)]
  d[nome_P_M_S_N == 1, (s2) := stringr::str_replace(get(s2), 'PAI XAO', 'PAIXAO')]
  d[nome_P_M_S_N == 1, (s2) := stringr::str_replace(get(s2), 'SEM SOBRENOME$', '')]
  d[nome_P_M_S_N == 1, (s2) := stringr::str_replace(get(s2), '(PAI|MAE) ADOTIV[AO]$', '')]
  d[nome_P_M_S_N == 1, (s2) := stringr::str_replace(get(s2), regex_naoMora, '')]
  # Recompute the condition after the corrections; if still problematic then (optionally) set to NA.
  d[nome_P_M_S_N == 1, nome_P_M_S_N := stringr::str_detect(get(s2), regex_P_M_S_N)]
  #d[nome_P_M_S_N == 1, (s2) := NA]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('5. Detect "NADA_NAO" and "CONSTA" cases, and make their combo NA')
  d[, nada_nao := stringr::str_detect(get(s2), regex_NADA_NAO) &
      !stringr::str_detect(get(s2), regex_nome_aceito)]
  d[, consta := stringr::str_detect(get(s2), regex_CONSTA) &
      !stringr::str_detect(get(s2), regex_nome_aceito)]
  d[nada_nao == 1 & consta == 1, nada_nao_consta := stringr::str_detect(get(s2), regex_NADA_NAO_CONSTA)]
  d[nada_nao_consta == 1, nada_nao_consta2 := stringr::str_detect(get(s2), regex_NADA_NAO_CONSTA2)]
  
  # For unambiguous missing cases, force the cleaned column to NA.
  d[nada_nao_consta2 == 1, (s2) := NA]
  d[nada_nao == 1 & get(s2) %in% NA_nada_nao_copleto, (s2) := NA]
  d[consta == 1 & get(s2) %in% NA_consta_completo, (s2) := NA]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('6. Detect Xartigo and replace when unambigous (ex: DDE to DE)')
  d[, Xartigo :=  stringr::str_detect(get(s2), regex_nome_XEDADEDOS) &
      !stringr::str_detect(get(s2), regex_nome_XEDADEDOS_allowed)]
  d[Xartigo==1, (s2) := stringr::str_replace(get(s2), regex_nome_XEDADEDOS_replace, "\\1\\2\\3")]
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('7. Detect and clean SR_SRA and variants')
  d[,sr_sra := stringr::str_detect(get(s2),regex_SR_SRA)]
  d[sr_sra==1,(s2) := stringr::str_remove(get(s2),regex_SR_SRA)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('8. Detect and clean desconhecido ignorado and variants')
  d[,ignorado:= stringr::str_detect(get(s2),regex_IGNORADO)]
  d[ignorado==1,(s2):= stringr::str_remove(get(s2),regex_IGNORADO)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('9. Detect and clean specific titles - Doutor, Coronel and few others')
  d[,dr_coronel:= stringr::str_detect(get(s2),regex_DR_CORONEL)]
  d[dr_coronel==1,(s2):= stringr::str_remove(get(s2),regex_DR_CORONEL)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('10. Detect and clean repeated de de da da do do ')
  d[,dede_dada:= stringr::str_detect(get(s2),regex_DEDEDADA)]
  d[dede_dada==1,(s2):= stringr::str_remove(get(s2),regex_DEDEDADA)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('11. Detect and clean TESTE and variants ')
  d[,teste:= stringr::str_detect(get(s2),regex_TESTE)]
  d[teste==1,(s2):= stringr::str_remove(get(s2),regex_TESTE)]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('12. Detect and clean repeated letters with specific rules whether at beginning or middle of the word')
  d[,letra_repetida:= stringr::str_detect(get(s2),"([^\\s])\\1+")]
  #clean repeated letters in beginning of word not considered valid
  d[letra_repetida==1,(s2):= gsub(regex_LETRA_IN_REPETIDA,"\\1",get(s2),perl=T)]
  #clean repeated letters in the middle of word not considered valid
  d[letra_repetida==1,(s2):= stringr::str_replace_all(get(s2),"(?<!\\b)([^RSrs])\\1+","\\1")]
  d[letra_repetida==1,(s2):= stringr::str_replace_all(get(s2),"(?<!\\b)([RSrs])\\1\\1+","\\1\\1")]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::tic('13. Force NA if resulting cleaned name is empty or spaces')
  d[grepl('^\\s*$',nome_clean),nome_clean:= NA_character_]
  if (requireNamespace("tictoc", quietly = TRUE)) tictoc::toc()
  
  return(d)
}


#' @rdname marcar_problemas_e_limpar_nomes
#' @export
find_and_clean_NAnames_and_extra_spaces <- marcar_problemas_e_limpar_nomes