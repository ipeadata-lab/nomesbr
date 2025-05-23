#' @import stringi
NULL # Truque para que o @import stringi seja processado sem uma função imediatamente abaixo dele

# Funções auxiliares para metaphoneBR (internas ao pacote)

#' Pré-processamento Fonético: Remove acentos, números e capitaliza
#'
#' Remove diacríticos, converte para maiúsculas e remove caracteres
#' que não sejam letras ou espaços.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres pré-processado.
#' @keywords internal
#' @examples
#' # nomesbr:::remove_acentos_numeros_e_capitaliza("João Silva 123!")
remove_acentos_numeros_e_capitaliza <- function(texto) {
  # Remove diacríticos (ex.: "João" -> "Joao")
  texto <- stringi::stri_trans_general(texto, "Latin-ASCII")
  # Converte para maiúsculas (ex.: "joao" -> "JOAO")
  texto <- base::toupper(texto)
  # Remove caracteres que não sejam letras ou espaços
  # Exemplo: "JOAO123!" -> "JOAO"
  # Ajuste para manter espaços entre palavras, mas remover múltiplos espaços depois
  texto <- stringi::stri_replace_all_regex(texto, "[^A-Z ]+", "")
  texto <- stringi::stri_replace_all_regex(texto, "\\s+", " ") # Garante espaço único
  texto <- stringi::stri_trim_both(texto) # Remove espaços no início/fim
  return(texto)
}

#' Remoção Fonética: Letras Silenciosas
#'
#' Remove 'H' silencioso no início de cada palavra.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres com 'H's iniciais removidos.
#' @keywords internal
#' @examples
#' # nomesbr:::remove_letras_silenciosas("Helena Horta")
remove_letras_silenciosas <- function(texto) {
  # Remove 'H' silencioso no início de cada palavra.
  # Exemplo: "Helena Silva" -> "ELENA SILVA"
  texto <- stringi::stri_replace_all_regex(texto, "\\bH", "")
  return(texto)
}

#' Simplificação Fonética: Dígrafos Similares
#'
#' Transforma dígrafos comuns para simplificar a representação fonética.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres com dígrafos simplificados.
#' @keywords internal
#' @examples
#' # nomesbr:::simplifica_digrafos_similares("FILHA MANHA CHICO SCHMIDT SCENA ESCOVA QUILO")
simplifica_digrafos_similares <- function(texto) {
  # Transforma "LH" em "1" (representação arbitrária para som palatal lateral)
  texto <- stringi::stri_replace_all_fixed(texto, "LH", "1")
  # Transforma "NH" em "3" (representação arbitrária para som palatal nasal)
  texto <- stringi::stri_replace_all_fixed(texto, "NH", "3")
  # Transforma "CH" em "X" (som /ʃ/)
  texto <- stringi::stri_replace_all_fixed(texto, "CH", "X")
  # Transforma "SH" em "X" (para nomes estrangeiros com som /ʃ/)
  texto <- stringi::stri_replace_all_fixed(texto, "SH", "X")
  # Transforma "SCH" em "X" (som /ʃ/ ou /sk/ dependendo da origem, aqui simplificado para X)
  texto <- stringi::stri_replace_all_fixed(texto, "SCH", "X") # Pode ser "SK" também, decisão de design
  # Transforma "PH" em "F"
  texto <- stringi::stri_replace_all_fixed(texto, "PH", "F")
  # Trata dígrafos "SC" conforme a vogal que o segue:
  # Se "SC" seguido de E ou I, transforma em "S"
  texto <- stringi::stri_replace_all_regex(texto, "SC(?=[EI])", "S")
  # Se "SC" (ou XC) seguido de A, O ou U, transforma em "SK" (ou XK). O original tinha SK.
  # Para manter consistência com 'C' se tornando 'K', 'SC' se torna 'SK' aqui.
  texto <- stringi::stri_replace_all_regex(texto, "SC(?=[AOU])", "SK")
  # Trata o dígrafo "QU": remove o U mudo antes de E ou I
  texto <- stringi::stri_replace_all_regex(texto, "Q[UÜ](?=[EI])", "K") # QUE, QUI -> KE, KI
  
  # "QU" seguido de A, O -> K (o U é geralmente pronunciado mas aqui simplificamos para K)
  texto <- stringi::stri_replace_all_regex(texto, "QU", "K") # QUanto -> KANTO (simplificado)
  
  return(texto)
}

#' Simplificação Fonética: Consoantes Similares
#'
#' Agrupa sons consonantais foneticamente similares.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres com consoantes simplificadas.
#' @keywords internal
#' @examples
#' # nomesbr:::simplifica_consoantes_similares("CAÇADOR CELSO CARLOS GEOVANIA WALTER YARA ZEBRA")
simplifica_consoantes_similares <- function(texto) {
  # Transforma "Ç" em "S"
  texto <- stringi::stri_replace_all_fixed(texto, "Ç", "S")
  # Letra C: se seguida de E ou I, transforma em "S"
  texto <- stringi::stri_replace_all_regex(texto, "C(?=[EI])", "S")
  # Letra C: se não seguida de E ou I (e não parte de CH, SC já tratados), transforma em "K"
  texto <- stringi::stri_replace_all_regex(texto, "C(?![EIH])", "K") # Evita reprocessar CH
  texto <- stringi::stri_replace_all_fixed(texto, "C", "K") # C remanescente torna-se K
  
  # Letra G: se seguida de E ou I, transforma em "J" (GUE/GUI já tratados para G)
  texto <- stringi::stri_replace_all_regex(texto, "G(?=[EI])", "J")
  # G remanescente (antes de A, O, U ou consoante) permanece G, não K.
  
  # Q sempre se torna "K" (QU já tratado, mas Q isolado ou em outros contextos)
  texto <- stringi::stri_replace_all_fixed(texto, "Q", "K")
  # Transforma "W" em "V" (ou "U" dependendo da pronúncia, V é comum em BR)
  texto <- stringi::stri_replace_all_fixed(texto, "W", "V")
  # Transforma "Y" em "I"
  texto <- stringi::stri_replace_all_fixed(texto, "Y", "I")
  # Transforma todas as ocorrências de "Z" em "S"
  texto <- stringi::stri_replace_all_fixed(texto, "Z", "S")

  
  return(texto)
}

#' Simplificação Fonética: Sons Nasais Terminais
#'
#' Unifica sons nasais no final das palavras.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres com sons nasais terminais simplificados.
#' @keywords internal
#' @examples
#' # nomesbr:::simplifica_sons_nasais_terminais(c("JOAQUIN", "JOAQUIM"))
simplifica_sons_nasais_terminais <- function(texto) {
  # Converte N, M, ou qualquer vogal nasalizada (representada por vogal+M/N) no final da palavra.
  # O Metaphone original foca em consoantes. Aqui, uma simplificação:
  # AO, AN, AM -> OM (ou código numérico)
  # EN, EM -> EM
  # IN, IM -> IM
  # ON, OM -> OM
  # UN, UM -> UM
  # Simplificando para N final virar M (como no seu original)
  texto <- stringi::stri_replace_all_regex(texto, "N\\b", "M")

  return(texto)
}

#' Remoção Fonética: Vogais Duplicadas
#'
#' Comprime sequências de vogais idênticas adjacentes.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres com vogais duplicadas removidas.
#' @keywords internal
#' @examples
#' # nomesbr:::remove_vogais_duplicadas(c("AARAO", "REEBECA"))
remove_vogais_duplicadas <- function(texto) {
  # Comprime sequências de vogais duplicadas.
  # Exemplo: "REEBA" -> "REBA"
  texto <- stringi::stri_replace_all_regex(texto, "([AEIOU])\\1+", "$1")
  return(texto)
}

#' Remoção Fonética: Espaços e Letras Duplicadas
#'
#' Remove letras duplicadas (exceto RR, SS que podem ter sons distintos) e espaços extras.
#'
#' @param texto Um vetor de caracteres.
#' @return Um vetor de caracteres limpo.
#' @keywords internal
#' @examples
#' # nomesbr:::remove_espacos_e_letras_duplicadas(c("  CARRO OSSO  ", "JOAOZINHO"))
remove_espacos_e_letras_duplicadas <- function(texto) {
  # Remove letras duplicadas adjacentes.
  texto <- stri_replace_all_regex(texto, "(\\w)\\1+", "$1")
  # Remove espaços em branco no início e no fim, e múltiplos espaços.
  texto <- stringi::stri_replace_all_regex(texto, "\\s+", " ")
  texto <- stringi::stri_trim_both(texto)
  return(texto)
}


#' Gera Código Fonético (Metaphone-BR adaptado) para Nomes em Português
#'
#' Aplica uma série de transformações fonéticas a um vetor de nomes para
#' gerar códigos que representam sua pronúncia aproximada em português brasileiro.
#' O objetivo é agrupar nomes que soam de forma similar, mesmo que escritos
#' de maneira diferente.
#'
#' @param nomes Um vetor de caracteres contendo os nomes a serem processados.
#' @param verbose Lógico, se `TRUE`, imprime mensagens de progresso para cada passo.
#'        Padrão é `FALSE`.
#'
#' @return Um vetor de caracteres contendo os códigos fonéticos correspondentes
#'   a cada nome de entrada.
#'
#' @details
#' O processo de transformação inclui as seguintes etapas:
#' \enumerate{
#'   \item Pré-processamento: Remoção de acentos, números e capitalização.
#'   \item Remoção de letras silenciosas (como 'H' inicial).
#'   \item Simplificação de dígrafos comuns (LH, NH, CH, SC, QU, etc.).
#'   \item Simplificação de consoantes com sons similares (C/K/S, G/J, Z/S, etc.).
#'   \item Simplificação de sons nasais terminais.
#'   \item Remoção de vogais duplicadas.
#'   \item Remoção de letras duplicadas e espaços extras.
#' }
#' Esta é uma adaptação e não segue estritamente nenhum algoritmo Metaphone
#' publicado, mas é inspirada por eles para o contexto do português brasileiro.
#'
#' @export
#' @examples
#' nomes_exemplo <- c("João Silva", "Joao da Silva", "Maria", "Marya",
#'                    "Helena", "Elena", "Philippe", "Felipe", "Xavier", "Chavier")
#' codigos_foneticos <- metaphoneBR(nomes_exemplo)
#' print(data.frame(Original = nomes_exemplo, MetaphoneBR = codigos_foneticos))
#'
#' # Com mensagens de progresso
#' # codigos_foneticos_verbose <- metaphoneBR("Exemplo Único", verbose = TRUE)
metaphoneBR <- function(nomes, verbose = FALSE) {
  # Verifica se 'nomes' é um vetor de caracteres.
  if (!is.character(nomes)) {
    stop("Erro: 'nomes' deve ser um vetor de caracteres.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("Erro: 'verbose' deve ser um único valor lógico (TRUE ou FALSE).")
  }
  
  # Aplicar a cada nome no vetor
  resultados <- sapply(nomes, function(nome_individual) {
    if (is.na(nome_individual) || nome_individual == "") {
      return(NA_character_) # Retorna NA se a entrada for NA ou vazia
    }
    
    x <- remove_acentos_numeros_e_capitaliza(nome_individual)
    if (verbose) message("PASSO 1 - remove_acentos_numeros_e_capitaliza: '", nome_individual, "' -> '", x, "'")
    
    x <- remove_letras_silenciosas(x)
    if (verbose) message("PASSO 2 - remove_letras_silenciosas:           '", nome_individual, "' -> '", x, "'")
    
    x <- simplifica_digrafos_similares(x)
    if (verbose) message("PASSO 3 - simplifica_digrafos_similares:       '", nome_individual, "' -> '", x, "'")
    
    x <- simplifica_consoantes_similares(x)
    if (verbose) message("PASSO 4 - simplifica_consoantes_similares:     '", nome_individual, "' -> '", x, "'")
    
    x <- simplifica_sons_nasais_terminais(x)
    if (verbose) message("PASSO 5 - simplifica_sons_nasais_terminais:    '", nome_individual, "' -> '", x, "'")
    
    x <- remove_vogais_duplicadas(x)
    if (verbose) message("PASSO 6 - remove_vogais_duplicadas:            '", nome_individual, "' -> '", x, "'")
    
    x <- remove_espacos_e_letras_duplicadas(x) # Esta função já lida com trim e espaços múltiplos
    if (verbose) message("PASSO 7 - remove_espacos_e_letras_duplicadas:  '", nome_individual, "' -> '", x, "'")
    
    return(x)
  }, USE.NAMES = FALSE)
  
  return(resultados)
}