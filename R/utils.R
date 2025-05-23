#' @keywords internal
limpa_espaco_acento_til_apostrofe <- function(c) {
  if(!inherits(c, 'character')) stop('c must be a character vector')
  c <- c |> 
    stringr::str_replace(regex_cAO, "\\1AO\\2") |>
    stringr::str_replace(regex__AO, "\\1CAO\\2") |> 
    stringr::str_replace(regex_J_AO, "\\1JOAO\\2") |>
    stringr::str_replace(regex_apostrofe, function(x) gsub(" ", "", x)) |> 
    stringr::str_replace(regex_d_a_falsopositivo_apostrofo,\(x) gsub("\\bD A","DA ",x))|>
    stringr::str_replace(regex_d_e_falsopositivo_apostrofo_melhora_sobrenome,\(x) gsub("\\bD E","DE ",x))|>
    stringr::str_replace(regex_d_o_falsopositivo_apostrofo,\(x) gsub("\\bD O","DO ",x))|>
    stringr::str_replace(regex_d_vogal_candidato_apostrofo,\(x) gsub(" ","",x))|>
    stringr::str_replace(regex_nome_acento, function(x) gsub(" ", "", x))
  
  return(c)
}

