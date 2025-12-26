#' Validar abordagem de similaridade
#'
#' Função para validar a abordagem de cálculo de similaridade com casos de teste conhecidos.
#' Thresholds Recomendados

#' Alta confiança: 0.85-0.90 (poucos falsos positivos)

#' Balanceado: 0.75-0.85 (uso geral)

#' Sensível: 0.65-0.75 (captura mais variações)

#'
#' @return Imprime no console os resultados dos testes de similaridade
#'
#' @details
#' Testa a função calcular_similaridade_nomes com pares de nomes conhecidos,
#' exibindo os scores de similaridade para avaliação visual.
#'
#' @examples
#' validar_abordagem()
#'



validar_abordagem <- \() {
  
  testes <- data.frame(
    
    original = c("maria", "jose", "francisco", "ana","josue"),
    
    variantes = c("maria", "jos\u00e9", "francisca", "anna","jose"),
    
    stringsAsFactors = FALSE
    
  )
  
  
  
  resultado <- sapply(1:nrow(testes), \(i){
    
    sim <- round(calcular_similaridade_nomes(testes$original[i], testes$variantes[i]),3)
    
    cat(sprintf("%s vs %s: %.3f\n", testes$original[i], testes$variantes[i], sim))
   
    return(sim)
      
  })
  return(resultado)
}


testthat::test_that('teste_abordagem_similaridade_nomes',
          {testthat::expect_equal(validar_abordagem(),c(1,1,0.915,1,0.893))})

