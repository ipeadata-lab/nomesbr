
#Teste para função de limpeza de nomes marcar_problemas_e_limpar_nomes
test_that("marcar_problemas_e_limpar_nomes funciona corretamente",{
  DT_input <- data.table(nome = c("PEDRO SANT ANA MOURAO FALECIDO","JJOSE D ACQUA","NAO CONSTA NADA"," TESTE "))
  DT_cleaned <- marcar_problemas_e_limpar_nomes(DT_input,"nome")
  
  expect_equal(DT_cleaned[1,nome_clean],'PEDRO SANTANA MOURAO') #FALECIDO removido, removido espaco avaliado como apostrofo 
  expect_equal(DT_cleaned[2,nome_clean],'JOSE DACQUA') #repetição de D removida, removido espaco avaliado como apostrofo
  expect_true(is.na(DT_cleaned[3,nome_clean])) #NAO CONSTA NADA deve virar NA
  expect_true(is.na(DT_cleaned[4,nome_clean])) #TESTE e espaços extras devem ser removidos, resultando em NA se vazio
  expect_true(all(c('falecido','cartorio','nome_clean') %in% names(DT_cleaned)))
})




#Teste para função de simplificação de nomes remove_PARTICULAS_AGNOMES

test_that("remove_PARTICULAS_AGNOMES funciona", {
  expect_equal(remove_PARTICULAS_AGNOMES("JOAO DA SILVA FILHO"),"JOAO SILVA")
  expect_equal(remove_PARTICULAS_AGNOMES("MARIA DE SOUSA"),"MARIA SOUSA")
})

