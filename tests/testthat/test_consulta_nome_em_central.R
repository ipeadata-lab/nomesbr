# Testes para a função consulta_nome_em_central()
test_that("consulta_nome_em_central funciona corretamente com hash MD5", {
  # Pula este teste se duckdb não estiver instalado
  skip_if_not_installed("duckdb")
  # Pula este teste se DBI não estiver instalado
  skip_if_not_installed("DBI")
  # --- ARRANGE (Preparação do ambiente de teste) ---
  
  # 1. Criar um banco de dados DuckDB temporário em memória para os testes.
  #    Usamos :memory: para que o banco não seja salvo no disco.
  con_teste <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  
  # 2. Criar a tabela 'nomes_limpos' que a função espera encontrar.
  #    Incluímos as colunas necessárias para os dois tipos de consulta.
  DBI::dbExecute(con_teste, "
    CREATE TABLE nomes_limpos (
      id INTEGER PRIMARY KEY,
      nome_original VARCHAR,
      nome_original_hash BIGINT, -- md5_number retorna um BIGINT
      categoria VARCHAR
    )
  ")
  
  # 3. Inserir dados de teste com nomes fictícios brasileiros comuns.
  #    Primeiro, inserimos os dados base (nome_original e categoria).
  dados_base <- data.frame(
    id = 1:5,
    nome_original = c("Ana Silva", "Carlos Souza", "Maria Oliveira", "João Santos", "Fernanda Costa"),
    categoria = c("A", "B", "A", "C", "B")
  )
  duckdb::dbWriteTable(con_teste, "nomes_limpos", dados_base, append = TRUE)
  
  # 4. AGORA, usar o próprio DuckDB para calcular e preencher a coluna de hash.
  #    Isso simula perfeitamente o seu processo de ETL.
  duckdb::dbExecute(con_teste, "
    UPDATE nomes_limpos
    SET nome_original_hash = md5_number(nome_original)
  ")
  
  # 5. Desconectar para simular o comportamento real da função.
  #    A função irá se conectar novamente.
  duckdb::dbDisconnect(con_teste, shutdown = TRUE)
  
  # 6. Definir o caminho para o banco de dados temporário no disco.
  #    A função espera um caminho de arquivo, não um banco em memória.
  mestre_temp <- tempfile(fileext = ".duckdb")
  # Copiar o banco em memória para um arquivo temporário no disco
  # (Alternativa: criar o banco diretamente no disco desde o início)
  # Vamos recriar no disco para simplificar e garantir isolamento.
  
  con_temp <- duckdb::dbConnect(duckdb::duckdb(), dbdir = mestre_temp)
  duckdb::dbExecute(con_temp, "
    CREATE TABLE nomes_limpos (
      id INTEGER PRIMARY KEY,
      nome_original VARCHAR,
      nome_original_hash BIGINT,
      categoria VARCHAR
    )
  ")
  duckdb::dbWriteTable(con_temp, "nomes_limpos", dados_base, append = TRUE)
  DBI::dbExecute(con_temp, "
    UPDATE nomes_limpos
    SET nome_original_hash = md5_number(nome_original)
  ")
  duckdb::dbDisconnect(con_temp, shutdown = TRUE)
  
  
  # --- ACT & ASSERT (Execução e Verificação) ---
  
  # Para os testes, precisamos saber os valores dos hashes que foram gerados.
  # Vamos consultá-los para usar como entrada.
  con_para_hashes <- duckdb::dbConnect(duckdb::duckdb(), dbdir = mestre_temp)
  hashes_gerados <- DBI::dbGetQuery(con_para_hashes, "
    SELECT nome_original, nome_original_hash FROM nomes_limpos
  ")
  DBI::dbDisconnect(con_para_hashes, shutdown = TRUE)
  
  # Teste 1: Consulta por texto (usar_hash = FALSE)
  nomes_para_buscar_texto <- c("Ana Silva", "João Santos")
  resultado_texto <- consulta_nome_em_central(
    nomes = nomes_para_buscar_texto,
    mestre = mestre_temp,
    usar_hash = FALSE
  )
  
  # Verificar se o resultado tem o número correto de linhas
  expect_equal(nrow(resultado_texto), 2)
  # Verificar se os nomes retornados são os esperados (ordem não importa)
  expect_setequal(resultado_texto$nome_original, nomes_para_buscar_texto)
  # Verificar se a categoria de um dos nomes está correta
  expect_equal(resultado_texto$categoria[resultado_texto$nome_original == "Ana Silva"], "A")
  
  # Teste 2: Consulta por hash (usar_hash = TRUE, padrão)
  # Pega os hashes correspondentes aos nomes que queremos buscar
  hashes_para_buscar <- hashes_gerados$nome_original_hash[
    hashes_gerados$nome_original %in% c("Carlos Souza", "Fernanda Costa")
  ]
  
  resultado_hash <- consulta_nome_em_central(
    nomes = hashes_para_buscar,
    mestre = mestre_temp
  )
  
  # Verificar se o resultado tem o número correto de linhas
  expect_equal(nrow(resultado_hash), 2)
  # Verificar se os hashes retornados são os esperados
  expect_setequal(resultado_hash$nome_original_hash, hashes_para_buscar)
  # Verificar se a categoria de um dos nomes está correta
  expect_equal(resultado_hash$categoria[resultado_hash$nome_original_hash == hashes_para_buscar[1]], "B")
  
  # Teste 3: Consulta por nome que não existe
  nomes_inexistentes <- c("Pedro Henrique", "Clara Mendes")
  resultado_vazio <- consulta_nome_em_central(
    nomes = nomes_inexistentes,
    mestre = mestre_temp,
    usar_hash = FALSE
  )
  
  # Verificar se o resultado é um data.frame com zero linhas
  expect_s3_class(resultado_vazio, "data.frame")
  expect_equal(nrow(resultado_vazio), 0)
  
  # Teste 4: Consulta com vetor de entrada vazio
  resultado_entrada_vazia <- consulta_nome_em_central(
    nomes = character(0), # Vetor de caracteres vazio
    mestre = mestre_temp
  )
  
  # Verificar se o resultado é um data.frame com zero linhas
  expect_s3_class(resultado_entrada_vazia, "data.frame")
  expect_equal(nrow(resultado_entrada_vazia), 0)
  
  # --- CLEANUP (Limpeza) ---
  # Remover o arquivo de banco de dados temporário
  unlink(mestre_temp)
})