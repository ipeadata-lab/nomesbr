consulta_nome_em_central <- 
  \(nomes, 
    mestre ,
    usar_hash = TRUE) {
    
    conexao <- duckdb::dbConnect(duckdb::duckdb(),dbdir=mestre)
    
    on.exit(DBI::dbDisconnect(conexao,shutdown = TRUE))
    
    if (usar_hash) {
      coluna <-  'nome_original_hash'
    } else {
      coluna <- 'nome_original'
    }
    
    consulta1 <- \(w) { 
      elementos <- paste(rep("?",length(w)),collapse=", ")
      consulta <- 
        paste0(
          "SELECT * from nomes_limpos   
          WHERE ",coluna," IN (",
          elementos,")")
      
      a <- DBI::dbGetQuery(conexao,
                           consulta,params = as.list(w))
      
    }
    
    
    encontrado <- consulta1(nomes)
    
    
    return(encontrado)
  }
