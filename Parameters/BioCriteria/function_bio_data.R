
bio_data <- function(database) {
  
  
  require(tidyverse)
  require(RODBC)
  require(odeqIRtools)
  
  print("Fetch bio indexes from IR database")
  #connect to IR database view as a general user
  # import Temperature data
  database = "IR_Dev" # use for testing 
  IR.sql <-  DBI::dbConnect(odbc::odbc(), database)
  
  
  # Results_import <- DBI::dbReadTable(IR.sql, "VW_BioCriteria")
  # 
  
  Results_import <- tbl(IR.sql, 'VW_BioCriteria') |> 
    collect()
  
  DBI::dbDisconnect(IR.sql)
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Data censoring --------------------------------------------------------------------------------------------------
  
  # There is no need for data review or censored data ?? 
  
  return(Results_import)
  
}

