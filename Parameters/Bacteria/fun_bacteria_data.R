

Bacteria_data <- function(database) {
  
  
  require(tidyverse)
  # require(RODBC)
  require(odeqIRtools)

  
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
#   
#   print("Fetch bacteria data from IR database")
#   test <- dbGetQuery(con,'SELECT dbo.IR_24_missingUSGSdata.OrganizationID, dbo.IR_24_missingUSGSdata.MLocID, dbo.IR_24_missingUSGSdata.StationDes, dbo.IR_24_missingUSGSdata.GNIS_Name, dbo.IR_24_missingUSGSdata.GNIS_Name as AU_GNIS_Name, dbo.IR_24_missingUSGSdata.MonLocType, dbo.IR_24_missingUSGSdata.HUC12_Name, dbo.IR_24_missingUSGSdata.ELEV_Ft, 
#                   dbo.IR_24_missingUSGSdata.AU_ID, dbo.IR_24_missingUSGSdata.WaterTypeCode, dbo.IR_24_missingUSGSdata.WaterBodyCode, dbo.IR_24_missingUSGSdata.ben_use_code, dbo.IR_24_missingUSGSdata.OWRD_Basin, dbo.IR_24_missingUSGSdata.wqstd_code, dbo.IR_24_missingUSGSdata.Pollu_ID, dbo.Crit_Bact.SS_Crit, 
#                   dbo.Crit_Bact.Geomean_Crit, dbo.Crit_Bact.Perc_Crit, dbo.IR_24_missingUSGSdata.Char_Name, dbo.IR_24_missingUSGSdata.SampleMedia, dbo.IR_24_missingUSGSdata.SampleSubmedia, dbo.IR_24_missingUSGSdata.Sample_Fraction, dbo.IR_24_missingUSGSdata.Result_status, 
#                   dbo.IR_24_missingUSGSdata.SampleStartDate, dbo.IR_24_missingUSGSdata.SampleStartTime, dbo.IR_24_missingUSGSdata.SampleStartTZ, dbo.IR_24_missingUSGSdata.act_depth_height, dbo.IR_24_missingUSGSdata.ActDepthUnit, dbo.IR_24_missingUSGSdata.Result_UID, dbo.IR_24_missingUSGSdata.Result_Type, 
#                   dbo.IR_24_missingUSGSdata.Result_Numeric, dbo.IR_24_missingUSGSdata.Result_Unit, dbo.IR_24_missingUSGSdata.Result_Operator, dbo.IR_24_missingUSGSdata.IRResultNWQSunit, dbo.IR_24_missingUSGSdata.IRWQSUnitName, dbo.IR_24_missingUSGSdata.lab_Comments, dbo.IR_24_missingUSGSdata.General_Comments, 
#                   dbo.IR_24_missingUSGSdata.QualifierAbbr, dbo.IR_24_missingUSGSdata.QualifierTxt, dbo.Crit_Bact.Bacteria_code
# FROM     dbo.IR_24_missingUSGSdata INNER JOIN
#                   dbo.Crit_Bact ON dbo.IR_24_missingUSGSdata.BacteriaCode = dbo.Crit_Bact.Bacteria_code
# WHERE  (dbo.IR_24_missingUSGSdata.wqstd_code = 1)
# ')
#   
#   
#   AUs <- unique(test$AU_ID)
#   
#   
#   irdb <- tbl(con, 'VW_Bacteria') |> 
#     #filter(AU_ID %in% AUs) |> 
#     collect()
  
  
  
    Results_import <-  tbl(con, 'VW_Bacteria') |> 
      #filter(AU_ID %in% AUs) |> 
      collect()
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import 
  Results_import <- Results_import |> 
    mutate(Result_UID = as.character(Result_UID)) |> 
    mutate(SampleStartDate = lubridate::ymd(SampleStartDate))  
  
  Results_import <- Results_import |>
    mutate(Result_UID = as.numeric(Result_UID))
  
  Results_import <- odeqIRtools::data_aggregation(Results_import)
  

# Data censoring --------------------------------------------------------------------------------------------------
  
  print("Data censor process")
  # Get all the standards to be used when dealing with the censored data
  Results_crit <- Results_import %>%
    # Get lowest criteria value to set censored results
    mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  
Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  
  
  return(Results_censored)
  
}
