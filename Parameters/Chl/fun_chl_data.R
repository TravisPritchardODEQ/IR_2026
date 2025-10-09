require(rgdal)
require(RODBC)
library(tidyverse)
library(odeqIRtools)




chla_data <- function(database) {
  print("Fetch Chl data from IR database")
  
  #connect to IR database view as a general user
  # import bacteria data
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  results_usgs_qry <-"SELECT dbo.IR_24_missingUSGSdata.OrganizationID, dbo.IR_24_missingUSGSdata.MLocID, dbo.IR_24_missingUSGSdata.StationDes, dbo.IR_24_missingUSGSdata.GNIS_Name, dbo.IR_24_missingUSGSdata.GNIS_Name as AU_GNIS_Name, dbo.IR_24_missingUSGSdata.MonLocType, dbo.IR_24_missingUSGSdata.HUC12_Name, dbo.IR_24_missingUSGSdata.ELEV_Ft, 
                  dbo.IR_24_missingUSGSdata.AU_ID, dbo.IR_24_missingUSGSdata.WaterTypeCode, dbo.IR_24_missingUSGSdata.WaterBodyCode, dbo.IR_24_missingUSGSdata.ben_use_code, dbo.IR_24_missingUSGSdata.OWRD_Basin, dbo.IR_24_missingUSGSdata.wqstd_code, dbo.IR_24_missingUSGSdata.Pollu_ID, dbo.IR_24_missingUSGSdata.Char_Name, 
                  dbo.IR_24_missingUSGSdata.chr_uid, dbo.IR_24_missingUSGSdata.SampleMedia, dbo.IR_24_missingUSGSdata.SampleSubmedia, dbo.IR_24_missingUSGSdata.Result_status, dbo.IR_24_missingUSGSdata.SampleStartDate, dbo.IR_24_missingUSGSdata.SampleStartTime, dbo.IR_24_missingUSGSdata.SampleStartTZ, 
                  dbo.IR_24_missingUSGSdata.act_depth_height, dbo.IR_24_missingUSGSdata.ActDepthUnit, dbo.IR_24_missingUSGSdata.Result_Type, dbo.IR_24_missingUSGSdata.Result_Numeric, dbo.IR_24_missingUSGSdata.Result_Unit, dbo.IR_24_missingUSGSdata.IRResultNWQSunit, dbo.IR_24_missingUSGSdata.Result_Operator, 
                  dbo.IR_24_missingUSGSdata.IRWQSUnitName, dbo.IR_24_missingUSGSdata.Result_UID, dbo.IR_24_missingUSGSdata.lab_Comments, dbo.IR_24_missingUSGSdata.General_Comments, dbo.IR_24_missingUSGSdata.QualifierAbbr, dbo.IR_24_missingUSGSdata.QualifierTxt, dbo.Crit_Chla.Chla_Criteria
FROM     dbo.IR_24_missingUSGSdata INNER JOIN
                  dbo.Crit_Chla ON dbo.IR_24_missingUSGSdata.MonLocType = dbo.Crit_Chla.MonLocType
WHERE  (dbo.IR_24_missingUSGSdata.wqstd_code = 17) AND (dbo.IR_24_missingUSGSdata.Result_Unit <> 'mg/cm2')" 
  
  results_usgs <- dbGetQuery(con,results_usgs_qry) |> 
    mutate(act_depth_height = as.numeric(act_depth_height))
  
  new_aus <- unique(results_usgs$AU_ID)  
  
  # connect to IR database view as a general user
  # import TOXHH data
  dbDisconnect(con)
  
  
  
  
  
  #connect to IR database view as a general user
  # import Temperature data
  IR.sql <-   odbcConnect(database)
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    sqlFetch(IR.sql, "dbo.VW_Chl") |> 
    filter(AU_ID %in% results_usgs)
  
  Results_import <- bind_rows(Results_import,results_usgs )
  
  
  odbcClose(IR.sql)
  
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  
  Results_import <- odeqIRtools::data_aggregation(Results_import)
  
  
  # Data censoring --------------------------------------------------------------------------------------------------
  
  print("Data censor process")
  # Get all the standards to be used when dealing with the censored data
  
  
  Results_censored <- censor_data(Results_import, crit = Chla_Criteria)
  
  
  
  return(Results_censored)
  
}