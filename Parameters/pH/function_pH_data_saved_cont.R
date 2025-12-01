cont_ph_raw_saved_cont <- function(database) {
  
  #database <- "IR_Dev"
  require(tidyverse)
  # require(RODBC)
  require(odeqIRtools)
  
  print("Fetch continuous pH data from IR database")
  #connect to IR database view as a general user
  # import bacteria data
  
  options(scipen = 999)
  
  #connect to IR database view as a general user
  # import bacteria data
  # con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  #   
  #   results_usgs_qry <-"SELECT dbo.IR_24_missingUSGSdata.OrganizationID, dbo.IR_24_missingUSGSdata.MLocID, dbo.IR_24_missingUSGSdata.StationDes, dbo.IR_24_missingUSGSdata.GNIS_Name as AU_GNIS_Name, dbo.IR_24_missingUSGSdata.GNIS_Name, dbo.IR_24_missingUSGSdata.AU_ID, dbo.IR_24_missingUSGSdata.ben_use_code, dbo.IR_24_missingUSGSdata.MonLocType, 
  #                   dbo.IR_24_missingUSGSdata.wqstd_code, dbo.IR_24_missingUSGSdata.Pollu_ID, dbo.IR_24_missingUSGSdata.OWRD_Basin, dbo.IR_24_missingUSGSdata.SampleMedia, dbo.IR_24_missingUSGSdata.SampleSubmedia, dbo.IR_24_missingUSGSdata.Sample_Fraction, dbo.IR_24_missingUSGSdata.Activity_Type, dbo.IR_24_missingUSGSdata.act_depth_height, 
  #                   dbo.IR_24_missingUSGSdata.ActDepthUnit, dbo.IR_24_missingUSGSdata.SampleStartDate, dbo.IR_24_missingUSGSdata.SampleStartTime, dbo.IR_24_missingUSGSdata.SampleStartTZ, dbo.IR_24_missingUSGSdata.Char_Name, dbo.IR_24_missingUSGSdata.Result_status, dbo.IR_24_missingUSGSdata.Result_UID, dbo.IR_24_missingUSGSdata.Result_Type, 
  #                   dbo.IR_24_missingUSGSdata.Result_Numeric, dbo.IR_24_missingUSGSdata.Result_Unit, dbo.IR_24_missingUSGSdata.IRResultNWQSunit, dbo.IR_24_missingUSGSdata.Result_Operator, dbo.IR_24_missingUSGSdata.IRWQSUnitName, dbo.IR_24_missingUSGSdata.lab_Comments, dbo.IR_24_missingUSGSdata.General_Comments, 
  #                   dbo.IR_24_missingUSGSdata.QualifierAbbr, dbo.IR_24_missingUSGSdata.QualifierTxt, dbo.Crit_pH.pH_code, dbo.Crit_pH.pH_Min, dbo.Crit_pH.pH_Max, dbo.IR_24_missingUSGSdata.Statistical_Base
  # FROM     dbo.IR_24_missingUSGSdata INNER JOIN
  #                   dbo.Crit_pH ON dbo.IR_24_missingUSGSdata.pH_code = dbo.Crit_pH.pH_code
  # WHERE  (dbo.IR_24_missingUSGSdata.wqstd_code = 10) AND (dbo.IR_24_missingUSGSdata.Result_Type = 'Actual')" 
  #   
  #   results_usgs <- dbGetQuery(con,results_usgs_qry) |> 
  #     mutate(act_depth_height = as.numeric(act_depth_height))
  #   
  #   new_aus <- unique(results_usgs$AU_ID)
  
  
  
  # DBI::dbDisconnect(con)
  
  
  
  database = "IR_Dev"
  IR.sql <-  DBI::dbConnect(odbc::odbc(), database)
  

# Get continuous data -----------------------------------------------------

#This data hhas been exported oit of ssms  
  cont_pH_colnames_df <- data.frame(
    stringsAsFactors = FALSE,
    MLocID = c("DRA31617"),
    GNIS_Name = c("Deschutes River"),
    AU_GNIS_Name = c("Deschutes River"),
    StationDes = c("Deschutes River approx. 1 mi. d/s of Pelton/Rnd. Butte Re-Reg. Dam",
                   NA),
    MonLocType = c("River/Stream"),
    HUC12_Name = c("Pelton Dam-Deschutes River"),
    AU_ID = c("OR_SR_1707030603_05_102625"),
    WaterBodyCode = c(2L),
    ben_use_code = c(5L),
    wqstd_code = c(10L),
    Pollu_ID = c(124L),
    OWRD_Basin = c("Deschutes"),
    OrganizationID = c("DRA(NOSTORETID)"),
    Equipment_ID = c("15M101730"),
    Media = c("Water"),
    Sub_Media = c("Surface Water"),
    Char_Name = c("pH"),
    Depth_Unit = c("m"),
    Result_Date = c("2020-04-29"),
    Depth = c(1),
    Result_Time = c("12:00:00.0000000"),
    Result_Numeric = c(9.28),
    Operator = c("="),
    Result_Unit = c("None"),
    Result_Status = c("Accepted"),
    DQL = c("NULL"),
    Comments = c("NULL"),
    WaterTypeCode = c(2L),
    pH_code = c(6L),
    pH_Min = c(6.5),
    pH_Max = c(8.5)
  )
  
  print("fetch continuous")
  Results_import_cont <-read.csv('Parameters/pH/cont_pH_data.csv', col.names = colnames(cont_pH_colnames_df)) %>%
    dplyr::filter(!is.na(Result_Numeric)) 
  
  print("fetch grab")
  Results_import_grab <- tbl(IR.sql, "VW_pH")  %>%
    dplyr::filter(!is.na(Result_Numeric)) |> 
    collect()
  
  
  Results_import_grab 
  
  
  DBI::dbDisconnect(IR.sql)
  
  print(paste("Fetched", nrow(Results_import_cont), "continuous results from", length(unique(Results_import_cont$MLocID)), "monitoring locations" ))
  print(paste("Fetched", nrow(Results_import_grab), "grab results from", length(unique(Results_import_grab$MLocID)), "monitoring locations" ))
  
  # Set factors to characters
  Results_import_cont %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import_cont
  Results_import_grab %>% map_if(is.factor, as.character) %>% as_tibble -> Results_import_grab
  
  
  Results_import_grab  <- Results_import_grab|> 
    mutate(Result_UID = as.integer(Result_UID))
  
  Results_import_grab <- odeqIRtools::data_aggregation(Results_import_grab)
  
  # Data censoring --------------------------------------------------------------------------------------------------
  #LAM commented this out as we don't censor data for pH
  # print("Data censor process")
  # # Get all the standards to be used when dealing with the censored data
  # Results_crit <- Results_import_cont %>%
  #   # Get lowest criteria value to set censored results
  #   mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  # 
  # Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  # 
  # print("Data censor process")
  # # Get all the standards to be used when dealing with the censored data
  # Results_crit <- Results_import_grab %>%
  #   # Get lowest criteria value to set censored results
  #   mutate(lowest_crit = pmin(SS_Crit, Geomean_Crit, Perc_Crit, na.rm = TRUE))
  # 
  # Results_censored <- censor_data(Results_crit, crit = lowest_crit)
  # 
  # 
  # return(Results_censored)
  # 
  
  pH_list <- list(pH_cont = Results_import_cont,
                  ph_grab = Results_import_grab)
  
}