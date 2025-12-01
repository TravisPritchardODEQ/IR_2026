#library(rgdal)
#library(RODBC)
library(tidyverse)
library(odeqIRtools)
library(DBI)
HH_tox_data <- function(database) {
  

# testing ---------------------------------------------------------------------------------------------------------

database <- "IR_Dev"  
  
  print("Fetch HH Tox data from IR database")
 
  
  
  
  

# Database --------------------------------------------------------------------------------------------------------

  
  options(scipen = 999)
  #ALTox_data <- function(database) {
  print("Fetch AL Toxic data from IR database")
  
#   #connect to IR database view as a general user
#   # import bacteria data
#   con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
#   
#   results_usgs_qry <-"SELECT dbo.IR_24_missingUSGSdata.OrganizationID, dbo.IR_24_missingUSGSdata.MLocID, dbo.IR_24_missingUSGSdata.StationDes, dbo.IR_24_missingUSGSdata.GNIS_Name as AU_GNIS_Name, dbo.IR_24_missingUSGSdata.GNIS_Name, dbo.IR_24_missingUSGSdata.MonLocType, dbo.IR_24_missingUSGSdata.AU_ID, dbo.IR_24_missingUSGSdata.ben_use_code, 
#                   dbo.IR_24_missingUSGSdata.OWRD_Basin, dbo.IR_24_missingUSGSdata.wqstd_code, dbo.IR_24_missingUSGSdata.WaterBodyCode, dbo.IR_24_missingUSGSdata.WaterTypeCode, dbo.Crit_ToxHH.Pollutant, dbo.Crit_ToxHH.WaterOrganism, dbo.Crit_ToxHH.Organism, 
#                   dbo.Crit_ToxHH.Organism_SW, dbo.Crit_ToxHH.Fraction AS Crit_Fraction, dbo.IR_24_missingUSGSdata.Pollu_ID, dbo.IR_24_missingUSGSdata.SampleMedia, dbo.IR_24_missingUSGSdata.SampleSubmedia, dbo.IR_24_missingUSGSdata.SampleStartDate, dbo.IR_24_missingUSGSdata.SampleStartTime, 
#                   dbo.IR_24_missingUSGSdata.SampleStartTZ, dbo.IR_24_missingUSGSdata.Char_Name, dbo.IR_24_missingUSGSdata.chr_uid, dbo.IR_24_missingUSGSdata.Char_Speciation, dbo.IR_24_missingUSGSdata.Sample_Fraction, dbo.IR_24_missingUSGSdata.CASNumber, dbo.IR_24_missingUSGSdata.Result_UID, dbo.IR_24_missingUSGSdata.Result_status, 
#                   dbo.IR_24_missingUSGSdata.Result_Numeric, dbo.IR_24_missingUSGSdata.Result_Unit, dbo.IR_24_missingUSGSdata.Result_Type, dbo.IR_24_missingUSGSdata.IRResultNWQSunit, dbo.IR_24_missingUSGSdata.Result_Operator, dbo.IR_24_missingUSGSdata.IRWQSUnitName, dbo.IR_24_missingUSGSdata.act_depth_height, 
#                   dbo.IR_24_missingUSGSdata.ActDepthUnit, dbo.IR_24_missingUSGSdata.Statistical_Base, dbo.IR_24_missingUSGSdata.Time_Basis, dbo.IR_24_missingUSGSdata.lab_Comments, dbo.IR_24_missingUSGSdata.General_Comments, dbo.IR_24_missingUSGSdata.Analytical_method, dbo.IR_24_missingUSGSdata.QualifierAbbr, 
#                   dbo.IR_24_missingUSGSdata.QualifierTxt
# FROM     dbo.Crit_ToxHH INNER JOIN
#                   dbo.IR_24_missingUSGSdata ON dbo.Crit_ToxHH.Pollu_ID = dbo.IR_24_missingUSGSdata.Pollu_ID
# WHERE  (dbo.IR_24_missingUSGSdata.wqstd_code = 16) AND (dbo.IR_24_missingUSGSdata.AU_ID IS NOT NULL)" 
#   
#   results_usgs <- dbGetQuery(con,results_usgs_qry)
#   
#   results_usgs <- results_usgs |> 
#     mutate(act_depth_height = as.numeric(act_depth_height))
#   
#   new_aus <- unique(results_usgs$AU_ID)  
#   
#   # connect to IR database view as a general user
#   # import TOXHH data
#   dbDisconnect(con)
#   
  
  IR.sql <-   DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  
  # Get data from IR database where wqstd_code = 12, ResStatusName = Final, 
  # Join with Crit_Temp to get temperature Criteria and spawn ?
  Results_import <-    tbl(IR.sql, "VW_ToxHH") |> 
    collect()
  
  
  dbDisconnect(IR.sql)
  
  
  print(paste("Fetched", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations" ))
  
  
  
  # Set factors to characters
  Results_import %>% map_if(is.factor, as.character) %>% as_data_frame -> Results_import
  
  Results_import <- odeqIRtools::data_aggregation(Results_import)
  
  
  # choose the correct crit
  # if the ben-use code includes public or private water supply, select WaterOrganism
  # if ben-use does not include  public or private water supply, but does have fishing,  select Organism
  # if the characteristic has a salt water specific criteria, and the Water body code inidates salt water, 
  # and there is no drinking water, select Organism_SW 
  Results_import_crit <-  Results_import %>%
    mutate(crit = ifelse(ben_use_code %in% c('2','4','5','10','11','12', '16', '88'), WaterOrganism, 
                         ifelse(ben_use_code %in% c('1','3','6','7','8','9','13', '14', '15'), Organism, NA )),
           crit = ifelse(WaterBodyCode %in% c('1','3','4') & 
                           ben_use_code %in% c('1','3','6','7','8','9','13', '14', '15')&
                           !is.na(Organism_SW), Organism_SW, crit ))
  
  
  
  print("Modify censored data")
  
  #run the censored data function to set censored data. This will use the lowest crit value from above
  Results_censored <- censor_data(Results_import_crit, crit = `crit` ) %>%
    mutate(Result_cen = as.numeric(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$Result_cen)), "null values"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(Result_cen))
  
  print(paste("Removing", sum(is.na(Results_censored$crit)), "Results with no criteria"))
  
  Results_censored <- Results_censored %>%
    filter(!is.na(crit))
  
  
  print("Data fetch and censored data modifications complete")
  
  return(Results_censored)
  
}