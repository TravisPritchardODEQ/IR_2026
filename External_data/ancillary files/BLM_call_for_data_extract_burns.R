# This script will process the .mdb access databases provided by the BLM as part of the 2024 IR Call For Data
# The data within the access database matches the tabs in the 24 CFD template
# This script will extract the data from .mdb and place it into an excel spreadsheet


library(RODBC)
library(openxlsx)
library(tidyverse)
library(odeqIRextdata)



# Set up splitting function
data_split_AWQMS_excel <- function(df, split_on, size, workbook) {
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  tempsplit <- split(df, df[[split_on]])
  length(tempsplit)
  datasplit <- list()
  
  j = 1
  for (i in 1:length(tempsplit)) {
    datasplit[[i]] <- dplyr::bind_rows(tempsplit[[i]])
    
    if (nrow(dplyr::bind_rows(datasplit)) > size | i == length(tempsplit)) {
      
      addWorksheet(workbook, sheetName = paste("Results-", j))
      writeData(wb = workbook, sheet = paste("Results-", j), x = dplyr::bind_rows(datasplit), headerStyle = header_st)
      
      
      datasplit <- NULL
      j <- j + 1
    }
  }
}

filepath <- 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/CallForData/2024/Submitted Data/Archive of Original Files/BLM/BLM_Data_NWOregon.mdb'
savepath <- 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/CallForData/2024/Submitted Data/Archive of Original Files/BLM/updated_submissions_082023/'

## Set up driver info and database path
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- filepath
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
channel <- odbcDriverConnect(PATH,
                             rows_at_time = 1)


# #List Tables
#  sqlTables(channel, errors = FALSE, as.is = TRUE,
#           catalog = NULL, schema = NULL, tableName = NULL,
#           tableType = NULL, literal = FALSE)



project <- sqlFetch(channel, "DEQ_PROJECT")
audit <- sqlFetch(channel, "DEQ_AUDIT")
deploy <- sqlFetch(channel, "DEQ_DEPLOY")
# Looks like some GIS related fields exist in the monloc table. This will remove them after import
monloc <- sqlFetch(channel, "DEQ_MONLOC") |> 
  select(1:16)
results <- sf::st_read("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/CallForData/2024/Submitted Data/Archive of Original Files/BLM/updated_submissions_082023/DEQ_RESULTS.gdb/DEQ_RESULTS.gdb/",
                       layer ='DEQ_RESULTS_NWO' )


odbcClose(channel)



header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Projects")
addWorksheet(wb, sheetName = "Monitoring_Locations")
addWorksheet(wb, sheetName = "Deployment_Info")
addWorksheet(wb, sheetName = "Audit_Data")
addWorksheet(wb, sheetName = "Results")

freezePane(wb, "Projects", firstRow = TRUE) 
freezePane(wb, "Monitoring_Locations", firstRow = TRUE)
freezePane(wb, "Deployment_Info", firstRow = TRUE)
freezePane(wb, "Audit_Data", firstRow = TRUE)
freezePane(wb, "Results", firstRow = TRUE)

writeData(wb = wb, sheet = "Projects", x = project, headerStyle = header_st)
writeData(wb = wb, sheet = "Monitoring_Locations", x = monloc, headerStyle = header_st)
writeData(wb = wb, sheet = "Deployment_Info", x = deploy, headerStyle = header_st)
writeData(wb = wb, sheet = "Audit_Data", x = audit, headerStyle = header_st)
#writeData(wb = wb, sheet = "Results", x = results, headerStyle = header_st)
data_split_AWQMS_excel(results, 'MONITORING_LOCATION_ID', size = 500000, workbook = wb)

saveWorkbook(wb, paste0(savepath,basename(tools::file_path_sans_ext(filepath)),'.xlsx'), overwrite = TRUE) 


