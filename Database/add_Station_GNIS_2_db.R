# This script pulls in the OR_AUS lineowrk feature layer which is the central repository for AU info by reach code. 
# It adds this dataframe to an in process, non persisteant duckdb database and summarizes by reachcode to get the final
# AU_GNIS_Name. ResultsRawWater station and reachcode is brought in and jhoined to this table to get AU_GNIS name for 
# each station. This data is then brought into the IR database to be used in the InputRaw View to give AU_GNIS name to
# each station.

library(tidyverse)
library(arcgisbinding)

url <- 'https://services.arcgis.com/uUvqNMGPm7axC2dD/ArcGIS/rest/services/OR_AUs_Line_work/FeatureServer/1'
savepath <- 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2026/Database/'


# Get GNIS info from published flowlines layer -------------------------------------------------------------------

print("Check ArcGIS license")
  # Initialize ArcGIS license
arcgisbinding::arc.check_product()


#Pull data from published feature layer

print("Begin flowlines query. This process make take some time")

a <- Sys.time()

OR_flowlines <- arcgisbinding::arc.data2sf(arcgisbinding::arc.select(arcgisbinding::arc.open(url), fields = c("ReachCode", 'AU_ID', 'AU_GNIS_Name'))) |>
  as.data.frame() |> 
  select(ReachCode, AU_ID,AU_GNIS_Name )

Sys.time() - a


print("Finish flow lines query")

# save(OR_flowlines, file = "OR_Flowlines.RData")
#
# load("OR_Flowlines.RData")
# 
# OR_flowlines <- OR_flowlines |>
#   select(ReachCode, AU_ID, AU_GNIS_Name)


# Summarize by reachcode to get single reachcode/GNIS value
# This was taking >2 hours on my R sessions, So I moved the processing to an in memory, non persistent duckdb database

a <-  Sys.time()
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
table_name <- "flowlines"

duckdb::dbWriteTable(con, table_name, OR_flowlines)
OR_flowlines_collapsed <- tbl(con, "flowlines") |>
  group_by(ReachCode) |>
  filter(!is.na(AU_GNIS_Name)) |>
  summarise(AU_GNIS_Name = first(AU_GNIS_Name)) |>
  mutate(AU_GNIS_Name = dplyr::case_when(AU_GNIS_Name == " " ~ NA_character_,
                                         TRUE ~ AU_GNIS_Name)) |>
  collect()



Sys.time() - a

DBI::dbDisconnect(con)

# Pull MLocID from ResultsRawWater --------------------------------------------------------------------------------


con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

#Pull station and reachcode to use in join
station_reachcode <- dplyr::tbl(con, "ResultsRawWater") |>
  dplyr::select(AU_ID, MLocID, Reachcode ) |>
  dplyr::distinct() |>
  dplyr::collect()


# Join Station Info to flowlines GNIS info ------------------------------------------------------------------------

Station_AU_GNIS <- station_reachcode |>
  dplyr::filter(!is.na(Reachcode)) |>
  dplyr::left_join(OR_flowlines_collapsed, by =c('Reachcode' = 'ReachCode')) |>
  dplyr::mutate(Station_Comment = NA_character_)



# Write Excel of mlocs with no GNIS info toupdate flowlines.  -----------------------------------------------------


missing_GNIS <- Station_AU_GNIS |>
  dplyr::filter(is.na(AU_GNIS_Name))


openxlsx::write.xlsx(missing_GNIS, paste0(savepath, 'database_missingGNIS_', Sys.Date(), '.xlsx'))


# IR DB SQL -------------------------------------------------------------------------------------------------------

Station_AU_GNIS <- Station_AU_GNIS |>
  select(-AU_ID)
#Drop and write new table
#DBI::dbRemoveTable(con, "Station_AU_GNIS_Name")
DBI::dbExecute(con, "DELETE FROM Station_AU_GNIS_Name")
DBI::dbWriteTable(con, "Station_AU_GNIS_Name", Station_AU_GNIS, append = TRUE)

DBI::dbDisconnect(con)


# 
# con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
# 
# inputRaw_mloc_GNIS <-  dplyr::tbl(con, "InputRaw") |> 
#   select(AU_ID, )
#   