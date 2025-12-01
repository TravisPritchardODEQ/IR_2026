# This script pulls in the OR_AUS lineowrk feature layer which is the central repository for AU info by reach code. 
# It adds this dataframe to an in process, non persisteant duckdb database and summarizes by reachcode to get the final
# AU_GNIS_Name. ResultsRawWater station and reachcode is brought in and jhoined to this table to get AU_GNIS name for 
# each station. This data is then brought into the IR database to be used in the InputRaw View to give AU_GNIS name to
# each station.

library(tidyverse)
library(arcgisbinding)

url <- 'https://services.arcgis.com/uUvqNMGPm7axC2dD/arcgis/rest/services/Oregon_AUs/FeatureServer/0'
savepath <- 'C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Database/'


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
station_reachcode_rawwater <- dplyr::tbl(con, "ResultsRawWater") |>
  dplyr::select(AU_ID, MLocID, Reachcode ) |>
  dplyr::distinct() |>
  dplyr::collect()


station_reachcode_bio <- dplyr::tbl(con, "ResultsRawBioIndex") |>
  dplyr::select(AU_ID, MLocID, Reachcode ) |>
  dplyr::distinct() |>
  dplyr::collect()

station_reachcode_pH <- dplyr::tbl(con, "ResultsRawCont_pH") |>
  dplyr::select(AU_ID, MLocID, Reachcode ) |>
  dplyr::distinct() |>
  dplyr::collect()


station_reachcode_marine <- dplyr::tbl(con, "ResultsRawMarine") |>
  dplyr::select(AU_ID, MLocID, Reachcode ) |>
  dplyr::distinct() |>
  dplyr::collect()



station_reachcode <- bind_rows(station_reachcode_rawwater, station_reachcode_bio) |>
  bind_rows(station_reachcode_pH) |> 
  bind_rows(station_reachcode_marine) |> 
  distinct()


exclusions <- DBI::dbReadTable(con, 'Unused_Stations')


station_reachcode <- station_reachcode |> 
  filter(!MLocID %in% exclusions$MLocID)
# Join Station Info to flowlines GNIS info ------------------------------------------------------------------------

Station_AU_GNIS <- station_reachcode |>
  dplyr::filter(!is.na(Reachcode)) |>
  dplyr::left_join(OR_flowlines_collapsed, by =c('Reachcode' = 'ReachCode')) |>
  dplyr::mutate(Station_Comment = NA_character_)



# Write Excel of mlocs with no GNIS info toupdate flowlines.  -----------------------------------------------------


missing_GNIS <- Station_AU_GNIS |>
  dplyr::filter(is.na(AU_GNIS_Name),
                AU_ID != '99')


openxlsx::write.xlsx(missing_GNIS, paste0(savepath, 'database_missingGNIS_', Sys.Date(), '.xlsx'))


# IR DB SQL -------------------------------------------------------------------------------------------------------

Station_AU_GNIS <- Station_AU_GNIS |>
  select(-AU_ID)
#Drop and write new table
#DBI::dbRemoveTable(con, "Station_AU_GNIS_Name")
DBI::dbExecute(con, "DELETE FROM Station_AU_GNIS_Name")
DBI::dbWriteTable(con, "Station_AU_GNIS_Name", Station_AU_GNIS, overwrite = TRUE)

DBI::dbDisconnect(con)





# Standardize Results to Units --------------------------------------------

results_raw_water_string <- "Update [IntegratedReport].[dbo].[ResultsRawWater]
Set IRResultNWQSunit = 
Case when Unit_UID=IRpollunit_ID then Result_Numeric
	 When Unit_UID=1 then Result_Numeric
	 When Unit_UID=12 and IRPollUnit_ID = 14 then Result_Numeric*0.000001 --pg/L to ug/L
	 When Unit_UID=13 and IRPollUnit_ID = 14 then Result_Numeric*0.001 --ng/L to ug/L
	 When Unit_UID=14 and IRPollUnit_ID = 15 then Result_Numeric*0.001 --ug/L to mg/L
	 When Unit_UID=15 and IRPollUnit_ID = 14 then Result_Numeric*1000  --mg/L to ug/L
	 When Unit_UID=258 and IRPollUnit_ID = 15 then Result_Numeric --mg/l CaCO3 to mg/l - This is for USGS data
	 When Unit_UID=30 and IRPollUnit_ID = 15 then Result_Numeric --meq/l CaCO3 to mg/l - This is how city of eugene reported some of their - need to check conversion
	 When Unit_UID=91 and IRPollUnit_ID = 14 then Result_Numeric  --ppb to ug/L
	 When Unit_UID=92 and IRPollUnit_ID = 14 then Result_Numeric*1000  --ppm to ug/L
	 When Unit_UID=92 and IRPollUnit_ID = 15 then Result_Numeric  --ppm to mg/L
	 When Unit_UID=146 and IRPollUnit_ID = 164 then Result_Numeric  --MPN to cfu/100ml
	 When Unit_UID=151 and IRPollUnit_ID = 164 then Result_Numeric  --#/100ml to cfu/100ml
	 When Unit_UID=164 and IRPollUnit_ID = 164 then Result_Numeric  --cfu/100ml to cfu/100ml
	 When Unit_UID=247 and IRPollUnit_ID = 246 then ((Result_Numeric-32)*0.5556)  --deg F to deg C
	 When Unit_UID=262 and IRPollUnit_ID = 164 then Result_Numeric  --MPN/100ml to cfu/100ml
	 When Unit_UID=298 and IRPollUnit_ID = 103 then Result_Numeric  --FNU to NTU
	 When Unit_UID=299 and IRPollUnit_ID = 103 then Result_Numeric  --NTRU to NTU
	 When Unit_UID=354 and IRPollUnit_ID = 223 then Result_Numeric  --% saturatn to
	 When Unit_UID=261 and IRPollUnit_ID = 93 then Result_Numeric 	 -- ppt for salinity
	 When Unit_UID=31 and IRPollUnit_ID = 168 then Result_Numeric 	 -- ppt for salinity
	 When Unit_UID=32 and IRPollUnit_ID = 31 then Result_Numeric*1000  --mg/L to ug/L
	 When Unit_UID=60 and IRPollUnit_ID = 31 then Result_Numeric  --mg/L to ug/L
	 When Unit_UID=19 and IRPollUnit_ID = 15 then Result_Numeric*0.001 -- mg/m3 to mg/l
	 when Unit_UID=31 and IRPollUnit_ID is null then Result_Numeric --uS/cm specific conductivity used for aluminum calculator. No IRPollUnit_ID due to not having a sp conductance standard
Else -9999 End"

marine_string <-"Update [IntegratedReport].[dbo].[ResultsRawMarine]
Set IRResultNWQSunit = Result_Numeric,
IRWQSUnitName = Result_Unit"



con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


DBI::dbSendQuery(con, results_raw_water_string)

DBI::dbSendQuery(con, marine_string)

# 
# con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
# 
# inputRaw_mloc_GNIS <-  dplyr::tbl(con, "InputRaw") |> 
#   select(AU_ID, )
#   