
## Write to database -------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), 'IR_dev')


exlcude_Mloc <- tbl(con, "Unused_Stations") |> 
  collect()

ResultsRawWater_missing_AUID <- tbl(con, "ResultsRawWater") |> 
  filter(!MLocID %in%  exlcude_Mloc$MLocID) |> 
  select(OrganizationID, MLocID, AU_ID) |> 
  distinct() |> 
  collect()



ResultsRawCont_pH_missing_AUID <- tbl(con, "ResultsRawCont_pH") |> 
  filter(!MLocID %in%  exlcude_Mloc$MLocID) |> 
  select(OrganizationID, MLocID, AU_ID) |> 
  distinct() |> 
  collect()



ResultsRawBioIndex_missing_AUID <- tbl(con, "ResultsRawBioIndex") |> 
  filter(!MLocID %in%  exlcude_Mloc$MLocID) |> 
  select(org_id, MLocID, AU_ID) |> 
  rename(OrganizationID = org_id) |> 
  distinct() |> 
  collect()

ResultsRawMarine_pH_missing_AUID <- tbl(con, "ResultsRawMarine") |> 
  filter(!MLocID %in%  exlcude_Mloc$MLocID) |> 
  select(OrganizationID, MLocID, AU_ID) |> 
  distinct() |> 
  collect()


df_list <- list(ResultsRawWater_missing_AUID, ResultsRawCont_pH_missing_AUID, 
                ResultsRawBioIndex_missing_AUID,ResultsRawMarine_pH_missing_AUID )

all_stations_2026IR <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) |> 
  distinct()
#openxlsx::write.xlsx(all_stations_2026IR, file = 'all_stations_2026IR_exlcude.xlsx')

missing_AU <- all_stations_2026IR |> 
  filter(is.na(AU_ID))

