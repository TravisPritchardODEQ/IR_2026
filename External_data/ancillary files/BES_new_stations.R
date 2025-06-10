BES_stations <- BES[["sum_stats"]] |> 
  pull(Monitorining.Location.ID) |> 
  unique()

Stations_server <- Sys.getenv('STATIONS_SERVER')

stations_odbc="STATIONS"

query <- paste0("Select * from ", Stations_server, "[VWStationsFinal] where MLocID in ({BES_stations*})")

 
con <- DBI::dbConnect(odbc::odbc(), stations_odbc)
query <- glue::glue_sql(query, .con = con)
AWQMS_stations <- DBI::dbGetQuery(con, query)
DBI::dbDisconnect(con)

BES_Station_df <- BES[["sum_stats"]] |> 
  select(Monitorining.Location.ID) |> 
  unique()

new_stations <- BES_Station_df |> 
  anti_join(AWQMS_stations, by = c('Monitorining.Location.ID' = 'MLocID'))

unique(AWQMS_stations$MLocID)

PDX_BES-FC-8 = PDX_BES-FC8
PDX_BES-JC1 = PDX_BES-JC-1
PDX_BES-JC1B = 
PDX_BES-P2512 = 
PDX_BES-TC-4 = PDX_BES-TC4
PDX_BES-TC-5 = PDX_BES-TC5
PDX_BES-TC-6 = PDX_BES-TC6
