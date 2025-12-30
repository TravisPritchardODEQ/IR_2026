library(tidyverse)
library(lubridate)
library(odeqIRtools)


source('Parameters/DO/fun_DO_data.R')
source("Parameters/DO/fun_DO_year_round_analysis.R")
source("Parameters/DO/fun_DO_spawn_analysis.R")




Results_censored_DO <- DO_data('IR_Dev')


DO_Year_Round <- fun_DO_year_round(df = Results_censored_DO, write_excel = FALSE)

DO_Spawn <- fun_DO_spawn(df = Results_censored_DO, write_excel = FALSE)



# Put it together -------------------------------------------------------------------------------------------------

DO_Year_Round$cont_data <- DO_Year_Round$cont_data |> 
  mutate(SpawnStart = as.character(SpawnStart),
         SpawnEnd = as.character(SpawnEnd))
DO_Spawn$cont_data <- DO_Spawn$cont_data |> 
  mutate(SpawnStart = as.character(SpawnStart),
         SpawnEnd = as.character(SpawnEnd))


DO_Year_Round$inst_data <- DO_Year_Round$inst_data |> 
  mutate(act_depth_height = as.character(act_depth_height))

DO_Spawn$inst_data <- DO_Spawn$inst_data |> 
  mutate(act_depth_height = as.character(act_depth_height))


AU_display <- bind_rows(DO_Year_Round$AU_display, DO_Spawn$AU_display) |> 
  arrange(AU_ID)

Other_AU_categorization <- bind_rows(DO_Year_Round$Other_AU_categorization, DO_Spawn$Other_AU_categorization)|> 
  arrange(AU_ID)

WS_GNIS_categorization <- bind_rows(DO_Year_Round$WS_GNIS_categorization, DO_Spawn$WS_GNIS_categorization)|> 
  arrange(AU_ID)

WS_station_categorization <- bind_rows(DO_Year_Round$WS_station_categorization, DO_Spawn$WS_station_categorization)|> 
  arrange(AU_ID)



# Write Excel -----------------------------------------------------------------------------------------------------

# Truncate values

# 5 less than excel max character
truncate_characters <- 3000 - 5

AU_display <- AU_display |> 
  mutate(across(where(is.character), ~ str_trunc(., width = truncate_characters, ellipsis = "...")))



wb <- createWorkbook()

addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')

addWorksheet(wb, sheetName = "Other_AU_categorization"   ,tabColour = 'dodgerblue3' )
addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')

addWorksheet(wb, sheetName = "DO Data Inst yearround",     tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "DO Data Cont yearround",     tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "DO Data Inst spawn",         tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "DO Data Cont spawn",         tabColour = 'paleturquoise2')




header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)

writeData(wb = wb, sheet = "Other_AU_categorization", x = Other_AU_categorization, headerStyle = header_st)
writeData(wb = wb, sheet = "WS station categorization", x = WS_station_categorization, headerStyle = header_st)
writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_categorization, headerStyle = header_st)


writeData(wb = wb, sheet = "DO Data Inst yearround",   x = DO_Year_Round$inst_data, headerStyle = header_st )
writeData(wb = wb, sheet = "DO Data Cont yearround",   x = DO_Year_Round$cont_data, headerStyle = header_st )
writeData(wb = wb, sheet = "DO Data Inst spawn",       x = DO_Spawn$inst_data, headerStyle = header_st )
writeData(wb = wb, sheet = "DO Data Cont spawn",       x = DO_Spawn$cont_data, headerStyle = header_st )


print("Writing excel doc")
saveWorkbook(wb, paste0("Parameters/Outputs/DO-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
