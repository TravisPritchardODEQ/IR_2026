library(openxlsx)
library(tidyverse)
library(odeqIRtools)



con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)

# Send query to database and return with the data
Char_rename <-  DBI::dbGetQuery(con, db_qry) |> 
  mutate(Pollu_ID = as.character(Pollu_ID))



file_path <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/Non_R_assessments/Non_R_assessments_2026_01_14.xlsx"


# Other catrgories ------------------------------------------------------------------------------------------------



other_AU_import <- read.xlsx(file_path,
                             sheet = 'Other_AU_categorization')


Other_categories <- other_AU_import |> 
  select(-prev_category, -Year_listed, -year_last_assessed, -prev_rationale,
         -final_AU_cat, status_change) |> 
  mutate(Delist_eligability = 0,
         period = as.character(period),
         stations = NA_character_,
         recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
  mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5", '5C' ), ordered=TRUE)) 


other_category <- join_prev_assessments(Other_categories, AU_type = 'Other') |> 
  filter(!(wqstd_code == 16 & IR_category == "Unassessed" ))

other_category_delist <-  assess_delist(other_category, type = "Other")



# Watershed -------------------------------------------------------------------------------------------------------


WS_GNIS_import <- read.xlsx(file_path,
                             sheet = 'WS GNIS categorization')

WS_GNIS_rollup <- WS_GNIS_import %>%
  select(-recordID,-prev_GNIS_category, -prev_GNIS_rationale,-final_GNIS_cat, 
         -prev_AU_category, -prev_AU_rationale, -status_change  ) |> 
  mutate(Delist_eligability =  0) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_24, levels=c('Unassessed', "3", "3B", "2", "5", "5C" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ),
         period = as.character(period),
         stations = NA_character_) 

WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")

### Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')


## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name)

# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)|> 
  select(-Char_Name) |> 
  left_join(Char_rename) |> 
  relocate(Char_Name, .after = AU_ID)

AU_display_ws <- WS_AU_rollup |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)


AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               TRUE ~ Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        .default = year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A', "5C") & is.na(Year_listed) ~ '2026',
                                 .default = NA_character_)) 



WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  




# Data sheets -----------------------------------------------------------------------------------------------------

RecreationsHabs_data <- read.xlsx(file_path,
          sheet = 'Data - Recreational HABs')






  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "RecreationsHabs_data", tabColour = 'paleturquoise2')
  
  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  #writeData(wb = wb, sheet = "WS station categorization", x = WS_categories, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
  
  writeData(wb = wb, sheet = "RecreationsHabs_data", x = RecreationsHabs_data, headerStyle = header_st)


  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/non_R-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  



