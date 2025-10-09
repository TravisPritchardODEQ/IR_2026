library(openxlsx)


fun_turb_analysis <- function(df, write_excel = TRUE){

#df <- Results_censored_turb 


turb_data <- df %>%
  mutate(excursion = case_when(Result_cen >= Turb_Criteria ~ 1,
                               TRUE ~ 0)) 





turb_cat_fun <- function(df_data = turb_data, AU_type){
  
  
  if(AU_type == "other"){  
    group1 <- c('AU_ID', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Turb_Criteria', 'year' )
    group2 <- c('AU_ID',  'Pollu_ID', 'wqstd_code', 'Char_Name',  'OWRD_Basin')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Turb_Criteria', 'year' )
    group2 <- c('AU_ID','MLocID',  'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name',  'OWRD_Basin')
    inverse <- FALSE
  }
  
  
  


turb_assessment <- df_data %>%
  mutate(year = lubridate::year(SampleStartDate)) %>%
  filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
  group_by_at(group1) %>%
  #group_by(AU_ID,  Pollu_ID, wqstd_code,  OWRD_Basin, year) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            total_n = n(),
            total_excursions = sum(excursion),
            total_excursion_day = n_distinct(SampleStartDate[excursion == 1])) %>%
  ungroup() %>%
  group_by_at(group2) %>%
  summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
            IR_category = case_when(max(total_excursion_day) > 45 ~ "5",
                                    max(total_excursion_day) <= 45 ~ "2"),
            Rationale = case_when(max(total_excursion_day) > 45 ~ paste0("Impaired: ",str_c( year[total_excursion_day > 45], ': ', total_excursion_day[total_excursion_day > 45], " high turbidity days", collapse = "; ")),
                                  max(total_excursion_day) <= 45 ~ "Attaining: All years of data show 45 or less high turbidity days per year."))%>%
  mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))%>%
  mutate(period = NA_character_) |> 
  mutate(Delist_eligability = 0) 




}


# Watershed Assessment --------------------------------------------------------------------------------------------

Turb_WS <- turb_cat_fun(df_data = turb_data, AU_type = "WS")

## GNIS rollup -----------------------------------------------------------------------------------------------------


WS_GNIS_rollup <- Turb_WS %>%
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            IR_category_GNIS_24 = max(IR_category),
            Rationale_GNIS = str_c(str_unique(Rationale),collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS") |> 
  relocate(Char_Name, .after = AU_GNIS_Name) |> 
  mutate(Char_Name = 'Turbidity')



### Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')

## AU Rollup -------------------------------------------------------------------------------------------------------
WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name) 
WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) 

# Other assessment ------------------------------------------------------------------------------------------------

Turb_other <- turb_cat_fun(df_data = turb_data, AU_type = "other") 

other_category <- join_prev_assessments(Turb_other, AU_type = 'Other')|> 
  ungroup() |> 
  relocate(Char_Name, .after = AU_ID)

other_category_delist <-  assess_delist(other_category, type = "Other")  |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) 





# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup_joined |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_24,
         Rationale = Rationale_AU)

AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2024",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2024',
                                 TRUE ~ year_last_assessed)) |> 
  mutate(status_change = case_when(is.na(status_change) ~ paste(prev_category, "to", final_AU_cat),
                                   TRUE ~  status_change)) |> 
  mutate(Char_Name = 'Turbidity')

# Export ----------------------------------------------------------------------------------------------------------

turb <- list(data =turb_data,
                            AU_Decisions = AU_display,
                            Other_AU_categorization = other_category_delist,
                            WS_Station_cat = Turb_WS,
                            WS_GNIS_cat = WS_GNIS_rollup_delist)



if(write_excel){
  
  
  wb <- createWorkbook()
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "Turb Data", tabColour = 'paleturquoise2')
  
  freezePane(wb, "AU_Decisions",             firstRow = TRUE) 
  freezePane(wb, "Other_AU_categorization",   firstRow = TRUE) 
  freezePane(wb, "WS station categorization", firstRow = TRUE) 
  freezePane(wb, "WS GNIS categorization",    firstRow = TRUE) 
  freezePane(wb, "Turb Data",               firstRow = TRUE) 
  
  
  
  writeData(wb,  "AU_Decisions",                x = AU_display,               headerStyle = header_st) 
  writeData(wb,  "Other_AU_categorization",     x = other_category_delist,     headerStyle = header_st) 
  writeData(wb,  "WS station categorization",   x = Turb_WS,              headerStyle = header_st) 
  writeData(wb,  "WS GNIS categorization",      x = WS_GNIS_rollup_delist,  headerStyle = header_st) 
  writeData(wb,  "Turb Data",                 x = turb_data,  headerStyle = header_st) 
  
  
  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/turbidity-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}



return(turb)

}