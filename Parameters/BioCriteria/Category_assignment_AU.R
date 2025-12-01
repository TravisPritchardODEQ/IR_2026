### script to apply new 2026 biocrtieria AM

#modified by TP


###The rollup components will need to change for 2028. This function
#automatically passes the new assessment forward. Future assessments will 
#need to evaluate delisting the same as everywhere else


library(tidyverse)
library(openxlsx)




biocriteria_assessment <- function(df_data, results_low_count, write_excel = TRUE){

  
  
  # Testing -----------------------------------------------------------------
  # df_data = Results_import

  #
  
# Prep data ---------------------------------------------------------------


biocriteria_wide <- df_data |> 
  mutate(Score = as.numeric(Score)) |> 
  mutate(Act_id = str_remove(Act_id, "\\:[^:]*$")) |> 
  pivot_wider(names_from = Index_Name,
              values_from = Score) |> 
  rename(OE = `O/E Ratio`)


low_count_wide <-  results_low_count |> 
  mutate(Score = as.numeric(Score)) |> 
  mutate(Act_id = str_remove(Act_id, "\\:[^:]*$")) |> 
  pivot_wider(names_from = Index_Name,
              values_from = Score) |> 
  rename(OE = `O/E Ratio`)

source('Parameters/BioCriteria/custom_functions.R')

bioassess_assessment_fun <- function(df_data = joined_OE_BCG_MMI_good, AU_type){
  
  


  if(AU_type == "other"){  
    group1 <- c('AU_ID')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'AU_GNIS_Name')
    inverse <- FALSE
  }
  


  averages <- biocriteria_wide %>% 
    filter(str_detect(AU_ID, "WS", negate = inverse)) |> 
    group_by_at(group1) %>% 
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              n= n(),
              n_distinct_days = n_distinct(Sample_Date),
              MMI_AU_avg=mean(MMI, na.rm = TRUE), 
              OE_AU_avg=mean(OE, na.rm = TRUE),
              years =stringr::str_c(sort(unique(year(Sample_Date))), collapse = "; "))
  
  averages_low_count <- low_count_wide %>% 
    filter(str_detect(AU_ID, "WS", negate = inverse)) |> 
    group_by_at(group1) %>% 
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              n= n(),
              n_distinct_days = n_distinct(Sample_Date),
              MMI_AU_avg=mean(MMI, na.rm = TRUE), 
              OE_AU_avg=mean(OE, na.rm = TRUE),
              years =stringr::str_c(unique(year(Sample_Date)), collapse = "; "))
  
  
  category <- averages %>% 
    mutate(IR_category = case_when(n_distinct_days == 1 ~ '3' ,
                                   
                                   OE_AU_avg <= 0.79 &  MMI_AU_avg <= 0.81 ~ '5',
                                   
                                   OE_AU_avg <= 0.79 & MMI_AU_avg > 0.81 ~ '3B',
                                   
                                   OE_AU_avg > 0.79 & OE_AU_avg <= 0.91 &
                                     MMI_AU_avg <= 0.81 ~ '3B',
                                   
                                   OE_AU_avg > 0.79 & OE_AU_avg <= 0.91 &
                                     MMI_AU_avg > 0.81 ~ '3C',
                                   
                                   OE_AU_avg > 0.91 & 
                                     MMI_AU_avg <= 0.81 ~ '3B',
                                   
                                   OE_AU_avg > 0.91 & 
                                     MMI_AU_avg > 0.81 & MMI_AU_avg <= 0.90 ~ '3C',
                                   
                                   OE_AU_avg > 0.91 & 
                                     MMI_AU_avg > 0.90 ~ '2',
                                   
                                   TRUE ~ "ERROR")) |> 
    mutate(IR_category = factor(IR_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE),
           Rationale = case_when(n_distinct_days == 1 ~  paste0("Only 1 valid sample in data window. OE = ", round(OE_AU_avg, 2), ". MMI = ", round(MMI_AU_avg, 2) ),
                                 TRUE ~ paste0("OE average = ", round(OE_AU_avg, 2), ". MMI average = ", round(MMI_AU_avg, 2) )))    |> 
    mutate(Pollu_ID = '156',
           wqstd_code = '5',
           Char_Name = 'BioCriteria', 
           period = NA_character_) |> 
    mutate(Delist_eligability = case_when(n_distinct_days > 1 & IR_category == '2'  ~ 1,
                                          TRUE ~ 0)) 
  
  category_low_count <- averages_low_count %>% 
    mutate(IR_category = '3' ) |> 
    mutate(IR_category = factor(IR_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE),
           Rationale = case_when(n_distinct_days == 1 ~  paste0("Sample has < 300 individuals. Only 1 sample in data window. OE = ", round(OE_AU_avg, 2), ". MMI = ", round(MMI_AU_avg, 2) ),
                                 TRUE ~ paste0("All Samples have < 300 individuals. OE average = ", round(OE_AU_avg, 2), ". MMI average = ", round(MMI_AU_avg, 2) )))    |> 
    mutate(Pollu_ID = '156',
           wqstd_code = '5',
           Char_Name = 'BioCriteria', 
           period = NA_character_) |> 
    mutate(Delist_eligability = case_when(n_distinct_days > 1 & IR_category == '2'  ~ 1,
                                          TRUE ~ 0)) 
  
  
  if(AU_type == "other"){  
    Assessed_AUs <- unique(category$AU_ID)
    
    filtered_category_low_count <- category_low_count |> 
      filter(!AU_ID %in% Assessed_AUs)
    
    filtered_category_low_count_removed <- category_low_count |> 
      filter(AU_ID %in% Assessed_AUs)
    
    
    categorization <- bind_rows(category, filtered_category_low_count) |> 
      mutate(Rationale = case_when(AU_ID %in% filtered_category_low_count_removed$AU_ID ~ paste0(Rationale, '. One or more samples were excluded from analysis due to low sample counts.'),
                                   TRUE ~ Rationale))
      

    
    
  } else if (AU_type == "WS"){
   
    Assessed_AUs <- unique(category$AU_ID)
    Assessed_GNIS <- unique(category$AU_GNIS_Name)
    
    filtered_category_low_count <- category_low_count |> 
      filter(!(AU_ID %in% Assessed_AUs & AU_GNIS_Name %in% Assessed_GNIS))
    
    filtered_category_low_count_removed <- category_low_count |> 
      filter(AU_ID %in% Assessed_AUs & AU_GNIS_Name %in% Assessed_GNIS)
    
    
    
    
    categorization <- bind_rows(category, filtered_category_low_count)|> 
      mutate(Rationale = case_when(AU_ID %in% filtered_category_low_count_removed$AU_ID & AU_GNIS_Name %in% filtered_category_low_count_removed$AU_GNIS_Name ~ paste0(Rationale, '. One or more samples were excluded from analysis due to low sample counts.'),
                                   TRUE ~ Rationale))
    
  }
  
 
  return(categorization)
   
}

# This is the non watershed assessment grouped by AU
biocriteria_AU <-  bioassess_assessment_fun(df_data = Results_import, AU_type = "other")

#This is the watershed assessment by mloc ID
biocriteria_WS <- bioassess_assessment_fun(df_data = Results_import, AU_type = "WS")





WS_GNIS_rollup <- biocriteria_WS %>%
  # ungroup() %>%
  # group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  # summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
  #           IR_category_GNIS_24 = max(IR_category),
  #           Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
  #           Delist_eligability = max(Delist_eligability)) %>%
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  |> 
  ungroup() |> 
  select(-Char_Name, -IR_category) |> 
  transmute(AU_ID, 
            AU_GNIS_Name,
            Pollu_ID,
            wqstd_code,
            period,
            stations,
            IR_category_GNIS_26,
            Rationale_GNIS = Rationale,
            Delist_eligability,
            recordID)


WS_GNIS_rollup <- join_prev_assessments_biocriteria(WS_GNIS_rollup, AU_type = "WS")



WS_GNIS_rollup_delist <- assess_delist_biocriteria(WS_GNIS_rollup, type = 'WS') |> 
  mutate(Char_Name = "BioCriteria") |> 
  relocate(Char_Name, .after = AU_GNIS_Name)


## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <-  rollup_WS_AU_biocriteria(WS_GNIS_rollup_delist, char_name_field = Char_Name)


WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) |> 
  mutate(prev_AU_category = case_when(is.na(prev_AU_category) ~ "Unassessed",
                                      TRUE ~ prev_AU_category))





# Deal wit other units ----------------------------------------------------



other_category <- join_prev_assessments_biocriteria(biocriteria_AU, AU_type = 'Other') 


other_category_delist <- assess_delist_biocriteria(other_category, type = "Other") |> 
  rename(IR_category_26 = IR_category ) |> 
  mutate(assessed_cat = IR_category_26) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                 TRUE ~  Year_listed))







# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, status_change, IR_category_26, Year_listed,  year_last_assessed) |> 
  rename(assessed_cat = IR_category_26)


AU_display_ws <- WS_AU_rollup_joined |> 
  mutate(assessed_cat = IR_category_AU_26) |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)


AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale)) |> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                 TRUE ~  Year_listed)) |> 
  mutate(status_change = case_when(final_AU_cat == '4A' & prev_category == '4A' & year_last_assessed == '2026' ~ 'No change in status- Assessed',
                                   final_AU_cat == '4A' & prev_category == '4A' & year_last_assessed != '2026' ~ 'No change in status- No new assessment',
                                   TRUE ~ status_change))



WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category) 




# raw data prep -----------------------------------------------------------

stream_river_data_2_exclude <-  AU_display |> 
  filter(str_detect(AU_ID, "WS", negate = TRUE)) |> 
  filter(str_detect(Rationale, "One or more samples were excluded from analysis due to low sample counts.")) |> 
  select(AU_ID) |> 
  mutate(data_exlcude_SR = 1)


WS_data_2_exlcude <- WS_GNIS_rollup_delist |> 
  filter(str_detect(Rationale_GNIS, "One or more samples were excluded from analysis due to low sample counts.")) |>
  select(AU_ID, AU_GNIS_Name)|> 
  mutate(data_exlcude_WS = 1)
  

low_count_wide2 <- low_count_wide |> 
  left_join(stream_river_data_2_exclude) |> 
  left_join(WS_data_2_exlcude, by = join_by(AU_ID, AU_GNIS_Name)) |> 
  mutate(Assessment_comment = case_when(data_exlcude_SR == 1 | data_exlcude_WS == 1 
                                        ~ "Not included in assessment due to low sample count. Other samples meet minimum individual counts and were used instead.")) |> 
  select(-data_exlcude_SR, -data_exlcude_WS)


biocriteria_wide <- bind_rows(biocriteria_wide, low_count_wide2)


# Write excel docs --------------------------------------------------------
if(write_excel){
wb <- createWorkbook()

addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')

addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')

addWorksheet(wb, sheetName = "Biocriteria Raw Data", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Biocriteria WS Data", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Biocriteria other Data", tabColour = 'paleturquoise2')


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)

writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
#writeData(wb = wb, sheet = "WS station categorization", x = WS_category, headerStyle = header_st)
writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)

writeData(wb = wb, sheet = "Biocriteria Raw Data", x = biocriteria_wide, headerStyle = header_st)
writeData(wb = wb, sheet = "Biocriteria WS Data", x = biocriteria_WS, headerStyle = header_st )
writeData(wb = wb, sheet = "Biocriteria other Data", x = biocriteria_AU, headerStyle = header_st )

print("Writing excel doc")
saveWorkbook(wb, paste0("Parameters/Outputs/biocriteria",Sys.Date(), ".xlsx"), overwrite = TRUE) 
}




}
