


chl_assessment <- function(df, write_excel = TRUE, database = 'IR_Dev'){

 # df <- Results_censored_chla
#  database = 'IR_Dev'


  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(runner)
  library(openxlsx)
  library(odeqIRtools)

  aus <- unique(df$AU_ID)

# Char rename -----------------------------------------------------------------------------------------------------
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  chars <- tbl(con, "LU_Pollutant") |> 
    select(Pollu_ID, `Pollutant_DEQ WQS`) |> 
    rename(Char_Name = `Pollutant_DEQ WQS`) |>
    mutate(Pollu_ID = as.character(Pollu_ID)) |> 
    collect()
  
    DBI::dbDisconnect(con)
  # Watershed units -------------------------------------------------------------------------------------------------


## calculate excursions --------------------------------------------------------------------------------------------



chla_data_month_ws <- df %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  mutate(yearmon = lubridate::floor_date(lubridate::as_date(SampleStartDate), "month")) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code,  yearmon, Chla_Criteria) %>%
  summarise(month_average = mean(Result_cen, na.rm = TRUE),
            month_n = n() ) %>%
  ungroup()

WS_3mo <- chla_data_month_ws %>%
  ungroup() |> 
  #filter(AU_ID == 'OR_WS_170703010301_05_102282') |> 
  group_by(AU_ID, MLocID, Chla_Criteria) %>%
  dplyr::mutate(d = runner(x = data.frame(yearmon  = ymd(yearmon),
                                          month_average = month_average,
                                          month = as.yearmon(yearmon, "%m/%Y")),
                           k = "3 months",
                           lag = 0,
                           idx = yearmon,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(avg_3mo = case_when(n_distinct(yearmon) == 3 ~ mean(month_average),
                                                                      TRUE ~ NA_real_),
                                                  months_3mo = paste(min(month), "-", max(month)) #str_c(month, collapse = ", ")
                                 )
  )) %>%
  tidyr::unnest_wider(d)


# Categorize mlocs ------------------------------------------------------------------------------------------------


WS_category <- WS_3mo %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code,  Chla_Criteria, yearmon) %>%
  mutate(month = as.yearmon(yearmon, "%m/%Y"),
         avg_3mo_excursion = case_when(is.na(avg_3mo) ~ 0,
                                      avg_3mo > Chla_Criteria ~ 1,
                                      TRUE ~ 0 ),
         num_month_avg = n()) %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Char_Name,  Pollu_ID, wqstd_code) %>%
  summarise(total_n = sum(month_n),
            num_monthly_avg = n_distinct(yearmon),
            num_3mo_avg_calculated = sum(!is.na(avg_3mo)),
            num_excur_3mo_avg = sum(avg_3mo_excursion),
            excur_3mo_avg_months = ifelse(max(avg_3mo_excursion, na.rm = TRUE) == 1,str_c(unique(months_3mo[avg_3mo_excursion == 1]), collapse = "; "),NA),
            num_monthly_excur = sum(month_average > Chla_Criteria),
            critical_excursions_month = binomial_excursions(num_monthly_avg, "Conventionals"),
            monthly_excur_month = str_c(unique(month[month_average > Chla_Criteria]), collapse = "; ")
            ) %>%
  ungroup() %>%
  mutate(IR_category = case_when(num_excur_3mo_avg >= 1 ~ "5",
                                 num_monthly_excur > critical_excursions_month  ~ "5",
                                 num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ "2",
                                 total_n >= 8 & num_monthly_excur < critical_excursions_month  ~ "2",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~ "3B",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ "3",
                                 TRUE ~ 'ERROR'
                                 ),
         Rationale =  case_when(num_excur_3mo_avg >= 1 ~ paste0(MLocID, ": ", "Impaired: ", num_excur_3mo_avg, " 3-month averages exceed critertia: ", 
                                                                excur_3mo_avg_months, ": ", 
                                                                total_n, " total samples"),
                                num_monthly_excur > critical_excursions_month ~ paste0(MLocID, ": ","Impaired: ", 
                                                                                       num_monthly_excur, " month averages exceed criteria: ",
                                                                                       monthly_excur_month, ": ", 
                                                                                       total_n, " total samples") ,
                                total_n >= 8 & num_monthly_excur < critical_excursions_month  ~  paste0("Attaining: ",  
                                                                                                        "< 10% (binomial) monthly ",
                                                                                                        "averages exceed criteria: ", 
                                                                                                        num_monthly_excur, 
                                                                                                        " excursions of ", total_n, 
                                                                                                        " total samples"),
                                num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ paste0(MLocID, "- Attaining: ", 
                                                                                             "0 3-month averages exceed criteria. < 10% (binomial) monthly ",
                                                                                             "averages exceed criteria: ", 
                                                                                             total_n, " total samples"),
                                num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~  paste0(MLocID, "- Insuffcient data: ", 
                                                                                              num_monthly_excur, " month averages exceed criteria: ",
                                                                                              monthly_excur_month, ". No 3-month averages calculated: ", 
                                                                                              total_n, " total samples"),
                                num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ paste0(MLocID, "- Insuffcient data: ", 
                                                                                              "0 month averages exceed criteria. No 3-month averages calculated: ", 
                                                                                              total_n, " total samples"),
                                TRUE ~ 'ERROR'
         )) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(period = NA_character_) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) |> 
  mutate(Delist_eligability = case_when(num_monthly_avg >= 18 & num_monthly_excur <= binomial_delisting(num_monthly_avg, 'Conventionals') 
                                        & num_3mo_avg_calculated > 1 & num_excur_3mo_avg == 0  ~ 1,
                                        TRUE ~ 0)) 


## GNIS rollup -----------------------------------------------------------------------------------------------------


WS_GNIS_rollup <- WS_category %>%
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            IR_category_GNIS_24 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  |> 
  ungroup() |> 
  select(-Char_Name)

WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")|> 
  filter(AU_ID %in% aus)


### Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS') |> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_GNIS_Name)


## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <-  rollup_WS_AU(WS_GNIS_rollup_delist, char_name_field = Char_Name)


WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) |> 
  ungroup() |> 
  select(-Char_Name) |> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)
# Other AUs -------------------------------------------------------------------------------------------------------




chla_data_month_other <- df %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  mutate(yearmon = lubridate::floor_date(lubridate::as_date(SampleStartDate), "month")) %>%
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code) %>%
  mutate(stations =  stringr::str_c(unique(MLocID), collapse = "; ")) |> 
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code,  yearmon, Chla_Criteria) %>%
  summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
            month_average = mean(Result_cen, na.rm = TRUE),
            month_n = n() ) %>%
  ungroup()

other_3mo <- chla_data_month_other %>%
  ungroup() |> 
  #filter(AU_ID == 'OR_LK_1707030101_05_100050') |> 
  group_by(AU_ID) %>%
  arrange(AU_ID, yearmon) |> 
  dplyr::mutate(d = runner(x = data.frame(yearmon  = ymd(yearmon),
                                          month_average = month_average,
                                          month = as.yearmon(yearmon, "%m/%Y")),
                           k = "3 months",
                           lag = 0,
                           idx = yearmon,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(avg_3mo = case_when(n_distinct(yearmon) == 3 ~ mean(month_average),
                                                                      TRUE ~ NA_real_),
                                                  months_3mo = paste(min(month), "-", max(month)) #str_c(month, collapse = ", ")
                                 )
  )) %>%
  tidyr::unnest_wider(d)


## Other AU categorization -----------------------------------------------------------------------------------------


other_category <- other_3mo %>%
  mutate(month = as.yearmon(yearmon, "%m/%Y"),
         avg_3mo_excursion = case_when(is.na(avg_3mo) ~ 0,
                                       avg_3mo > Chla_Criteria ~ 1,
                                       TRUE ~ 0 ),
         num_month_avg = n()) %>%
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code) %>%
  summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
            total_n = sum(month_n),
            num_monthly_avg = n_distinct(yearmon),
            num_3mo_avg_calculated = sum(!is.na(avg_3mo)),
            num_excur_3mo_avg = sum(avg_3mo_excursion),
            excur_3mo_avg_months = ifelse(max(avg_3mo_excursion, na.rm = TRUE) == 1,str_c(unique(months_3mo[avg_3mo_excursion == 1]), collapse = "; "),NA),
            num_monthly_excur = sum(month_average > Chla_Criteria),
            critical_excursions_month = binomial_excursions(num_monthly_avg, "Conventionals"),
            monthly_excur_month = str_c(unique(month[month_average > Chla_Criteria]), collapse = "; ")
  ) %>%
  ungroup() %>%
  mutate(IR_category = case_when(num_excur_3mo_avg >= 1 ~ "5",
                                 num_monthly_excur > critical_excursions_month  ~ "5",
                                 num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ "2",
                                 total_n >= 8 & num_monthly_excur < critical_excursions_month  ~ "2",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~ "3B",
                                 num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ "3",
                                 TRUE ~ 'ERROR'
  ),
  Rationale =  case_when(num_excur_3mo_avg >= 1 ~ paste0("Impaired: ", num_excur_3mo_avg, " 3-month averages exceed critertia: ", 
                                                         excur_3mo_avg_months, ": ", 
                                                         total_n, " total samples"),
                         num_monthly_excur > critical_excursions_month ~ paste0("Impaired: ", 
                                                                                num_monthly_excur, " month averages exceed criteria: ",
                                                                                monthly_excur_month, ": ", 
                                                                                total_n, " total samples") ,
                         total_n >= 8 & num_monthly_excur < critical_excursions_month  ~  paste0("Attaining: ",  
                                                                                                 "< 10% (binomial) monthly ",
                                                                                                 "averages exceed criteria: ", 
                                                                                                 num_monthly_excur, 
                                                                                                 " excursions of ", total_n, 
                                                                                                 " total samples"),
                         num_3mo_avg_calculated > 0 & num_excur_3mo_avg == 0 ~ paste0("Attaining: ", 
                                                                                      "0 3-month averages exceed criteria. < 10% (binomial) monthly ",
                                                                                      "averages exceed criteria: ", 
                                                                                      total_n, " total samples"),
                         num_3mo_avg_calculated == 0 & num_monthly_excur > 0 ~  paste0("Insuffcient data: ", 
                                                                                       num_monthly_excur, " month averages exceed criteria: ",
                                                                                       monthly_excur_month, ". No 3-month averages calculated: ", 
                                                                                       total_n, " total samples"),
                         num_3mo_avg_calculated == 0 & num_monthly_excur == 0 ~ paste0("Insuffcient data: ", 
                                                                                       "0 month averages exceed criteria. No 3-month averages calculated: ", 
                                                                                       total_n, " total samples"),
                         TRUE ~ 'ERROR'
  )) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(period = NA_character_) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) |> 
  mutate(Delist_eligability = case_when(num_monthly_avg >= 18 & num_monthly_excur <= binomial_delisting(num_monthly_avg, 'Conventionals') 
                                        & num_3mo_avg_calculated > 1 & num_excur_3mo_avg == 0  ~ 1,
                                        TRUE ~ 0)) 

other_category <- join_prev_assessments(other_category, AU_type = 'Other') |> 
  select(-Char_Name) |> 
  filter(AU_ID %in% aus)

other_category_delist <-  assess_delist(other_category, type = "Other")|> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)



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
                               .default = Rationale)) |> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2024",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2024',
                                 TRUE ~  Year_listed)) 
  

WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  


# Write excel docs ------------------------------------------------------------------------------------------------

if(write_excel){
  
  chla_data_month_ws <- chla_data_month_ws %>%
    mutate(yearmon = as.yearmon(yearmon, "%m/%Y"))
  
  
  chla_data_month_other <- chla_data_month_other %>%
    mutate(yearmon = as.yearmon(yearmon, "%m/%Y"))
  
  
  
  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "Chl-a Raw Data", tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "Chl-a WS Data", tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "Chl-a other Data", tabColour = 'paleturquoise2')
  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_category, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
 
   writeData(wb = wb, sheet = "Chl-a Raw Data", x = df, headerStyle = header_st)
  writeData(wb = wb, sheet = "Chl-a WS Data", x = chla_data_month_ws, headerStyle = header_st )
  writeData(wb = wb, sheet = "Chl-a other Data", x = chla_data_month_other, headerStyle = header_st )
  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/chl-a-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}

chl_assessment <- list(Chl_a_Raw_Data = as.data.frame(df),
                       Chl_a_WS_Data = as.data.frame(chla_data_month_ws),
                       chl_a_other_data = as.data.frame(chla_data_month_other),
                       WS_station_categorization = as.data.frame(WS_category),
                       WS_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                       Other_AU_categorization = as.data.frame(other_category_delist)) 

return(chl_assessment)

}
  