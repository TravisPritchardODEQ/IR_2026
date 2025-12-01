## L.Merrick 8/5/2021 
#function to assess continuous data using the ten-ten 
#modifued By Travis Pritchard to fit the IR process



pH_assessment <- function(cont_data, grab_data, write_xlsx = TRUE){


# Testing and setup -----------------------------------------------------------------------------------------------

  # cont_data <- pH_cont
  # grab_data <- pH_grab
# Exclude continuous data dates from grab data --------------------------------------------------------------------

  
  #Get list of continuous data to exclude from grab data
  cont_data_to_exclude <- cont_data %>%
    rename(SampleStartDate  = Result_Date) %>%
    select(MLocID, SampleStartDate) %>%
    distinct() %>%
    mutate(has_cont_data = 1)
  
  #exclude continuous data from grab data
  pH_grab_no_cont <- grab_data %>%
    left_join(cont_data_to_exclude, by = c("MLocID", "SampleStartDate")) %>%
    #Keep grab if no continuous data on that same day
    filter(is.na(has_cont_data) | has_cont_data != 1) %>%
    select(-has_cont_data)
  
  
  aus <- unique(pH_grab_no_cont$AU_ID)
  
  
  
  pH_assessment_fun <- function(df_cont_data = cont_data, df_grab_data = pH_grab_no_cont,  AU_type){
    #testing
    #  df_cont_data = cont_data
    # df_grab_data= grab_data
    #  AU_type = "other"
    # # 
    
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name','GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Result_Date')
      group2 <-  c('AU_ID',  'Pollu_ID', 'wqstd_code', 'Char_Name')
      
      
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'GNIS_Name',  'Pollu_ID', 'wqstd_code', 'Char_Name', 'Result_Date' )
      group2 <- c('AU_ID', 'MLocID','AU_GNIS_Name', 'GNIS_Name',  'Pollu_ID', 'wqstd_code', 'Char_Name')
      
      inverse <- FALSE
    }
    
    
  
  
  
  cont_pH_ten_ten_data <- df_cont_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    mutate(pH_violation = ifelse(Result_Numeric < pH_Min | Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_Numeric < pH_Min, 1, 0 )) %>%
    group_by_at(group1) %>%
    summarise(daily_ten_num_Samples = n(),
              daily_ten_num_violation = sum(pH_violation),
              daily_ten_num_violation_high = sum(pH_violation_high),
              daily_ten_num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) %>%
    mutate(daily_ten_critical_excursions = binomial_excursions(daily_ten_num_Samples,type = "Conventionals"),
           daily_ten_excursion =  case_when( daily_ten_num_violation >= daily_ten_critical_excursions ~ 1,
                                             daily_ten_num_violation < daily_ten_critical_excursions ~ 0),
           daily_ten_excursion_high = case_when(daily_ten_num_violation_high >= daily_ten_critical_excursions ~ 1,
                                                daily_ten_num_violation_high < daily_ten_critical_excursions ~ 0),
           daily_ten_excursion_low = case_when(daily_ten_num_violation_low >= daily_ten_critical_excursions ~ 1,
                                               daily_ten_num_violation_low < daily_ten_critical_excursions ~ 0))
    
  

  grab_data <- df_grab_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    rename(Result_Date = SampleStartDate) %>%
    mutate(pH_violation = ifelse(Result_Numeric < pH_Min | Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_high = ifelse(Result_Numeric > pH_Max, 1, 0 ),
           pH_violation_low = ifelse(Result_Numeric < pH_Min, 1, 0 )) %>%
    group_by_at(group1) %>%
    summarise(grab_result = first(IRResultNWQSunit),
              grab_num_Samples = n(),
              grab_num_violation = sum(pH_violation),
              grab_num_violation_high = sum(pH_violation_high),
              grab_num_violation_low = sum(pH_violation_low),
              pH_low_crit = min(pH_Min),
              pH_high_crit = max(pH_Max),
              pH_code = first(pH_code)) 
  
  pH_data_together <- grab_data %>%
    bind_rows(cont_pH_ten_ten_data) %>%
    arrange(AU_ID, Result_Date)
  
    
  cont_pH_categories <- pH_data_together %>%
    group_by_at(group2) %>% 
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              total_continuous_days = n_distinct(Result_Date[!is.na(daily_ten_num_Samples)]),
              sum_daily_ten_excursion = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion == 1]),
              sum_daily_ten_excursion_high = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion_high > 0]),
              sum_daily_ten_excursion_low = n_distinct(Result_Date[!is.na(daily_ten_num_Samples) & daily_ten_excursion_low > 0]),
              total_grab_samples = sum(grab_num_Samples, na.rm = TRUE),
              total_grab_days =  n_distinct(Result_Date[!is.na(grab_num_Samples)]),
              sum_grab_excursions = sum(grab_num_violation, na.rm = TRUE),
              sum_grab_excursions_high = sum(grab_num_violation_high, na.rm = TRUE),
              sum_grab_excursions_low = sum(grab_num_violation_low, na.rm = TRUE),
              total_sample_days = total_continuous_days +total_grab_days,
              total_excursions = sum_daily_ten_excursion + sum_grab_excursions) %>%
    mutate(critical_excursions = binomial_excursions(total_continuous_days +total_grab_samples, type =  "Conventionals"), 
           IR_category = case_when(total_grab_samples == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion >= critical_excursions ~ "5",
                                   total_grab_samples == 0 & total_continuous_days < 8 & sum_daily_ten_excursion >= 1 ~ "3B",
                                   total_grab_samples == 0 & total_continuous_days < 8 & sum_daily_ten_excursion < 1 ~ "3",
                                   total_grab_samples == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion < critical_excursions ~ "2",
                                   total_continuous_days == 0 & total_grab_samples >=8 & sum_grab_excursions >= critical_excursions ~ "5",
                                   total_continuous_days == 0 & total_grab_samples < 8 & sum_grab_excursions >= 1 ~ "3B",
                                   total_continuous_days == 0 & total_grab_samples < 8 & sum_grab_excursions < 1 ~ "3",
                                   total_continuous_days == 0 & total_grab_samples >= 8 & sum_grab_excursions < critical_excursions ~ "2",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ "5",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= 1 ~ "3B",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < 1 ~ "3",
                                   total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~ "2",
                                   TRUE ~ "ERROR"
                                     ),
           Rationale = case_when(total_grab_samples == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion >= critical_excursions ~ paste0("Impaired: ",sum_daily_ten_excursion, 
                                                                                                                                             " daily time series measurements fall outside range of criteria (",
                                                                                                                                             sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                             sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                             total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_samples == 0 & total_continuous_days < 8 & sum_daily_ten_excursion >= 1 ~  paste0("Insufficient data: Less than 8 samples, ",sum_daily_ten_excursion, 
                                                                                                                                             " daily time series measurements fall outside range of criteria (",
                                                                                                                                             sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                             sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                             total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_samples == 0 & total_continuous_days < 8 & sum_daily_ten_excursion < 1 ~paste0("Insufficient data: Less than 8 samples, ",sum_daily_ten_excursion, 
                                                                                                                                          " daily time series measurements fall outside range of criteria (",
                                                                                                                                          sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                          sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                          total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_grab_samples == 0 & total_continuous_days >= 8 & sum_daily_ten_excursion < critical_excursions ~ paste0("Attaining: ",sum_daily_ten_excursion, 
                                                                                                                                            " daily time series measurements fall outside range of criteria (",
                                                                                                                                            sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                            sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                            total_continuous_days, " total days of continuous data. No grab samples used"),
                                 total_continuous_days == 0 & total_grab_samples >=8 & sum_grab_excursions >= critical_excursions ~ paste0("Impaired: ", sum_grab_excursions, 
                                                                                                                                        " samples fall outside criteria range (",
                                                                                                                                        sum_grab_excursions_high, " above criteria, ",
                                                                                                                                        sum_grab_excursions_low, " below criteria). ",
                                                                                                                                        total_grab_samples, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_samples < 8 & sum_grab_excursions >= 1 ~ paste0("Insufficient data: Less than 8 samples. ", sum_grab_excursions, 
                                                                                                                                        " samples fall outside criteria range (",
                                                                                                                                        sum_grab_excursions_high, " above criteria, ",
                                                                                                                                        sum_grab_excursions_low, " below criteria). ",
                                                                                                                         total_grab_samples, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_samples < 8 & sum_grab_excursions < 1 ~ paste0("Insufficient data: Less than 8 samples. ", sum_grab_excursions, 
                                                                                                                                       " samples fall outside criteria range (",
                                                                                                                                       sum_grab_excursions_high, " above criteria, ",
                                                                                                                                       sum_grab_excursions_low, " below criteria). ",
                                                                                                                        total_grab_samples, " total samples. No continuous data used."),
                                 total_continuous_days == 0 & total_grab_samples >= 8 & sum_grab_excursions < critical_excursions ~  paste0("Attaining: ", sum_grab_excursions, 
                                                                                                                                         " samples fall outside criteria range (",
                                                                                                                                         sum_grab_excursions_high, " above criteria, ",
                                                                                                                                         sum_grab_excursions_low, " below criteria). ",
                                                                                                                                         total_grab_samples, " total samples. No continuous data used."),
                                 total_grab_samples > 0 & total_continuous_days > 0 & (total_grab_samples + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= critical_excursions ~ paste0("Impaired: ",
                                                                                                                                                                                                                     sum_daily_ten_excursion, 
                                                                                                                                                                                                                     " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                     sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                     sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                     total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                     sum_grab_excursions, 
                                                                                                                                                                                                                     "grab samples fall outside criteria range (",
                                                                                                                                                                                                                     sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                     sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                     total_grab_samples, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) >= 1 ~ paste0("Insufficient data: Less than 8 days of continuous and grab samples combined. ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                  total_grab_samples, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) < 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < 1 ~  paste0("Insufficient data: Less than 8 days of continuous and grab samples combined. ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                  total_grab_samples, " total grab samples."),
                                 total_grab_days > 0 & total_continuous_days > 0 & (total_grab_days + total_continuous_days) >= 8 & (sum_daily_ten_excursion + sum_grab_excursions ) < critical_excursions ~ paste0("Attaining: ",
                                                                                                                                                                                                                    sum_daily_ten_excursion, 
                                                                                                                                                                                                                    " daily time series measurements fall outside range of criteria (",
                                                                                                                                                                                                                    sum_daily_ten_excursion_high, " above criteria, ",
                                                                                                                                                                                                                    sum_daily_ten_excursion_low, " below criteria) ",
                                                                                                                                                                                                                    total_continuous_days, " total days of continuous data. ",
                                                                                                                                                                                                                    sum_grab_excursions, 
                                                                                                                                                                                                                    " grab samples fall outside criteria range (",
                                                                                                                                                                                                                    sum_grab_excursions_high, " above criteria, ",
                                                                                                                                                                                                                    sum_grab_excursions_low, " below criteria). ",
                                                                                                                                                                                                                    total_grab_samples, " total grab samples."),
                                 TRUE ~ "ERROR")
                                     ) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) %>%
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) |> 
    mutate(period = NA_character_) |> 
    mutate(Delist_eligability = case_when(total_sample_days >= 18 & total_excursions <= binomial_delisting(total_sample_days, 'Conventionals')  ~ 1,
                                        TRUE ~ 0)) 
  

  ph_assessment_list <- list(data = pH_data_together,
                             assessment = cont_pH_categories)
  
        
  }
  
  
  
  WS_Assessment <- pH_assessment_fun(df_cont_data = cont_data, df_grab_data = pH_grab_no_cont,  AU_type = "WS")
  WS_data <-  WS_Assessment[["data"]]
  WS_categories <- WS_Assessment[["assessment"]]
  
  ## GNIS rollup -----------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup <- WS_categories %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              IR_category_GNIS_26 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
              Delist_eligability = max(Delist_eligability)) %>% 
    mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                          TRUE ~ 0)) |> 
    mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  
  
  WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")
  
  ### Delist process --------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')|> 
    mutate(Char_Name = 'pH') |> 
    relocate(Char_Name, .after = AU_GNIS_Name)
  
  
  ## AU Rollup -------------------------------------------------------------------------------------------------------
  
  
  WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup_delist, char_name_field = Char_Name)
  WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) 
  
  
  
  # Other AUs ------------------------------------------------------------------------------------------------------- 
  
  
  Other_Assessment <- pH_assessment_fun(df_cont_data = cont_data, df_grab_data = pH_grab_no_cont,  AU_type = "other")
  Other_data <-  Other_Assessment[["data"]]
  Other_categories <- Other_Assessment[["assessment"]]
  
  other_category <- join_prev_assessments(Other_categories, AU_type = 'Other')|> 
    select(-Char_Name) 
  
  
  other_category_delist <-  assess_delist(other_category, type = "Other")|> 
    mutate(Char_Name = 'pH') |> 
    relocate(Char_Name, .after = AU_ID)
  
  
  # prep data for export --------------------------------------------------------------------------------------------
  
  AU_display_other <- other_category_delist |> 
    select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)
  
  AU_display_ws <- WS_AU_rollup_joined |> 
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
                                          TRUE  ~ year_last_assessed)) |> 
    mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                   TRUE  ~ Year_listed)) 
  

  
  WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
    join_TMDL(type = 'GNIS') |> 
    join_AU_info()|> 
    relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
    relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
    relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  

# write xlsx ------------------------------------------------------------------------------------------------------

if(write_xlsx){
  
  
  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "pH WS Data", tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "pH other Data", tabColour = 'paleturquoise2')
  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_categories, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
 
  writeData(wb = wb, sheet = "pH WS Data", x = WS_data, headerStyle = header_st )
  writeData(wb = wb, sheet = "pH other Data", x = Other_data, headerStyle = header_st )
  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/pH-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}
  
  
  pH_list <- list(WS_data = WS_data,
                  Other_data = Other_data, 
                  AU_display = AU_display,
                  Other_AU_categorization = other_category_delist,
                  WS_station_categorization = WS_categories,
                  WS_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                  Other_AU_categorization = as.data.frame(other_category_delist)) 
  
  return(pH_list)
  
  }

