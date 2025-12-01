library(lubridate)
library(runner)
library(odeqIRtools)
library(openxlsx)





coast_contact <- function(df, type = "coast", write_excel = TRUE, database = 'IR_Dev'){
# Testing and development settings --------------------------------------------------------------------------------


 #df <- Bacteria_results 
 # type = "coast"
 #write_excel = TRUE
#database = 'IR_Dev'
  # Char rename -----------------------------------------------------------------------------------------------------
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  chars <- tbl(con, "LU_Pollutant") |> 
    select(Pollu_ID, `Pollutant_DEQ WQS`) |> 
    rename(Char_Name = `Pollutant_DEQ WQS`) |>
    #mutate(Pollu_ID = as.character(Pollu_ID)) |> 
    collect() |> 
    mutate(Pollu_ID = as.character(Pollu_ID))
  
  DBI::dbDisconnect(con)

# Filter data down. -----------------------------------------------------------------------------------------------

#if doing coast contact, keep relevant bacteria codes. If doing freshwater as entero, keep relevant codes/   
  if(type == "coast"){
    
    Coastal <- df %>%
      filter(Bacteria_code %in%  c(1, 3, 4),
             Char_Name == "Enterococcus")
  } else if (type == "freshwater") {
    Coastal <- df %>%
    filter(Bacteria_code %in%  c(4),
           Char_Name == "Enterococcus")
    
  } else {
    stop("Error- type must be 'coast' or 'freshwater'")
  }

  
  
if(length(unique(Coastal$AU_ID)) == 0) {
  stop("No Enterococcus Data")
} 

# NON Watershed unit categorization -----------------------------------------------------------------------------------
coast_contact_geomeans_no_WS <- Coastal %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  mutate(Geomean_Crit = 35,
         SS_Crit = 130 ) %>%
  group_by(AU_ID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen,
                                          Geomean_Crit = Geomean_Crit,
                                          SS_Crit = SS_Crit),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x)))%>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  date_range_90 = paste(min(SampleStartDate), "-", max(SampleStartDate)),
                                                  geomean_excur = ifelse(!all(is.na(geomean)) & geomean > max(Geomean_Crit), 1, 0),
                                                  #ss_excur_90 = ifelse(!is.na(Result_cen) & Result_cen > SS_Crit, 1, 0),
                                                  ss_count_excur_90 = n_distinct(SampleStartDate[Result_cen > SS_Crit]),
                                                  percent_excur_90 = ifelse(count_90 >= 5, n_distinct(SampleStartDate[Result_cen > SS_Crit])/ count_90, NA_real_) ,
                                                  ss_excur_dates = case_when(ss_count_excur_90 > 0 ~ stringr::str_c(unique(SampleStartDate [Result_cen > SS_Crit]),  collapse = "; ") ,
                                                                             ss_count_excur_90 == 0 ~ NA_character_) ,
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                             TRUE ~ NA_character_) 
                                 )
                               
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(AU_ID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ),
         single_sample_90_day_above_10perc_excursion = case_when(is.na(percent_excur_90) ~ "no",
                                                                 percent_excur_90 > 0.1 ~ "yes",
                                                                 TRUE ~ "no" ))


# Categorization --------------------------------------------------------------------------------------------------




coast_AU_summary_no_WS0 <-  coast_contact_geomeans_no_WS %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(AU_ID,  Char_Name, Pollu_ID, wqstd_code ) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            num_Samples = as.numeric(n()),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            max_ss_excursions_90_day = max(ss_count_excur_90, na.rm = TRUE),
            # max_ss_percent_excursion_90_day = ifelse(!all(is.na(percent_excur_90)),  
            #                                                     round(max(percent_excur_90, na.rm = TRUE), 2), 
            #                                                     NA_real_),
            # n_90day_percent_exceed = sum(percent_excur_90 > 0.10, na.rm = TRUE),
            geomeans_calculated = sum(!is.na(geomean)),
            geomean_excursions = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > max(Geomean_Crit), "yes", "no"),
            n_geomean_excursions = sum(geomean_excur),
            geomean_exceed_date_periods = case_when(geomean_excursions == "yes" ~ str_c(na.omit(unique(date_range_90[geomean > Geomean_Crit])), collapse = "; ")),
            mlocs_geomean_exceed = ifelse(geomean_excursions == "yes", str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
              ) %>%
    mutate(critical_excursions = binomial_excursions(num_Samples, type = "Conventionals"),
           IR_category = case_when(geomean_excursions == "yes" ~ "5",
                                   num_ss_excursions > critical_excursions & num_Samples >= 5 ~ "5",
                                   num_ss_excursions <= critical_excursions & num_Samples >= 5~ '2',
                                   geomeans_calculated == 0 & num_ss_excursions > 0 ~ "3B",
                                   geomeans_calculated == 0 & num_ss_excursions == 0 ~ "3",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(geomean_excursions == "yes" ~ paste0(n_geomean_excursions, 
                                                                      " geometric means exceed geomean criteria in time periods ",
                                                                      geomean_exceed_date_periods),
                                 num_ss_excursions > critical_excursions & num_Samples >= 5 ~ paste0(n_geomean_excursions, 
                                                                                                     " geometric means exceed geomean criteria. Single sample exceedance rate > 10% ccording to the exact binomial test"),
                                 num_ss_excursions <= critical_excursions & num_Samples >= 5 ~ paste0("Attaining: ", num_ss_excursions, " excursions is less than ",
                                                                                                      critical_excursions, " needed to list- ",
                                                                                                      num_Samples, " total samples"),
                                 geomeans_calculated == 0 & num_ss_excursions > 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                           num_ss_excursions, " single sample excursions"),
                                 geomeans_calculated == 0 & num_ss_excursions == 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                            num_ss_excursions, " single sample excursions"),
                                 geomeans_calculated >= 1 & geomean_excursions == "no" & num_ss_excursions <= critical_excursions   ~ "No excursions of geometric mean. No 90 day period > 10% single sample exceedance rate",
                                 TRUE ~ "ERROR"),
    ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ),
         period = NA_character_) |> 
  mutate(Delist_eligability = case_when(num_Samples >= 18 & num_ss_excursions <= binomial_delisting(num_Samples, 'Conventionals') & IR_category == '2' ~ 1,
                                        TRUE ~ 0)) |> 
  mutate(period = NA_character_) |> 
    ungroup() |> 
    select(-Char_Name)

coast_AU_summary_no_WS <-  join_prev_assessments(coast_AU_summary_no_WS0, AU_type = 'Other')

if(type == 'freshwater'){
  
  coast_AU_summary_no_WS <- coast_AU_summary_no_WS |> 
    filter(AU_ID %in% coast_contact_geomeans_no_WS$AU_ID)
  
}

coast_AU_summary_no_WS_delist <- assess_delist(coast_AU_summary_no_WS, type = 'Other')|> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)





# Watershed unit categorization -----------------------------------------------------------------------------------



coast_contact_geomeans_WS <- Coastal %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) 

if(nrow(coast_contact_geomeans_WS) > 0){
  
  coast_contact_geomeans_WS <- coast_contact_geomeans_WS |> 
  mutate(Geomean_Crit = 35,
         SS_Crit = 130 ) %>%
  group_by(MLocID, AU_ID, AU_GNIS_Name, Char_Name) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen,
                                          Geomean_Crit = Geomean_Crit,
                                          SS_Crit = SS_Crit),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x)))%>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  date_range_90 = paste(min(SampleStartDate), "-", max(SampleStartDate)),
                                                  geomean_excur = ifelse(!all(is.na(geomean)) & geomean > max(Geomean_Crit), 1, 0),
                                                  #ss_excur_90 = ifelse(!is.na(Result_cen) & Result_cen > SS_Crit, 1, 0),
                                                  ss_count_excur_90 = n_distinct(SampleStartDate[Result_cen > SS_Crit]),
                                                  percent_excur_90 = ifelse(count_90 >= 5, n_distinct(SampleStartDate[Result_cen > SS_Crit])/ count_90, NA_real_) ,
                                                  ss_excur_dates = case_when(ss_count_excur_90 > 0 ~ stringr::str_c(unique(SampleStartDate [Result_cen > SS_Crit]),  collapse = "; ") ,
                                                                             ss_count_excur_90 == 0 ~ NA_character_) ,
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                             TRUE ~ NA_character_) 
                                 )
                               
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(MLocID, SampleStartDate)  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ),
         single_sample_90_day_above_10perc_excursion = case_when(is.na(percent_excur_90) ~ "no",
                                                                 percent_excur_90 > 0.1 ~ "yes",
                                                                 TRUE ~ "no" ))


## Categorization --------------------------------------------------------------------------------------------------




coast_AU_summary_WS0 <-  coast_contact_geomeans_WS %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(MLocID, AU_ID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code, OWRD_Basin ) %>%
  summarise( stations =  stringr::str_c(unique(MLocID), collapse = "; "),
             num_Samples = as.numeric(n()),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            max_ss_excursions_90_day = max(ss_count_excur_90, na.rm = TRUE),
            max_ss_percent_excursion_90_day = ifelse(is.finite(max(percent_excur_90, na.rm = TRUE)),  
                                                     round(max(percent_excur_90, na.rm = TRUE), 2), 
                                                     NA_real_),
            n_90day_percent_exceed = sum(percent_excur_90 > 0.10, na.rm = TRUE),
            geomeans_calculated = sum(!is.na(geomean)),
            geomean_excursions = ifelse(!is.na(Max_Geomean) &
                                          Max_Geomean > max(Geomean_Crit), "yes", "no"),
            n_geomean_excursions = sum(geomean_excur),
            geomean_exceed_date_periods = case_when(geomean_excursions == "yes" ~ str_c(na.omit(unique(date_range_90[geomean > Geomean_Crit])), collapse = "; ")),
            mlocs_geomean_exceed = ifelse(geomean_excursions == "yes", str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
  ) %>%
  mutate(critical_excursions = binomial_excursions(num_Samples, type = "Conventionals"),
         IR_category = case_when(geomean_excursions == "yes" ~ "5",
                                 num_ss_excursions > critical_excursions & num_Samples >= 5 ~ "5",
                                 num_ss_excursions <= critical_excursions & num_Samples >= 5~ '2',
                                 geomeans_calculated == 0 & num_ss_excursions > 0 ~ "3B",
                                 geomeans_calculated == 0 & num_ss_excursions == 0 ~ "3",
                                                                  TRUE ~ "ERROR"),
         Rationale = case_when(geomean_excursions == "yes" ~ paste0(n_geomean_excursions, 
                                                                    " geometric means exceed geomean criteria in time periods ",
                                                                    geomean_exceed_date_periods),
                               num_ss_excursions > critical_excursions & num_Samples >= 5 ~ paste0(n_geomean_excursions, 
                                                                                                   " geometric means exceed geomean criteria. Single sample exceedance rate > 10% ccording to the exact binomial test"),
                               num_ss_excursions <= critical_excursions & num_Samples >= 5 ~ paste0("Attaining: ", num_ss_excursions, " excursions is less than ",
                                                                                                    critical_excursions, " needed to list- ",
                                                                                                    num_Samples, " total samples"),
                               geomeans_calculated == 0 & num_ss_excursions > 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                         num_ss_excursions, " single sample excursions"),
                               geomeans_calculated == 0 & num_ss_excursions == 0 ~ paste0("No 90 day period has at least 5 results. ", 
                                                                                          num_ss_excursions, " single sample excursions"),
                               geomeans_calculated >= 1 & geomean_excursions == "no" & num_ss_excursions <= critical_excursions   ~ "No excursions of geometric mean. No 90 day period > 10% single sample exceedance rate",
                               TRUE ~ "ERROR"),
  ) %>%
  mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code ),
         period = NA_character_) |> 
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(Delist_eligability = case_when(num_Samples >= 18 & num_ss_excursions <= binomial_delisting(num_Samples, 'Conventionals') & IR_category == '2' ~ 1,
                                        TRUE ~ 0)) |> 
  mutate(period = NA_character_)


WS_GNIS_rollup <- coast_AU_summary_WS0 %>%
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  summarise( stations =  stringr::str_c(unique(stations), collapse = "; "),
             IR_category_GNIS_26 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
  ungroup() |> 
  select(-Char_Name) 

coast_AU_summary_WS <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")

if(type == 'freshwater'){
  
  coast_AU_summary_WS <- coast_AU_summary_WS |> 
    filter(AU_GNIS_Name %in% WS_GNIS_rollup$AU_GNIS_Name)
  
}

WS_GNIS_rollup_delist <- assess_delist(coast_AU_summary_WS, type = 'WS')|> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_GNIS_Name)

WS_AU_rollup <- rollup_WS_AU(coast_AU_summary_WS)

WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) |> 
  ungroup() |> 
  select(-any_of("Char_Name")) |> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)


#end if statement
} 


# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- coast_AU_summary_no_WS_delist |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup_joined |>
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)

# 
# AU_display <- bind_rows(AU_display_other, AU_display_ws) |>
#   mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
#                                TRUE ~ Rationale))
AU_display <-  bind_rows(AU_display_other,  AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               TRUE ~ Rationale)) |> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                 TRUE ~  Year_listed)) 


if(write_excel){
  print("Writing excel doc")
  
  wb <- createWorkbook()
  
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "Coast Contact Raw Data", tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "Coast Contact WS Data", tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "Coast Contact other Data", tabColour = 'paleturquoise2')
  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = coast_AU_summary_no_WS_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = coast_AU_summary_WS0, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Coast Contact Raw Data", x = df, headerStyle = header_st)
  writeData(wb = wb, sheet = "Coast Contact WS Data", x = coast_contact_geomeans_no_WS, headerStyle = header_st )
  writeData(wb = wb, sheet = "Coast Contact other Data", x = coast_contact_geomeans_no_WS, headerStyle = header_st )
  

  
  

  saveWorkbook(wb, paste0("Parameters/Outputs/bacteria coast contact- ", Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}




bacteria_coast <-list(coast_bacteria_data_other=as.data.frame(coast_contact_geomeans_no_WS),
                      other_au_categorization=as.data.frame(coast_AU_summary_no_WS_delist))


if(nrow(coast_contact_geomeans_WS) > 0){
bacteria_coast_ws <- list(coast_bacteria_data_ws=as.data.frame(coast_contact_geomeans_WS),
                          ws_station_categorization=as.data.frame(coast_AU_summary_WS0),
                          ws_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                          ws_au_categorization=as.data.frame(WS_AU_rollup_joined))

bacteria_coast <- c(bacteria_coast, bacteria_coast_ws)             

}


return(bacteria_coast)

}
