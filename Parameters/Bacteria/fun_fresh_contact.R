

fresh_contact_rec <- function(df, write_excel = TRUE, database = "IR_Dev"){


  

# Testing and development settings --------------------------------------------------------------------------------

  
  #df <- Bacteria_results  
  #write_excel = TRUE  
library(runner)
library(openxlsx)
  
  
  
  # Char rename -----------------------------------------------------------------------------------------------------
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  chars <- tbl(con, "LU_Pollutant") |> 
    select(Pollu_ID, `Pollutant_DEQ WQS`) |> 
    rename(Char_Name = `Pollutant_DEQ WQS`) |>
    mutate(Pollu_ID = as.character(Pollu_ID)) |> 
    collect()
  
  DBI::dbDisconnect(con)

# Initial Filtering -----------------------------------------------------------------------------------------------

  
fresh_contact <- df %>%
  filter(Bacteria_code %in% c(2, 4)) |> 
  filter(case_when(Bacteria_code == 2 ~ Char_Name == "Escherichia coli",
                   Bacteria_code == 4 ~ Char_Name %in% c("Enterococcus", "Escherichia coli"))) |> 
  group_by(MLocID) |> 
  mutate(has_ecoli = case_when(any(Char_Name == 'Escherichia coli') ~ 1,
                               TRUE ~ 0)) |> 
  ungroup() |> 
  group_by(AU_ID) |> 
  mutate(entero = case_when(any(Bacteria_code == 4) & max(has_ecoli) == 0 ~ 1,
                            TRUE ~ 0)) |> 
  ungroup() |> 
  filter(case_when(Bacteria_code == 2 ~ Char_Name == "Escherichia coli",
                   Bacteria_code == 4 & entero == 1 ~ Char_Name == "Enterococcus",
                   Bacteria_code == 4 & entero == 0 ~ Char_Name == 'Escherichia coli'))

if(length(unique(fresh_contact$AU_ID)) == 0) {
  stop("No E coli Data")
} 

# Geometric mean calculations --------------------------------------------

#Analyze by monitoring location 
#Add column (d) which is a rolling 90 day period dataframe of date and results
#Calculate geomena if >= 5 samples in 90 day period
#Number of samples in 90 day period (count_90)
#String of unique dates in 90 day period
  #This is used to create ratioanale
#Comment if geomean is able to be calculated

#Pull it back into main datafrmae with unnest_wider

# Create 2 columns for reviewer assistance
  #geomean_excursion - If the geomean associated with that day is an excursion of geomean crit
  #single_sample_excursion- If that day's result is an excursion of the single sample crit

#E coli assessment

fresh_contact_geomeans <- fresh_contact %>%
  filter(Char_Name == "Escherichia coli") |> 
  group_by(MLocID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(dplyr::n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                            TRUE ~ NA_character_) )
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(AU_ID, MLocID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ))
  



# Categorization --------------------------------------------------------------------------------------------------


## Watershed unit categorization -----------------------------------------------------------------------------------


fresh_AU_summary_WS0 <-  fresh_contact_geomeans %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  arrange(MLocID) %>%
  ungroup() %>%
  group_by(AU_ID, MLocID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code ) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            #stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_Samples = as.numeric(n()),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            critical_excursions = binomial_excursions(num_Samples, "Conventionals"),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit),
            geomean_over = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > Geomean_Crit, 1, 0),
            geomean_exceed_date_periods = ifelse(geomean_over == 1 ,str_c(na.omit(unique(dates_90[geomean > Geomean_Crit]), collapse = " AND ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")
  ) %>%
  mutate(IR_category = case_when(geomean_over == 1 ~ "5",
                                 num_Samples >= 5 & num_ss_excursions > critical_excursions ~  "5",
                                 is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ "3",
                                 is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ "3B",
                                 !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ "2",
                                 is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions <= critical_excursions ~ "2",
                                 TRUE ~ "ERROR"), 
         Rationale = case_when(geomean_over == 1 ~ paste(MLocID, ": Geomeans exceed criteria value of", Geomean_Crit, "for time periods",geomean_exceed_date_periods, "-",num_Samples, "total samples"  ),
                        num_Samples >= 5 & num_ss_excursions > critical_excursions ~  paste(MLocID, ": Single samples exceed criteria value of",
                                                                                            SS_Crit, num_ss_excursions, 'times on', 
                                                                                            ss_exceed_date_periods, "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ paste(MLocID, ": Insufficient Data -",
                                                                                           num_Samples, 
                                                                                           "samples with no exceedances", "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ paste(MLocID, ": Insufficient Data - ",
                                                                                           num_Samples, 
                                                                                           "samples with", 
                                                                                           num_ss_excursions, 
                                                                                           "excursions", "-",num_Samples, "total samples"  ),
                        !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ paste(MLocID, ": Attaining- No geomean above criteria", "-",num_Samples, "total samples" ),
                        is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions <= critical_excursions ~ paste(MLocID, ": Attaining- No geomean calculated. Single sample excursions below binomial", "-",num_Samples, "total samples"  ),
                        TRUE ~ "ERROR"
  )
  ) %>%
  #Set category as ordered factor to allow for easier rollup
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
  #mutate(delist_max_excursions = binomial_delisting(num_Samples, 'Conventionals')) |> 
  mutate(Delist_eligability = case_when(num_Samples >= 18 & num_ss_excursions <= binomial_delisting(num_Samples, 'Conventionals') & (Max_Geomean < Geomean_Crit | is.na(Max_Geomean) & IR_category == '2') ~ 1,
                                        TRUE ~ 0)) |> 
  select(-geomean_over, -geomean_exceed_date_periods, -ss_exceed_date_periods) |> 
  mutate(period = NA_character_)


### WS GNIS Rollup --------------------------------------------------------------------------------------------------


WS_GNIS_rollup <- fresh_AU_summary_WS0 %>%
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            IR_category_GNIS_24 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))   |> 
  ungroup() |> 
  select(-Char_Name)


WS_GNIS_rollup_1 <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS") |> 
  filter(AU_ID %in% Bacteria_results$AU_ID)

# WS_GNIS_rollup_delist <- WS_GNIS_rollup_1 |> 
#   mutate(Delist_eligability = case_when( prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_24 == '2' & Delist_eligability ==1 ~ "Delist Eligible",
#                                          prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_24 == '2' & Delist_eligability < 1 ~ 'Insuffcient data to delist')) |> 
#   mutate(final_GNIS_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
#                                     TRUE ~ final_GNIS_cat),
#          Rationale_GNIS = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale_GNIS),
#                                     TRUE ~ Rationale_GNIS)) |> 
#   mutate(final_GNIS_cat = factor(final_GNIS_cat,levels=c("Unassessed",'3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
#   mutate(status_change = case_when(final_GNIS_cat == prev_GNIS_category & IR_category_GNIS_24 == 'Unassessed' ~ "No change in status- No new assessment",
#                                    final_GNIS_cat == prev_GNIS_category ~ "No change in status- Assessed",
#                                    final_GNIS_cat == '2' & prev_GNIS_category %in% c('5','4A','4B', '4C') ~ "Delist",
#                                    prev_GNIS_category == 'Unassessed' ~ "New Assessment",
#                                    prev_GNIS_category == '2' & final_GNIS_cat  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
#                                    prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
#                                    prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('2') ~ "Insufficient to Attain"
#   ))
WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup_1, type = 'WS')|> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_GNIS_Name)


### WS AU rollup ----------------------------------------------------------------------------------------------------

# WS_AU_rollup <- WS_GNIS_rollup_delist %>%
#   ungroup() %>%
#   mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
#                                     TRUE ~ Rationale_GNIS)) |> 
#   group_by(AU_ID, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
#   summarise(IR_category_AU_24 = max(final_GNIS_cat),
#             Rationale_AU = str_c(Rationale_GNIS,collapse =  " ~ " ) ) %>%
#   mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
#   mutate(status_change = case_when(IR_category_AU_24 == prev_AU_category & is.na(Rationale_AU) ~ "No change in status- No New Assessment",
#                                    IR_category_AU_24 == prev_AU_category & !is.na(Rationale_AU)~ "No change in status- New Assessment",
#                                    IR_category_AU_24 == '2' & prev_AU_category %in% c('5','4A','4B', '4C') ~ "Delist",
#                                    prev_AU_category == 'Unassessed' | is.na(prev_AU_category)  ~ "New Assessment",
#                                    prev_AU_category == '2' & IR_category_AU_24  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
#                                    prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
#                                    prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('2') ~ "Insufficient to Attain"
#   ))
# 
# 
# 
# WS_AU_rollup_joined <- WS_AU_rollup |> 
#   left_join(select(prev_list_AU, -prev_category, -prev_rationale, -Pollutant)) |> 
#   mutate(Year_listed = case_when(status_change %in% c("Attain to Impaired", "Insufficient to Impaired","New Assessment") &
#                                    IR_category_AU_24  %in% c('5','4A','4B', '4C') &
#                                    is.na(Year_listed) ~ '2024',
#                                  TRUE ~ Year_listed),
#          year_last_assessed = case_when(status_change != 'No change in status- No New Assessment' ~"2024",
#                                         TRUE ~ year_last_assessed)
#   )

WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup_delist)

WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) |> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)|> 
  filter(AU_ID %in% Bacteria_results$AU_ID)

## Non- watershed unit categorization ---------------------------------------------------------------------------------------------------

fresh_contact_geomeans_other <- fresh_contact %>%
  filter(Char_Name == "Escherichia coli") |> 
  group_by(AU_ID) %>%
  arrange(SampleStartDate) %>%
  dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                          Result_cen = Result_cen),
                           k = "90 days",
                           lag = 0,
                           idx = SampleStartDate,
                           f = function(x) list(x))) %>%
  dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                 dplyr::summarise(geomean = dplyr::case_when(dplyr::n_distinct(SampleStartDate) >= 5 ~  geo_mean(Result_cen),
                                                                             TRUE ~ NA_real_),
                                                  count_90 = dplyr::n_distinct(SampleStartDate),
                                                  dates_90 = stringr::str_c(unique(SampleStartDate),  collapse = "; "),
                                                  comment = dplyr::case_when(length(SampleStartDate) < 5 ~  'Not enough values to calculate geometric mean',
                                                                             TRUE ~ NA_character_) )
  )) %>%
  tidyr::unnest_wider(d) %>%
  arrange(AU_ID, MLocID, SampleStartDate )  %>%
  mutate(geomean_excursion = case_when(is.na(geomean) ~ "no: < 5 samples in 90 day period",
                                       geomean >  Geomean_Crit ~ "yes",
                                       TRUE ~ "no"),
         single_sample_excursion = ifelse(Result_cen > SS_Crit, "yes", "no" ))



fresh_AU_summary_no_WS0 <-  fresh_contact_geomeans_other %>%
  filter(!str_detect(AU_ID, "WS")) %>%
  ungroup() %>%
  group_by(AU_ID, Pollu_ID, wqstd_code ) %>%
  summarise(OWRD_Basin = first(OWRD_Basin), 
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            max.value  = max(Result_cen),
            num_Samples = as.numeric(n()),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            critical_excursions = binomial_excursions(num_Samples, "Conventionals"),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit),
            mlocs = str_c(unique(MLocID), collapse = "; "),
            mlocs_ss_exceed = str_c(unique(MLocID[Result_cen > SS_Crit]), collapse = "; "),
            geomean_over = ifelse(!is.na(Max_Geomean) &
                                    Max_Geomean > Geomean_Crit, 1, 0),
            #mlocs_geomean_exceed = ifelse(geomean_over == 1, str_c(unique(na.omit(MLocID[geomean > Geomean_Crit]), collapse = "; ")),NA),
            geomean_exceed_date_periods = ifelse(geomean_over == 1 ,str_c(na.omit(unique(dates_90[geomean > Geomean_Crit]), collapse = "; ")),NA),
            ss_exceed_date_periods = str_c(unique(SampleStartDate[Result_cen > SS_Crit]), collapse =  "; ")) %>%
  mutate(IR_category = case_when(geomean_over == 1 ~ "5",
                                 num_Samples >= 5 & num_ss_excursions >= critical_excursions ~  "5",
                                 is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ "3",
                                 is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ "3B",
                                 !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ "2",
                                 is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions < critical_excursions ~ "2",
                                 TRUE ~ "ERROR"),
         Rationale = case_when(geomean_over == 1 ~ paste0("Impaired: Geomean exceed criteria value of ", Geomean_Crit, " for time periods ",geomean_exceed_date_periods, "- ",num_Samples, " total samples"  ),
                               num_Samples >= 5 & num_ss_excursions >= critical_excursions ~  paste0("Impaired: Single samples exceed criteria value of ",
                                                                                                   SS_Crit, " ", num_ss_excursions, ' times on ', 
                                                                                                   ss_exceed_date_periods, " at ", mlocs_ss_exceed,   "- ",num_Samples, " total samples" ),
                               is.na(Max_Geomean) & max.value < SS_Crit & num_Samples < 5 ~ paste0("Insufficient Data: ",
                                                                                                  num_Samples, 
                                                                                                   " samples with no exceedances", "- ",num_Samples, " total samples at ", mlocs ),
                               is.na(Max_Geomean) & max.value > SS_Crit & num_Samples < 5 ~ paste0("Insufficient Data: ",
                                                                                                  num_Samples, 
                                                                                                  " samples with ", 
                                                                                                  num_ss_excursions, 
                                                                                                  " excursions", "- ",num_Samples, " total samples at ", mlocs  ),
                               !is.na(Max_Geomean) & Max_Geomean <= Geomean_Crit ~ paste0("Attaining: No geomean above criteria", "- ",num_Samples, " total samples at ", mlocs ),
                               is.na(Max_Geomean) & num_Samples >= 5 & num_ss_excursions < critical_excursions ~ paste0("Attaining: No geomean calculated. Single sample excursions below binomial", "- ",num_Samples, " total samples at", mlocs  ),
                               TRUE ~ "ERROR"
         )) %>%
  mutate(Delist_eligability = case_when(num_Samples >= 18 & num_ss_excursions <= binomial_delisting(num_Samples, 'Conventionals') & (Max_Geomean < Geomean_Crit | is.na(Max_Geomean) & IR_category == '2') ~ 1,
                                        TRUE ~ 0)) |> 
  select(-geomean_over, -geomean_exceed_date_periods, -ss_exceed_date_periods) %>%
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code )) |> 
  mutate(period = NA_character_)

fresh_AU_summary_no_WS <- join_prev_assessments(fresh_AU_summary_no_WS0, AU_type = "Other")|> 
  filter(AU_ID %in% Bacteria_results$AU_ID)



fresh_AU_summary_no_WS_delist <- assess_delist(fresh_AU_summary_no_WS, type = "Other")|> 
  left_join(chars, by = join_by(Pollu_ID)) |> 
  relocate(Char_Name, .after = AU_ID)



# Entero assessments ----------------------------------------------------------------------------------------------
entero_data <- fresh_contact %>%
  filter(Char_Name == "Enterococcus")


if(nrow(entero_data) > 0){

entero_assessments <- coast_contact(entero_data,  type = "freshwater", write_excel = FALSE)

entero_other_data <- entero_assessments[['coast_bacteria_data_other']]
entero_other_AU_cat <- entero_assessments[['other_au_categorization']]|> 
  mutate(Rationale = paste0('Freshwater assessed with Enterococcus. ', Rationale))

entero_ws_data <- entero_assessments[['coast_bacteria_data_ws']]

entero_ws_MLOC_cat <- entero_assessments[['ws_station_categorization']] |> 
  mutate(Rationale = paste0('Freshwater assessed with Enterococcus. ', Rationale))

entero_ws_GNIS_cat <- entero_assessments[['ws_GNIS_categorization']] |> 
  mutate(Rationale_GNIS = paste0('Freshwater assessed with Enterococcus. ', Rationale_GNIS))

entero_ws_AU_cat <- entero_assessments[['ws_au_categorization']] |> 
  mutate(Rationale_AU = paste0('Freshwater assessed with Enterococcus. ', Rationale_AU))




## join entero assessments -----------------------------------------------------------------------------------------

fresh_WS_GNIS_cat <-  bind_rows(WS_GNIS_rollup_delist, entero_ws_GNIS_cat)
fresh_WS_AU_cat <-  bind_rows(WS_AU_rollup_joined, entero_ws_AU_cat)

} else {
  fresh_WS_GNIS_cat <- WS_GNIS_rollup_delist
  fresh_WS_AU_cat <- WS_AU_rollup_joined
  
}


# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- fresh_AU_summary_no_WS_delist |> 
  select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_other_entero <- entero_other_AU_cat |> 
  select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale,stations, recordID, status_change, Year_listed,  year_last_assessed)


AU_display_ws <- WS_AU_rollup_joined |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_24,
         Rationale = Rationale_AU)

AU_display_ws_entero <- entero_ws_AU_cat |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_24,
         Rationale = Rationale_AU)



AU_display <- bind_rows(AU_display_other, 
                        #AU_display_other_entero, 
                        AU_display_ws, 
                        #AU_display_ws_entero
                        ) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment'  ~ "2024",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2024',
                                 TRUE ~  Year_listed)) 


WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  


entero_ws_GNIS_cat <- entero_ws_GNIS_cat|> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  


# Create excel doc ------------------------------------------------------------------------------------------------
if(write_excel){

  
  
  
  
wb <- createWorkbook()
addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')


addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
addWorksheet(wb, sheetName = "Other_AU_categorization_entero",tabColour = 'dodgerblue3')

addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
addWorksheet(wb, sheetName = "WS station cat_entero", tabColour = 'lightblue3')

addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
addWorksheet(wb, sheetName = "WS GNIS categorization_entero", tabColour = 'lightyellow1')

addWorksheet(wb, sheetName = "Fresh Bacteria Data_WS", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Fresh Bacteria Data_other", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Fresh Entero Bact Data_other", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Fresh Entero Bact Data_WS", tabColour = 'paleturquoise2')


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)

writeData(wb = wb, sheet = "Other_AU_categorization", x = fresh_AU_summary_no_WS_delist, headerStyle = header_st)
#writeData(wb = wb, sheet = "Other_AU_categorization_entero", x = entero_other_AU_cat, headerStyle = header_st)

writeData(wb = wb, sheet = "WS station categorization", x = fresh_AU_summary_WS0, headerStyle = header_st)
#writeData(wb = wb, sheet = "WS station cat_entero", x = entero_ws_MLOC_cat, headerStyle = header_st)

writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
#writeData(wb = wb, sheet = "WS GNIS categorization_entero", x = entero_ws_GNIS_cat, headerStyle = header_st)

writeData(wb = wb, sheet = "Fresh Bacteria Data_WS", x = fresh_contact_geomeans, headerStyle = header_st)
writeData(wb = wb, sheet = "Fresh Bacteria Data_other", x = fresh_contact_geomeans_other, headerStyle = header_st)
#writeData(wb = wb, sheet = "Fresh Entero Bact Data_other", x = entero_other_data, headerStyle = header_st)
#writeData(wb = wb, sheet = "Fresh Entero Bact Data_WS", x = entero_ws_data, headerStyle = header_st)


print("Writing excel doc")
saveWorkbook(wb, paste0("Parameters/Outputs/bacteria_freshwater_contact-",Sys.Date(), ".xlsx"), overwrite = TRUE) 

}



# Function output list --------------------------------------------------------------------------------------------


bacteria_freshwater <-list(AU_Decisions=as.data.frame(AU_display),
                           Other_AU_categorization=as.data.frame(fresh_AU_summary_no_WS_delist),
                           WS_station_categorization=as.data.frame(fresh_AU_summary_WS0),
                           WS_station_cat_entero=as.data.frame(entero_ws_MLOC_cat),
                           WS_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                           WS_GNIS_categorization_entero = as.data.frame(entero_ws_GNIS_cat),
                           Fresh_Bacteria_Data =as.data.frame( fresh_contact_geomeans),
                           Fresh_Entero_Bact_Data_other = as.data.frame(entero_other_data),
                           Fresh_Entero_Bact_Data_WS =as.data.frame( entero_ws_data)
                           )

return(bacteria_freshwater)
}
