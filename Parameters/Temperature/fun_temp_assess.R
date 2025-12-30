

library(lubridate)
library(runner)
library(odeqIRtools)



fun_temp_analysis <- function(df, write_excel = TRUE){
  
  # Testing ---------------------------------------------------------------------------------------------------------
  
  #df <- Results_censored_temp
  # write_excel = TRUE
  
  
  
  
  # Preliminary data prep ------------------------------------------------------------------------------------------------
  
  #calculate in/out critical period
  #calculate in/out spawn period
  #calculate prelim (non-air temp exlcusion) criteria violations
  print("Begin initial temp analysis")
  
  temp_analysis <- df %>%
    filter(!FishCode %in% c('10','11','22','23')) %>%
    mutate(SampleStartDate = ymd(SampleStartDate),
           # Add columns for Critcal period start and end date
           Crit_period_start = mdy(paste0("7/1/",year(SampleStartDate))),
           Cirt_period_end = mdy(paste0("9/30/",year(SampleStartDate))),
           # Append spawn start and end dates with year
           Start_spawn = ifelse(!is.na(SpawnStart), paste0(SpawnStart,"/",year(SampleStartDate)), NA ) ,
           End_spawn = ifelse(!is.na(SpawnEnd), paste0(SpawnEnd,"/",year(SampleStartDate)), NA ),
           # Make spwnmn start and end date date format
           Start_spawn = mdy(Start_spawn),
           End_spawn = mdy(End_spawn),
           # If Spawn dates span a calendar year, account for year change in spawn end date
           End_spawn = if_else(End_spawn < Start_spawn & SampleStartDate >= End_spawn, 
                               End_spawn + lubridate::years(1), # add a year if in spawn period carrying to next year
                               End_spawn),
           Start_spawn = if_else(End_spawn < Start_spawn & SampleStartDate <= End_spawn, Start_spawn - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                                 Start_spawn),
           SampleStartDate = ymd(SampleStartDate), 
           # Flag for results in critical period
           In_crit_period = ifelse(SampleStartDate >=Crit_period_start & SampleStartDate <= Cirt_period_end, 1, 0 ),
           # Print if result is in spawn or out of spawn
           Spawn_type = case_when(SampleStartDate >= Start_spawn + lubridate::days(6) & SampleStartDate <= End_spawn & !is.na(Start_spawn) ~ "Spawn",
                                  SampleStartDate >= Start_spawn  & SampleStartDate <= End_spawn & !is.na(Start_spawn) ~ "Early spawn- Not assessed for spawning",
                                  TRUE ~ 'Not_Spawn'
                                  ),
           # Flag if result violates standard,  use 13 for during spawn dates, else use criteria
           year_round_Violation = ifelse(Result_cen > Temp_Criteria, 1, 0),
           # Flag for is violation was in spawn period
           Spawn_Violation = ifelse(Spawn_type == "Spawn" & Result_cen > 13, 1, 0 )
    ) %>%
    arrange(SampleStartDate, SampleStartTime) %>%
    filter(!is.na(AU_ID))
  
  
  ## Air temp exclusion ----------------------------------------------------------------------------------------------
  
  
  print("Begin air temp exclusion analysis")
  temp_air_exclusion0 <- air_temp_exclusion(temp_analysis, date_col =  'SampleStartDate') 
  
  temp_air_exclusion <- temp_air_exclusion0 %>%
    mutate(year_round_excursion = case_when(above_exclusion_7d == 'Yes' & year_round_Violation == 1 ~ 0,
                                            TRUE ~ year_round_Violation),
           spawn_excursion = case_when(above_exclusion_7d == 'Yes' & Spawn_Violation == 1  ~ 0,
                                       TRUE ~ Spawn_Violation))
  
  
 
  

# Year Round Assessment -------------------------------------------------------------------------------------------

  
## Watershed Unit --------------------------------------------------------------------------------------------------
  
  
  # watershed unit 3 year excursion rollup
  # Grouped by mloc
  ws_3_year <- temp_air_exclusion %>%
    filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code) %>%
    dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                            year_round_excursion = year_round_excursion,
                                            spawn_excursion = spawn_excursion),
                             k = "3 years",
                             lag = 0,
                             idx = SampleStartDate,
                             f = function(x) list(x))) %>%
    dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                   dplyr::summarise(excursions_3yr = sum(year_round_excursion),
                                                    excursions_spawn_3yr = sum(spawn_excursion), 
                                                    samples_crit_period = sum(In_crit_period),
                                                    samples_spawn = sum(Spawn_type == "Spawn")) 
    )) %>%
    tidyr::unnest_wider(d)
  
  # other unit 3 year excursion rollup
  # Grouped by au_id
  
  other_3_year <- temp_air_exclusion %>%
    filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
    group_by(AU_ID,  Pollu_ID, wqstd_code) %>%
    dplyr::mutate(d = runner(x = data.frame(SampleStartDate  = SampleStartDate,
                                            year_round_excursion = year_round_excursion,
                                            spawn_excursion = spawn_excursion),
                             k = "3 years",
                             lag = 0,
                             idx = SampleStartDate,
                             f = function(x) list(x))) %>%
    dplyr::mutate(d = purrr::map(d, ~ .x %>%
                                   dplyr::summarise(excursions_3yr = sum(year_round_excursion),
                                                    excursions_spawn_3yr = sum(spawn_excursion), 
                                                    samples_crit_period = sum(In_crit_period),
                                                    samples_spawn = sum(Spawn_type == "Spawn")) 
    )) %>%
    tidyr::unnest_wider(d)
  
  
  
  ### Year Round Watershed  categorization --------------------------------------------------------------------------
  
  
  
  print('begin Year Round wastershed unit categorization')
  
  crit_period_check <- ws_3_year %>%
    mutate(year =  year(SampleStartDate)) %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code, year) %>%
    summarise(num_crit = sum(In_crit_period)) %>%
    ungroup() %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code) %>%
    summarise(distinct_years_sufficient_crit_period = n_distinct(year[num_crit > .8 * 92 ]))
  
  
  
  temp_IR_categories_WS <- ws_3_year %>%
    left_join(crit_period_check) %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code) %>%
    summarise( stations =  stringr::str_c(unique(MLocID), collapse = "; "),
               Temp_Criteria = first(Temp_Criteria),
               data_period_start = min(SampleStartDate),
               data_period_end = max(SampleStartDate),
               total_valid_excursions = sum(year_round_excursion),
               total_air_exclusions = sum(year_round_Violation) -sum(year_round_excursion) ,
               max_3yr_excursions = max(excursions_3yr),
               max_3yr_results_in_crit_period = max(samples_crit_period),
               distinct_years = n_distinct(year(SampleStartDate)),
               distinct_years_sufficient_crit_period = max(distinct_years_sufficient_crit_period),
               total_results = n()
    ) %>%
    mutate(period = "year_round",
           IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                                   max_3yr_excursions == 1 ~ "3B",
                                   max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ "3",
                                   TRUE ~ '2'),
           Rationale = case_when(max_3yr_excursions >= 2 ~ paste0(MLocID, ": ","Impaired: ", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 1 ~ paste0(MLocID, ": Insufficient data:", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ paste0(MLocID, ": Insufficient data: insufficient data collected during critical warm period- ",
                                                                                                              total_results, " total results"),
                                 TRUE ~ paste0(MLocID, ": Attaining: No 7DADM excursions- ",
                                               total_results, " total results"))) %>%
    mutate(IR_category = factor(IR_category, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE))
  

  
  WS_GNIS_rollup <- temp_IR_categories_WS %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
              distinct_years_sufficient_crit_period_max = max(distinct_years_sufficient_crit_period),
              IR_category_GNIS_26 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ) ) %>% 
    mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
     mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  
  
  WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")
  
  

  
  WS_GNIS_rollup_delist <- WS_GNIS_rollup |>
    mutate(Delist_eligability = case_when( prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_26 == '2' & distinct_years_sufficient_crit_period_max >=3 ~ "Delist Eligible",
                               prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_26 == '2' & distinct_years_sufficient_crit_period_max < 3 ~ 'Insuffcient data to delist')) |>
    mutate(final_GNIS_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
                                      TRUE ~ final_GNIS_cat),
           Rationale_GNIS = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale_GNIS),
                                      TRUE ~ Rationale_GNIS)) |>
    mutate(final_GNIS_cat = factor(final_GNIS_cat,levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
    mutate(status_change = case_when(final_GNIS_cat == prev_GNIS_category & IR_category_GNIS_26 == 'Unassessed' ~ "No change in status- No new assessment",
                                     final_GNIS_cat == prev_GNIS_category ~ "No change in status- Assessed",
                                     final_GNIS_cat == '2' & prev_GNIS_category %in% c('5','4A','4B', '4C') ~ "Delist",
                                     prev_GNIS_category == 'Unassessed' ~ "New Assessment",
                                     prev_GNIS_category == '2' & final_GNIS_cat  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
                                     prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
                                     prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('2') ~ "Insufficient to Attain"
                                     )) |>
    mutate(Char_Name = 'Temperature, water')
  
  # WS_AU_rollup <- WS_GNIS_rollup_delist %>%
  #   ungroup() %>%
  #   mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
  #                                     TRUE ~ Rationale_GNIS)) |> 
  #   group_by(AU_ID, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
  #   summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
  #             IR_category_AU_24 = max(final_GNIS_cat),
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
  
  WS_AU_rollup <-  rollup_WS_AU(WS_GNIS_rollup_delist, char_name_field = Char_Name)
  WS_AU_rollup_joined <-  WS_AU_prev_list(WS_AU_rollup) 

    

 
  
  


## Other Unit  -------------------------------------------------------------------------------------------
  
  
  print("Begin Year Round Other AU categorization")
  
  crit_period_check <- other_3_year %>%
    mutate(year =  year(SampleStartDate)) %>%
    group_by(AU_ID, MLocID,  Pollu_ID, wqstd_code, year) %>%
    summarise(num_crit = sum(In_crit_period)) %>%
    ungroup() %>%
    group_by(AU_ID, MLocID,  Pollu_ID, wqstd_code) %>%
    summarise(distinct_years_sufficient_crit_period = n_distinct(year[num_crit > .8 * 92 ]))
  

### Year Round Other Categorization ---------------------------------------------------------------------------------

  
  
  temp_IR_categories_other <- other_3_year %>%
    left_join(crit_period_check) %>%
    group_by(AU_ID,  Pollu_ID, wqstd_code) %>%
    summarise( stations =  stringr::str_c(unique(MLocID), collapse = "; "),
               Temp_Criteria = first(Temp_Criteria),
               data_period_start = min(SampleStartDate),
               data_period_end = max(SampleStartDate),
               total_valid_excursions = sum(year_round_excursion),
               total_air_exclusions = sum(year_round_Violation) -sum(year_round_excursion) ,
               max_3yr_excursions = max(excursions_3yr),
               max_3yr_results_in_crit_period = max(samples_crit_period),
               distinct_years = n_distinct(year(SampleStartDate)),
               distinct_years_sufficient_crit_period = max(distinct_years_sufficient_crit_period),
               total_results = n()
               
    ) %>%
    mutate(period = "year_round",
           IR_category =  case_when(max_3yr_excursions >= 2 ~ "5",
                                    max_3yr_excursions == 1 ~ "3B",
                                    max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1 ~ "3",
                                    max_3yr_excursions == 1 ~ "3B",
                                    TRUE ~ '2'),
           Rationale = case_when(max_3yr_excursions >= 2 ~ paste0("Impaired: ", total_valid_excursions, 
                                                                  " valid excursions of criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule"),
                                 max_3yr_excursions == 1 ~ paste0("Insufficient data:", total_valid_excursions, 
                                                                  " valid excursions of criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule"),
                                 max_3yr_excursions == 0 & distinct_years_sufficient_crit_period < 1~ paste0("Insufficient data: insufficient data collected during critical warm period"),
                                 TRUE ~ 'Attaining: No 7DADM excursions'))%>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
    mutate(Delist_eligability = case_when(distinct_years_sufficient_crit_period >=3 ~ 1,
                                          TRUE ~ 0))
                                                                                                                 

  other_category_yr <- join_prev_assessments(temp_IR_categories_other, AU_type = "Other")
  
  
  other_category_yr_delist <-  assess_delist(other_category_yr, type = "Other")
  
  # 
  # temp_IR_categories_other_delist <- temp_IR_categories_other |> 
  #   mutate(Delist_eligability = case_when( prev_category %in% c('5',  '4A') & IR_category == '2' & distinct_years_sufficient_crit_period >=3 ~ "Delist Eligible",
  #                                          prev_category %in% c('5',  '4A') & IR_category == '2' & distinct_years_sufficient_crit_period < 3 ~ 'Insuffcient data to delist')) |> 
  #   mutate(final_AU_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
  #                                     TRUE ~ final_AU_cat),
  #          Rationale = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale),
  #                                     TRUE ~ Rationale)) |> 
  #   mutate(final_AU_cat = factor(final_AU_cat,levels=c("Unassessed",'3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
  #   mutate(status_change = case_when(final_AU_cat == prev_category & IR_category == 'Unassessed' ~ "No change in status- No new assessment",
  #                                    final_AU_cat == prev_category ~ "No change in status- Assessed",
  #                                    final_AU_cat == '2' & prev_category %in% c('5','4A','4B', '4C') ~ "Delist",
  #                                    prev_category == 'Unassessed' ~ "New Assessment",
  #                                    prev_category == '2' & final_AU_cat  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
  #                                    prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
  #                                    prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('2') ~ "Insufficient to Attain"
  #   ))|> 
  #   mutate(year_last_assessed = case_when(status_change != 'No change in status- No New Assessment' ~"2026",
  #                                         TRUE ~ year_last_assessed))
  # 
  
  
  # Spawning --------------------------------------------------------------------------------------------------------
  
  
  ## Watershed units -------------------------------------------------------------------------------------------------
  
  print("Begin Spawning watershed unit categorization")
  
  crit_period_check <- ws_3_year %>%
    filter(Spawn_type == "Spawn") %>%
    mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
    mutate(year =  year(SampleStartDate)) %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code, Spawn_type,spawn_length, year) %>%
    summarise(num_spawn = sum(Spawn_type == "Spawn")) %>%
    ungroup() %>%
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code) %>%
    summarise(distinct_years_sufficient_spawn_period = n_distinct(year[num_spawn > .8 * min(spawn_length)]))
  
  

# Delist_crit_period_check ----------------------------------------------------------------------------------------
# This section performs the temperature delisting minimum data check
  #spawn_crit_periods is a table of spawning critical periods. We make them dates, assess each result as inside or ourisde these date ranges
  #and then sum up number of years covering at least 80% of these dates. 
  
  
  # Table of spawning critical periods
  spawn_crit_periods <- data.frame(
    stringsAsFactors = FALSE,
    SpawnCode = c(12L,13L,14L,15L,
                  16L,18L,19L,20L,21L,22L,23L,24L,25L,27L,
                  28L,29L,30L),
    Spawn_Dates = c("September 1 - May 15","September 1 - June 15",
                    "September 15 - May 15","September 15 - June 15",
                    "October 23 - April 15","October 1 - June 15","October 15 - May 15",
                    "October 15 - June 15","November 1 - May 15",
                    "November 1 - May 1","November 1 - June 15",
                    "January 1 - May 15","January 1 - June 15",
                    "August 1 - June 15","August 15 - June 15","August 15 - May 15",
                    "October 15 - March 31"),
    Total_Spawning_Days = c(257L,288L,242L,
                            273L,174L,257L,212L,243L,195L,181L,226L,134L,
                            165L,318L,305L,273L,167L),
    Fall_Critical_Period = c("9/01 - 11/30",
                             "9/01 - 11/30","9/15 - 11/30","9/15 - 11/30",
                             "10/23 - 11/30","10/01 - 11/30","10/15 - 11/30",
                             "10/15 - 11/30","11/01 - 11/30","11/01 - 11/30",
                             "11/01 - 11/30",NA_character_,NA_character_,"8/01 - 11/30","8/15 - 11/30",
                             "8/15 - 11/30","10/15 - 11/30"),
    Spring_Critical_Period = c("4/01 - 5/15",
                               "4/01 - 6/15","4/01 - 5/15","4/01 - 6/15",
                               "4/01 - 4/15","4/01 - 6/15","4/01 - 5/15","4/01 - 6/15",
                               "4/01 - 5/15","4/01 - 5/01","4/01 - 6/15",
                               "4/01 - 5/15","4/01 - 6/15","4/01 - 6/15","4/01 - 6/15",
                               "4/01 - 5/15",NA_character_)
  )
  
  
  spawn_delist_crit_period_check <- ws_3_year |> 
    ungroup() |> 
    #Filter to only results in spawning periods
    filter(Spawn_type == "Spawn") %>%
    #join table of spawning critical periods
    left_join(spawn_crit_periods) |> 
    #Break up the ranges of critical periods into start/end dates
    mutate(fall_crit_start = word(Fall_Critical_Period, 1),
           fall_crit_end = word(Fall_Critical_Period, 3),
           spring_crit_start = word(Spring_Critical_Period, 1),
           spring_crit_end = word(Spring_Critical_Period, 3)) |>
    #Add year of result to spawn dates to get actual date of spawning periods
    mutate(fall_crit_start = case_when(!is.na(fall_crit_start) ~ paste0(fall_crit_start, "/", year(SampleStartDate))),
           fall_crit_end = case_when(!is.na(fall_crit_end) ~ paste0(fall_crit_end, "/", year(SampleStartDate))),
           spring_crit_start = case_when(!is.na(spring_crit_start) ~ paste0(spring_crit_start, "/", year(SampleStartDate))),
           spring_crit_end = case_when(!is.na(spring_crit_end) ~ paste0(spring_crit_end, "/", year(SampleStartDate))),
    ) |> 
    #format as date type
    mutate(fall_crit_start = mdy(fall_crit_start),
           fall_crit_end = mdy(fall_crit_end),
           spring_crit_start = mdy(spring_crit_start),
           spring_crit_end = mdy(spring_crit_end)
    ) |> 
    #calculate number of critical spawning dates in each period
    mutate(total_fall_crit_dates = fall_crit_end - fall_crit_start + 1,
           total_spring_crit_dates = spring_crit_end - spring_crit_start + 1) |> 
    #Determin if sample is inside of critical spawning period date ranges
    mutate(is_fall_crit = case_when(between(SampleStartDate,fall_crit_start, fall_crit_end ) ~ 1,
                                    !(between(SampleStartDate,fall_crit_start, fall_crit_end )) ~ 0),
           is.spring_crit =  case_when(between(SampleStartDate,spring_crit_start, spring_crit_end ) ~ 1,
                                       !(between(SampleStartDate,spring_crit_start, spring_crit_end )) ~ 0)) |> 
    group_by(MLocID, year(Start_spawn)) |>
    #summarize my mlocID the number of results in critical period date range as well as if it is >= 80% of total critical spawning dates
    summarise(Spawn_Dates = first(Spawn_Dates),
              Start_spawn = first(Start_spawn),
              End_spawn = first(End_spawn),
              fall_crit_start = max(fall_crit_start),
              fall_crit_end = max(fall_crit_end),
              spring_crit_start = max(spring_crit_start),
              spring_crit_end = max(spring_crit_end),
              num_fall_crit_dates = max(total_fall_crit_dates),
              total_fall_crit_date_results = n_distinct(SampleStartDate[is_fall_crit == 1]),
              num_spring_crit_dates = max(total_spring_crit_dates),
              total_spring_crit_date_results = n_distinct(SampleStartDate[is.spring_crit == 1]),
              meets_fall_delist_min = case_when(total_fall_crit_date_results >= 0.8 * num_fall_crit_dates ~ 1,
                                                is.na(num_fall_crit_dates) ~ NA_integer_,
                                                TRUE ~ 0),
              meets_spring_delist_min = case_when(total_spring_crit_date_results >= 0.8 * num_spring_crit_dates ~ 1,
                                                  is.na(num_spring_crit_dates) ~ NA_integer_,
                                                  TRUE ~ 0),
              meets_delist_mins = case_when(anyNA(meets_fall_delist_min, meets_spring_delist_min) & sum(meets_fall_delist_min, meets_spring_delist_min, na.rm = TRUE) == 1 ~ 1,
                                            !anyNA(meets_fall_delist_min, meets_spring_delist_min) & sum(meets_fall_delist_min, meets_spring_delist_min, na.rm = TRUE) == 2 ~ 1,
                                            TRUE ~ 0)) |> 
    group_by(MLocID) |> 
    #get total years that meets condition
    summarise(num_yrs_meets_delist_mins = sum(meets_delist_mins))
  
  

## end Ws spawn crit period check ----------------------------------------------------------------------------------


## ws spawn categorization -----------------------------------------------------------------------------------------

  
  
  
  
  
  temp_IR_categories_WS_spawn <- ws_3_year %>%
    filter(Spawn_type == "Spawn") %>%
    mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
    left_join(crit_period_check) %>%
    left_join(spawn_delist_crit_period_check) |> 
    group_by(AU_ID, MLocID, AU_GNIS_Name, Pollu_ID, wqstd_code) %>%
    summarise( Temp_Criteria = 13,
               data_period_start = min(SampleStartDate),
               data_period_end = max(SampleStartDate),
               total_valid_excursions = sum(spawn_excursion),
               total_air_exclusions = sum(Spawn_Violation) -sum(spawn_excursion) ,
               max_3yr_excursions = max(excursions_spawn_3yr),
               max_3yr_results_in_spawn_period = max(samples_spawn),
               distinct_years = n_distinct(year(SampleStartDate)),
               spawn_period_length = first(spawn_length),
               distinct_years_sufficient_spawn_period = max(distinct_years_sufficient_spawn_period),
               distinct_years_sufficient_spawn_period_delist = max(num_yrs_meets_delist_mins),
               total_results = n()
    ) %>%
    mutate(period = "spawn",
           IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                                   max_3yr_excursions == 1 ~ "3B",
                                   max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ "3",
                                   TRUE ~ '2'),
           Rationale = case_when(max_3yr_excursions >= 2 ~ paste0(MLocID, ": ", "Impaired: ", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 1 ~ paste0(MLocID, ": Insufficient data:", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ paste0(MLocID, ": Insufficient data: insufficient data collected during spawn period- ",
                                                                                                               total_results, " total results"),
                                 TRUE ~ paste0(MLocID, ": Attaining: No 7DADM excursions- ",
                                               total_results, " total results"))) %>%
    mutate(IR_category = factor(IR_category, levels=c("Unassessed",'3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE))
  
 
  
  WS_GNIS_rollup_spawn <- temp_IR_categories_WS_spawn %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              distinct_years_sufficient_crit_period_max = max(distinct_years_sufficient_spawn_period_delist),
              IR_category_GNIS_26 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ) ) %>% 
    mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c("Unassessed",'3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
    mutate(Delist_eligability = case_when(distinct_years_sufficient_crit_period_max >=3 ~ 1,
                                          TRUE ~ 0))
  
  WS_GNIS_rollup_spawn <- join_prev_assessments(WS_GNIS_rollup_spawn, AU_type = "WS")
  
  WS_GNIS_rollup_delist_spawn <- assess_delist(WS_GNIS_rollup_spawn, type = 'WS') |> 
    mutate(Char_Name = 'Temperature, water')
  
  
   # WS_GNIS_rollup_delist_spawn <- WS_GNIS_rollup_spawn |> 
   #   mutate(Delist_eligability = case_when( prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_26 == '2' & distinct_years_sufficient_crit_period_max >=3 ~ "Delist Eligible",
   #                                          prev_GNIS_category %in% c('5',  '4A') & IR_category_GNIS_26 == '2' & distinct_years_sufficient_crit_period_max < 3 ~ 'Insuffcient data to delist')) |> 
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
   
   
# 
#    WS_AU_rollup_spawn <- WS_GNIS_rollup_delist_spawn %>%
#      ungroup() %>%
#      mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
#                                        TRUE ~ Rationale_GNIS)) |>
#      group_by(AU_ID, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
#      summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
#                IR_category_AU_24 = max(final_GNIS_cat),
#                Rationale_AU = str_c(Rationale_GNIS,collapse =  " ~ " ) ) %>%
#      mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |>
#      mutate(status_change = case_when(IR_category_AU_24 == prev_AU_category & is.na(Rationale_AU) ~ "No change in status- No New Assessment",
#                                       IR_category_AU_24 == prev_AU_category & !is.na(Rationale_AU)~ "No change in status- New Assessment",
#                                       IR_category_AU_24 == '2' & prev_AU_category %in% c('5','4A','4B', '4C') ~ "Delist",
#                                       prev_AU_category == 'Unassessed' | is.na(prev_AU_category)  ~ "New Assessment",
#                                       prev_AU_category == '2' & IR_category_AU_24  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
#                                       prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
#                                       prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_24 %in% c('2') ~ "Insufficient to Attain",
#                                       prev_AU_category == '4A' & IR_category_AU_24 == '5' ~ "No change in status- New Assessment"
#      ))
   

   

   
   WS_AU_rollup_spawn <-  rollup_WS_AU(WS_GNIS_rollup_delist_spawn, char_name_field = Char_Name)
   WS_AU_rollup_joined_spawn <-  WS_AU_prev_list(WS_AU_rollup_spawn) 
   
   
   
   
   # WS_AU_rollup_joined_spawn <- WS_AU_rollup_spawn |> 
   #   left_join(select(prev_list_AU, -prev_category, -prev_rationale, -Pollutant)) |> 
   #   mutate(Year_listed = case_when(status_change %in% c("Attain to Impaired", "Insufficient to Impaired","New Assessment") &
   #                                    IR_category_AU_24  %in% c('5','4A','4B', '4C') &
   #                                    is.na(Year_listed) ~ '2024',
   #                                  TRUE ~ Year_listed)) |> 
   #   mutate(year_last_assessed = case_when(status_change != 'No change in status- No New Assessment' ~"2024",
   #                                         TRUE ~ year_last_assessed))
   # 
   # 
 
  
  
  # Spawn other -----------------------------------------------------------------------------------------------------
  
  print("Begin spawning other AU categorization")
  
  
  
  
  crit_period_check <- other_3_year %>%
    filter(Spawn_type == "Spawn") %>%
    mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
    mutate(year =  year(SampleStartDate)) %>%
    group_by(AU_ID, MLocID,  Pollu_ID, wqstd_code, Spawn_type,spawn_length, year) %>%
    summarise(num_spawn = sum(Spawn_type == "Spawn")) %>%
    ungroup() %>%
    group_by(AU_ID, MLocID,  Pollu_ID, wqstd_code) %>%
    summarise(distinct_years_sufficient_spawn_period = n_distinct(year[num_spawn > .8 * min(spawn_length)]))
  
  
  
  spawn_delist_crit_period_check <- other_3_year |> 
    ungroup() |> 
    #Filter to only results in spawning periods
    filter(Spawn_type == "Spawn") %>%
    #join table of spawning critical periods
    left_join(spawn_crit_periods) |> 
    #Break up the ranges of critical periods into start/end dates
    mutate(fall_crit_start = word(Fall_Critical_Period, 1),
           fall_crit_end = word(Fall_Critical_Period, 3),
           spring_crit_start = word(Spring_Critical_Period, 1),
           spring_crit_end = word(Spring_Critical_Period, 3)) |>
    #Add year of result to spawn dates to get actual date of spawning periods
    mutate(fall_crit_start = case_when(!is.na(fall_crit_start) ~ paste0(fall_crit_start, "/", year(SampleStartDate))),
           fall_crit_end = case_when(!is.na(fall_crit_end) ~ paste0(fall_crit_end, "/", year(SampleStartDate))),
           spring_crit_start = case_when(!is.na(spring_crit_start) ~ paste0(spring_crit_start, "/", year(SampleStartDate))),
           spring_crit_end = case_when(!is.na(spring_crit_end) ~ paste0(spring_crit_end, "/", year(SampleStartDate))),
    ) |> 
    #format as date type
    mutate(fall_crit_start = mdy(fall_crit_start),
           fall_crit_end = mdy(fall_crit_end),
           spring_crit_start = mdy(spring_crit_start),
           spring_crit_end = mdy(spring_crit_end)
    ) |> 
    #calculate number of critical spawning dates in each period
    mutate(total_fall_crit_dates = fall_crit_end - fall_crit_start + 1,
           total_spring_crit_dates = spring_crit_end - spring_crit_start + 1) |> 
    #Determin if sample is inside of critical spawning period date ranges
    mutate(is_fall_crit = case_when(between(SampleStartDate,fall_crit_start, fall_crit_end ) ~ 1,
                                    !(between(SampleStartDate,fall_crit_start, fall_crit_end )) ~ 0),
           is.spring_crit =  case_when(between(SampleStartDate,spring_crit_start, spring_crit_end ) ~ 1,
                                       !(between(SampleStartDate,spring_crit_start, spring_crit_end )) ~ 0)) |> 
    group_by(AU_ID, year(Start_spawn)) |>
    #summarize my mlocID the number of results in critical period date range as well as if it is >= 80% of total critical spawning dates
    summarise(Spawn_Dates = first(Spawn_Dates),
              Start_spawn = first(Start_spawn),
              End_spawn = first(End_spawn),
              fall_crit_start = max(fall_crit_start),
              fall_crit_end = max(fall_crit_end),
              spring_crit_start = max(spring_crit_start),
              spring_crit_end = max(spring_crit_end),
              num_fall_crit_dates = max(total_fall_crit_dates),
              total_fall_crit_date_results = n_distinct(SampleStartDate[is_fall_crit == 1]),
              num_spring_crit_dates = max(total_spring_crit_dates),
              total_spring_crit_date_results = n_distinct(SampleStartDate[is.spring_crit == 1]),
              meets_fall_delist_min = case_when(total_fall_crit_date_results >= 0.8 * num_fall_crit_dates ~ 1,
                                                is.na(num_fall_crit_dates) ~ NA_integer_,
                                                TRUE ~ 0),
              meets_spring_delist_min = case_when(total_spring_crit_date_results >= 0.8 * num_spring_crit_dates ~ 1,
                                                  is.na(num_spring_crit_dates) ~ NA_integer_,
                                                  TRUE ~ 0),
              meets_delist_mins = case_when(anyNA(meets_fall_delist_min, meets_spring_delist_min) & sum(meets_fall_delist_min, meets_spring_delist_min, na.rm = TRUE) == 1 ~ 1,
                                            !anyNA(meets_fall_delist_min, meets_spring_delist_min) & sum(meets_fall_delist_min, meets_spring_delist_min, na.rm = TRUE) == 2 ~ 1,
                                            TRUE ~ 0)) |> 
    group_by(AU_ID) |> 
    #get total years that meets condition
    summarise(num_yrs_meets_delist_mins = sum(meets_delist_mins))
  
  
  temp_IR_categories_other_spawn <- other_3_year %>%
    filter(Spawn_type == "Spawn") %>%
    mutate(spawn_length = as.double(difftime(End_spawn,Start_spawn,unit="days"))) %>%
   # left_join(crit_period_check) %>%
    left_join(spawn_delist_crit_period_check) |> 
    group_by(AU_ID,  Pollu_ID, wqstd_code) %>%
    summarise( stations =  stringr::str_c(unique(MLocID), collapse = "; "),
               Temp_Criteria = 13,
               data_period_start = min(SampleStartDate),
               data_period_end = max(SampleStartDate),
               total_valid_excursions = sum(spawn_excursion),
               total_air_exclusions = sum(Spawn_Violation) -sum(spawn_excursion) ,
               max_3yr_excursions = max(excursions_spawn_3yr),
               max_3yr_results_in_spawn_period = max(samples_spawn),
               distinct_years = n_distinct(year(SampleStartDate)),
               spawn_period_length = first(spawn_length),
               distinct_years_sufficient_spawn_period = max(num_yrs_meets_delist_mins),
               total_results = n()
    ) %>%
    mutate(period = "spawn",
           IR_category = case_when(max_3yr_excursions >= 2 ~ "5",
                                   max_3yr_excursions == 1 ~ "3B",
                                   max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ "3",
                                   TRUE ~ '2'),
           Rationale = case_when(max_3yr_excursions >= 2 ~ paste0("Impaired: ", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 1 ~ paste0("Insufficient data:", total_valid_excursions, 
                                                                  " valid excursions of ", Temp_Criteria, 
                                                                  "° criteria. ", total_air_exclusions, 
                                                                  " excursions marked invald due to air temp exclusion rule- ",
                                                                  total_results, " total results"),
                                 max_3yr_excursions == 0 & distinct_years_sufficient_spawn_period < 1 ~ paste0("Insufficient data: insufficient data collected during spawn period- ",
                                                                                                               total_results, " total results"),
                                 TRUE ~ paste0("Attaining: No 7DADM excursions- ",
                                               total_results, " total results"))) %>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) %>%
    mutate(recordID = paste0("2022-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))|> 
    mutate(Delist_eligability = case_when(distinct_years_sufficient_spawn_period >=3 ~ 1,
                                          distinct_years_sufficient_spawn_period < 3 ~ 0))
  
  
  other_category_spawn <- join_prev_assessments(temp_IR_categories_other_spawn, AU_type = 'Other')

  
  other_category_delist_spawn <-  assess_delist(other_category_spawn, type = "Other")|> 
    mutate(Char_Name = 'Temperature, water')
  


# prep for export -------------------------------------------------------------------------------------------------


## pull together AU decisions --------------------------------------------------------------------------------------

  
  
  AU_display_other_yearround <- other_category_yr_delist |> 
    select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations,  recordID, status_change, Year_listed,  year_last_assessed)  
  
  
  AU_display_WS_yearround <- WS_AU_rollup_joined |> 
    rename(prev_category = prev_AU_category,
           prev_rationale = prev_AU_rationale,
           final_AU_cat = IR_category_AU_26,
           Rationale = Rationale_AU) |> 
    select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)
  
  AU_display_other_spawn <- other_category_delist_spawn|> 
    select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale,stations, recordID, status_change, Year_listed,  year_last_assessed)  
  
  
  
  AU_display_WS_spawn <- WS_AU_rollup_joined_spawn |> 
    rename(prev_category = prev_AU_category,
           prev_rationale = prev_AU_rationale,
           final_AU_cat = IR_category_AU_26,
           Rationale = Rationale_AU) |> 
    select(AU_ID, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)
 
   AU_display <- bind_rows(AU_display_other_yearround,AU_display_WS_yearround,  AU_display_other_spawn, AU_display_WS_spawn) |> 
     mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                                  TRUE ~ Rationale))|> 
     join_TMDL(type = 'AU')|> 
     join_AU_info() |> 
     distinct() |> 
     relocate(prev_category, .after = year_last_assessed) |> 
     relocate(prev_rationale, .after = prev_category) |> 
     arrange(AU_ID) |> 
     mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                           TRUE ~ year_last_assessed)) |> 
     mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                    TRUE ~  Year_listed)) |> 
     mutate(Char_Name = 'Temperature, water') |> 
     mutate(status_change = case_when(prev_category == '5' & final_AU_cat == '4A' ~ "Delist. 5 to 4A",
                                      TRUE ~ status_change )) |> 
     relocate(Char_Name,.after = AU_ID) 
  

## Pull together Other_AU_categorization decisions ------------------------------------------------------------------------------------
  Other_AU_categorization <- bind_rows(other_category_yr_delist, other_category_delist_spawn) |> 
    relocate(max_3yr_results_in_spawn_period, .after = max_3yr_results_in_crit_period) |> 
    relocate(distinct_years_sufficient_spawn_period, .after = distinct_years_sufficient_crit_period) |> 
    relocate(spawn_period_length, .after = distinct_years)  |> 
     mutate(Char_Name = 'Temperature, water') |> 
     relocate(Char_Name,.after = AU_ID) 

## mloc categorization ---------------------------------------------------------------------------------------------
  temp_IR_categories_WS0 <- temp_IR_categories_WS |> 
    mutate(IR_category = as.character(IR_category))
  
  temp_IR_categories_WS_spawn0 <- temp_IR_categories_WS_spawn |> 
    mutate(IR_category = as.character(IR_category))
  
  WS_station_cat <- bind_rows(temp_IR_categories_WS0,temp_IR_categories_WS_spawn0 ) |> 
    relocate(max_3yr_results_in_spawn_period, .after = max_3yr_results_in_crit_period) |> 
    relocate(distinct_years_sufficient_spawn_period, .after = distinct_years_sufficient_crit_period) |> 
    relocate(spawn_period_length, .after = distinct_years) |> 
    relocate(distinct_years_sufficient_spawn_period_delist, .after = distinct_years_sufficient_spawn_period) |> 
    arrange(MLocID) |> 
    mutate(Char_Name = 'Temperature, water') |> 
    relocate(Char_Name,.after = AU_ID) 
  

# GNIS categorization ---------------------------------------------------------------------------------------------

WS_GNIS_cat <- bind_rows(WS_GNIS_rollup_delist, WS_GNIS_rollup_delist_spawn) |> 
     join_TMDL(type = 'GNIS') |> 
    select(-distinct_years_sufficient_crit_period_max) |> 
    join_AU_info()|> 
    relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
    relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
    relocate(prev_GNIS_rationale, .after = prev_GNIS_category)   |> 
    mutate(Char_Name = 'Temperature, water') |> 
    mutate(status_change = case_when(prev_GNIS_category == '5' & final_GNIS_cat == '4A' ~ "Delist. 5 to 4A",
                                     TRUE ~ status_change)) |> 
    relocate(Char_Name,.after = AU_GNIS_Name) 
   
    
  
  
  
  
    

  if(write_excel){
    
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
    
    
    addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
    
    
    addWorksheet(wb, sheetName = "WS_station_categorization", tabColour = 'lightblue3')
  
    
    addWorksheet(wb, sheetName = "WS_GNIS_categorization", tabColour = 'lightyellow1')
    
    
    addWorksheet(wb, sheetName = "Temperature_Data", tabColour = 'paleturquoise2')

    
    
    
    header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
    freezePane(wb, "AU_Decisions", firstRow = TRUE) 
    freezePane(wb, "Other_AU_categorization", firstRow = TRUE) 
    freezePane(wb, "WS_station_categorization", firstRow = TRUE)
    freezePane(wb, "WS_GNIS_categorization", firstRow = TRUE)
    freezePane(wb, "Temperature_Data", firstRow = TRUE)
 
    
    writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
    writeData(wb = wb, sheet = "Other_AU_categorization", x = Other_AU_categorization, headerStyle = header_st)
    writeData(wb = wb, sheet = "WS_station_categorization", x = WS_station_cat, headerStyle = header_st)
    writeData(wb = wb, sheet = "WS_GNIS_categorization", x = WS_GNIS_cat , headerStyle = header_st)
    writeData(wb = wb, sheet = "Temperature_Data", x = temp_air_exclusion, headerStyle = header_st )
    
    
    print("Writing excel doc")
    saveWorkbook(wb, paste0("Parameters/Outputs/temperature-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
    
  }
  
  
  temperature <- list(AU_Decisions=as.data.frame(AU_display),
                      Other_AU_categorization=as.data.frame(Other_AU_categorization),
                      WS_station_categorization=as.data.frame(WS_station_cat),
                      WS_GNIS_categorization = as.data.frame(WS_GNIS_cat),
                      Temperature_Data =as.data.frame( temp_air_exclusion)
                      
  )
  

  return(temperature)
}