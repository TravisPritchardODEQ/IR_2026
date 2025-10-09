# Note, this was done using the salmonid species present criteria equations. Need to adjust for no salmonid species

ToxAL_Ammonia <- function(database){
  

# Testing ---------------------------------------------------------------------------------------------------------

#database <- "IR_Dev"
  
  print("Fetch Ammonia data from IR database")
  
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  #Build query language to get ammonia data out. this grabs the IR 2018 db view [dbo].[VW_Ammonia_AL]
  
  # db_qry <- glue::glue_sql( "SELECT *
  #                         FROM [IntegratedReport].[dbo].[VW_Ammonia_AL]
  #                         WHERE AU_ID != '99'", .con = con)
  # 
  # # Send query to database and return with the data
  # Results_import <-  DBI::dbGetQuery(con, db_qry)
  
  Results_import <- tbl(con, 'VW_Ammonia_AL') |> 
    filter(AU_ID != '99') |> 
    collect()
  
  Results_import_no_NAs <- Results_import %>%
    filter(!is.na(MLocID))
  
  print(paste("Returned", nrow(Results_import), "results from", length(unique(Results_import$MLocID)), "monitoring locations"))
  
  #Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
  mlocs <- unique(Results_import_no_NAs$MLocID)
  
  # chr_uids for ammonia ancillary data
  # Temp = 2849
  # pH = 1648
  
  print("Fetch ancillary data from IR database")
  ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[InputRaw]
                                WHERE chr_uid in (2849, 1648) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = con)
  
  
  
  #Query to get ancillary data
  Results_ancillary <- DBI::dbGetQuery(con, ancillary_qry)
  
  
  print("Joining ancillary data")
  
  # Spread the data from long format to wide format
  spread <- Results_ancillary %>%
    filter(!Sample_Fraction %in% c("Suspended")) %>%
    group_by(MLocID, SampleStartDate,Char_Name,Result_Depth  ) %>%
    summarise(result = first(IRResultNWQSunit)) %>%
    arrange(MLocID, SampleStartDate) %>%
    spread(key = Char_Name, value = result) %>%
    rename(Temp = "Temperature, water") %>%
    mutate(pH = ifelse(pH < 6.5, 6.5, 
                       ifelse(pH > 9.0, 9.0, pH )))
  
  
  # Join table together
  # Calculate crit
  # Enter fish codes for non-slmonid fish use
  ammonia_data <- Results_import %>%
    left_join(spread, by = c('MLocID', 'SampleStartDate', 'Result_Depth')) %>%
    filter(!is.na(pH) & !is.na(Temp)) %>%
    mutate(crit = ifelse(FishCode %in% c(11, 21, 99), 0.7249 * (0.0114 / (1 + 10^7204-pH)) + (1.6181 / (1 + 10 ^(pH-7204)) * min(51.93, 23.13*10^(0.036*(20-Temp)))),  
                         pmin((0.275/(1 + 10 ^ (7.204 - pH))) + (39.0/ (1 + 10 ^(pH - 7.204 ))) , 
                              0.7249 * ((0.0114/ ( 1 + 10^(7.204 - pH))) + (1.6181 / (1 + 10 ^ (pH - 7.204))) * (23.12*10^(0.036*(20-Temp))))) )  
    )
  
  Results_censored <- censor_data(ammonia_data, criteria_col =  crit ) %>%
    mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction %in% c("Dissolved", "Filterable"),  "Dissolved", "Total" )) %>%
    group_by(MLocID, SampleStartDate, SampleStartTime ,Char_Name,Result_Depth ) %>%
    mutate(has_total = ifelse(max(Simplfied_Sample_Fraction) == "Total", 1, 0 )) %>%
    ungroup() %>%
    filter((has_total == 1 & Simplfied_Sample_Fraction == "Total") |
             (has_total == 0 & Simplfied_Sample_Fraction == "Dissolved") ) %>%
    mutate(excursion = ifelse(Result_cen > crit, 1, 0 ))
  
  
  ammonia_cat_fun <- function(Results_censored, AU_type){
    
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Crit_fraction' )
      
      group2 <- c('AU_ID', 'Char_Name')
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name', 'Crit_fraction')
      
      group2 <- c('AU_ID', 'MLocID', 'Char_Name')
      inverse <- FALSE
    }
    
    
    Results_tox_Ammonia_categories <- Results_censored %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      group_by_at(group1) %>%
      #Summarise data
      summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
                num_samples = n(),
                percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
                num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
                num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
                num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
                num_excursions_all = sum(excursion),
                num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
                num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
                num_samples_crit_excursion_calc =  num_samples_total_fraction + num_excursions_dissolved_fraction, 
                critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
      # Assign categories
      mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                     num_samples_crit_excursion_calc > 2 & num_excursions_all >= critical_excursions ~ "5",
                                     num_samples_crit_excursion_calc < 10 & num_excursions_all > 0 ~ "3B",
                                     num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ "3",
                                     num_samples_crit_excursion_calc >= 10 &num_excursions_all < critical_excursions~  "2" ),
             Rationale =  case_when(percent_3d == 100 ~paste0("Insufficient data: ", "All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                    num_samples_crit_excursion_calc > 2 & num_excursions_all >= critical_excursions ~   paste0("Imapired: ",num_excursions_all,
                                                                                                                               " excursion of criteria with ",
                                                                                                                               num_samples, " total samples. ",
                                                                                                                               num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc < 10 & num_excursions_all > 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                           " excursion of criteria with ",
                                                                                                           num_samples, " total samples. ",
                                                                                                           num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                            " excursion of criteria with ",
                                                                                                            num_samples, " total samples. ",
                                                                                                            num_samples_total_fraction, " results of 'total fraction'."),
                                    num_samples_crit_excursion_calc >= 10 &num_excursions_all < critical_excursions~  paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                                                             critical_excursions, " needed to list- ",
                                                                                                                             num_samples, " total samples. ",
                                                                                                                             num_samples_total_fraction, " results of 'total fraction'.") )) %>%
      mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE))|> 
      mutate(period = NA_character_) |> 
      mutate(Delist_eligability = case_when(num_samples_crit_excursion_calc >= 18 & num_excursions_dissolved_fraction <= binomial_delisting(num_samples, 'Toxics')  ~ 1,
                                            TRUE ~ 0)) 
    
    

             
  
             return(Results_tox_Ammonia_categories)
               
  }
  
  
  
  # char rename -----------------------------------------------------------------------------------------------------
  
  
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)
  
  # Send query to database and return with the data
  Char_rename <-  DBI::dbGetQuery(con, db_qry) |> 
    mutate(Pollu_ID = as.character(Pollu_ID))
  
  
  # Watershed Assessment --------------------------------------------------------------------------------------------
  
  
  AL_tox_Ammonia_WS <- ammonia_cat_fun(Results_censored = Results_censored, AU_type = "WS" )
  
  
  
  
  WS_GNIS_rollup <- AL_tox_Ammonia_WS %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
              IR_category_GNIS_24 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
              Delist_eligability = max(Delist_eligability)) %>% 
    mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_24 == '2'~ 1,
                                          TRUE ~ 0)) |> 
    mutate(IR_category_GNIS_24 = factor(IR_category_GNIS_24, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  
  
  WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS") |> 
    select(-Char_Name) |> 
    left_join(Char_rename) |> 
    relocate(Char_Name, .after = AU_GNIS_Name)
  ### Delist process --------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')
  
  ## AU Rollup -------------------------------------------------------------------------------------------------------
  
  
  WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name)   
  WS_AU_rollup <- WS_AU_prev_list(WS_AU_rollup) 
  
  # Other assessment ------------------------------------------------------------------------------------------------
  
  AL_tox_Ammonia_other <- ammonia_cat_fun(Results_censored = Results_censored,AU_type = "other" )  |> 
    mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |> 
    ungroup()
  
  other_category <- join_prev_assessments(AL_tox_Ammonia_other, AU_type = 'Other')|> 
    select(-Char_Name) |> 
    left_join(Char_rename) |> 
    relocate(Char_Name, .after = AU_ID)
  
  other_category_delist <-  assess_delist(other_category, type = "Other") |> 
    ungroup()
  
  
  
  # prep data for export --------------------------------------------------------------------------------------------
  
  AU_display_other <- other_category_delist |> 
    select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale,stations, recordID, status_change, Year_listed,  year_last_assessed)
  
  AU_display_ws <- WS_AU_rollup |> 
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
                                   TRUE ~ Year_listed)) 
  
  
  WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
    join_TMDL(type = 'GNIS') |> 
    join_AU_info()|> 
    relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
    relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
    relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  
  
    
  AL_ammonia_list <- list(data = Results_censored,
                          AU_Decisions = AU_display,
                          Other_AU_categorization = other_category_delist,
                          WS_Station_cat = AL_tox_Ammonia_WS,
                          WS_GNIS_cat = WS_GNIS_rollup_delist)
  
  return(AL_ammonia_list)
  
  
}