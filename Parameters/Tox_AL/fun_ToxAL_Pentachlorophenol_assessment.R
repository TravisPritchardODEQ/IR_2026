library(odeqIRtools)
library(lubridate)


TOX_AL_penta_analysis <- function(df, database = "IR_Dev"){
  

# testing ---------------------------------------------------------------------------------------------------------
#df <- tox_AL_penta_data
  
  
  # Assign violations
  penta_data_analysis <- df %>%
    mutate(evaluation_crit = ifelse(WaterTypeCode == 2, pmin(CMC_crit, CCC_crit, na.rm = TRUE), 7.9 )) %>%
    mutate(excursion = ifelse(Result_cen > evaluation_crit, 1, 0 ))
  
  fun_penta_analysis <- function(df_data = penta_data_analysis, AU_type){
    
    if(AU_type == "other"){  
      group1 <- c('AU_ID',  'Pollu_ID', 'wqstd_code', 'Char_Name' )
      
    
      inverse <- TRUE
      
      
    } else if (AU_type == "WS"){
      group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name')
      
     
      inverse <- FALSE
    }
  
  #Summarize data and assign critical excursions and IR category
  penta_data_summary <- penta_data_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              num_samples = n(),
              num_excursions = sum(excursion),
              percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > evaluation_crit )/num_samples * 100)) %>%
    mutate(critical_excursions = binomial_excursions(num_samples, type = 'Toxics'),
           IR_category = case_when(percent_3d == 100 ~ "3D",
                                   num_samples < 2 & num_excursions >= critical_excursions ~ "5", 
                                   num_samples < 10  & num_excursions == 0 ~ "3",
                                   num_samples < 10  & num_excursions > 0  ~ "3B",
                                   num_samples >=  10 & num_excursions < critical_excursions ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale =  case_when(percent_3d == 100 ~ paste0("Insufficient data: ", "All results are non-detects with detection limits above criteria- ",
                                                                                  num_samples, " total samples"),
                                  num_samples < 2 & num_excursions >= critical_excursions ~  paste0("Impaired: ", num_excursions,
                                                                                                    " excursion of criteria with ",
                                                                                                    num_samples, " total samples. ", 
                                                                                                    critical_excursions, " needed to list."),
                                  num_samples < 10 & num_excursions == 0 ~ paste0("Insufficient data: ", "Only ",num_samples, " sample with ",
                                                                                  num_excursions," excursions"),
                                  num_samples < 10 & num_excursions > 0 ~ paste0("Insufficient data: ", "Only ",num_samples, " sample with ",
                                                                                  num_excursions," excursion"),
                                  
                                  num_samples >=  10 & num_excursions < critical_excursions ~ paste0("Attaining: ", num_excursions,
                                                                                  " excursion of criteria with ",
                                                                                  num_samples, " total samples. ", 
                                                                                  critical_excursions, " needed to list."),
                                  TRUE ~ "ERROR")) |> 
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) |> 
    mutate(period = NA_character_) |> 
    mutate(Delist_eligability = case_when(num_samples >= 18 & num_excursions <= binomial_delisting(num_samples, 'Toxics')  ~ 1,
                                          TRUE ~ 0)) 
  
    
  

                                  
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
  AL_tox_penta_WS <- fun_penta_analysis(df = penta_data_analysis, AU_type = "WS" )
  
  ## GNIS rollup -----------------------------------------------------------------------------------------------------
  
  
  WS_GNIS_rollup <- AL_tox_penta_WS %>%
    ungroup() %>%
    group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
    summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
              IR_category_GNIS_26 = max(IR_category),
              Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
              Delist_eligability = max(Delist_eligability)) %>% 
    mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                          TRUE ~ 0)) |> 
    mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  
  
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
  
  
  AL_tox_penta_other <- fun_penta_analysis(df = penta_data_analysis,AU_type = "other" )
  
  other_category <- join_prev_assessments(AL_tox_penta_other, AU_type = 'Other')|> 
    select(-Char_Name) |> 
    left_join(Char_rename) |> 
    relocate(Char_Name, .after = AU_ID)
  
  other_category_delist <-  assess_delist(other_category, type = "Other")  
  
  
  
  # prep data for export --------------------------------------------------------------------------------------------
  
  AU_display_other <- other_category_delist |> 
    select(AU_ID,Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
           final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)
  
  AU_display_ws <- WS_AU_rollup |> 
    rename(prev_category = prev_AU_category,
           prev_rationale = prev_AU_rationale,
           final_AU_cat = IR_category_AU_26,
           Rationale = Rationale_AU)
  
  AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
    mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                                 .default = Rationale))|> 
    join_TMDL(type = 'AU')|> 
    join_AU_info() |> 
    relocate(prev_category, .after = year_last_assessed) |> 
    relocate(prev_rationale, .after = prev_category)|> 
    mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                          TRUE ~ year_last_assessed)) |> 
    mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                   TRUE ~  Year_listed)) 
  
  
  WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
    join_TMDL(type = 'GNIS') |> 
    join_AU_info()|> 
    relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
    relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
    relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  
  
  
  # Export ----------------------------------------------------------------------------------------------------------
  
  Tox_AL_penta_list <- list(data = penta_data_analysis,
                              AU_Decisions = AU_display,
                              Other_AU_categorization = other_category_delist,
                              WS_Station_cat = AL_tox_penta_WS,
                              WS_GNIS_cat = WS_GNIS_rollup_delist)
  
  return(Tox_AL_penta_list)

}