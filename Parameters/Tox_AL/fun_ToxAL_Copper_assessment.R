require(tidyverse)
require(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)
library(dbplyr)


copper_assessment <- function(CU_file= 'Parameters/Tox_AL/Copper_criteria_results.csv', database = "IR_Dev"){
  

# Somewhere in the export/import some MLociD's got truncated. Get a list to fix that ------------------------------

  #CU_file <- 'Parameters/Tox_AL/Copper_criteria_results_2024.csv'
  #database = "IR_Dev"
  
  #open connection to database
  con <- DBI::dbConnect(odbc::odbc(), database)
  
  CU_stations <- tbl(con, 'VW_Copper') %>%
    select(MLocID, Result_UID) %>%
    collect() %>%
    rename(MLocID_cor = MLocID)
  
  save(CU_stations, file = 'CU_stations.Rdata')
  
  
Cu_BLM <- read.csv(CU_file)

# get criteria values - FW crit = lowest 

Cu_BLM_crit <- Cu_BLM %>%
  left_join(CU_stations,  by = "Result_UID") %>%
  mutate(MLocID = MLocID_cor) %>%
  select(-MLocID_cor) %>%
  #lowest SW criteria 
  mutate(CCC_SW = 3.1) %>%
  mutate(crit = case_when(WaterTypeCode == 2 ~  pmin(CCC_ug.L,CMC_ug.L), 
                          WaterTypeCode == 3 ~ CCC_SW,
                          WaterTypeCode == 1 ~ pmin(CCC_ug.L,CMC_ug.L,CCC_SW),
                          TRUE ~ pmin(CCC_ug.L,CMC_ug.L))) %>%
  mutate(crit = as.numeric(crit)) %>%
  mutate(Crit_fraction = "Dissolved")

print('Begin analysis')

# Use the conversion factor to transform total results to dissolved results  
Results_censored <- censor_data(Cu_BLM_crit) %>%
  mutate(Result_cen = as.numeric(Result_cen)) %>%
  mutate(Simplfied_Sample_Fraction = ifelse(Sample_Fraction ==  "Dissolved",  "Dissolved", "Total" )) %>%
  group_by(MLocID, SampleStartDate, Char_Name,Result_Depth) %>%
  mutate(Has_Crit_Fraction = ifelse(Crit_fraction == "Total" & max(Simplfied_Sample_Fraction) == "Total", 1, 
                                    ifelse(Crit_fraction == "Dissolved" & min(Simplfied_Sample_Fraction) == "Dissolved", 1, 0 ))) %>%
  # Filter out the results that do not macth criteira fraction, if the group has matching criteria. Also keep where whole group does not match
  ungroup() %>%
  filter((Has_Crit_Fraction == 1 & Simplfied_Sample_Fraction == Crit_fraction) | Has_Crit_Fraction == 0) %>%
  mutate(excursion = ifelse(Result_cen > crit , 1, 0 ))


# Assessment function ---------------------------------------------------------------------------------------------



AL_tox_CU_assess_fun <- function(df_data = Results_censored, AU_type){
  
  # 
  if(AU_type == "other"){  
    group1 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'Char_Name')
    
    
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name',  'Pollu_ID', 'wqstd_code', 'Char_Name' )
    
    
    inverse <- FALSE
  }
  
  
  Results_tox_AL_CU_cats <- df_data %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    group_by_at(group1) %>%
    #Summarise data
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              criteria_fraction = first(Crit_fraction),
              num_samples = n(),
              percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > crit )/num_samples * 100),
              num_fraction_types = n_distinct(Simplfied_Sample_Fraction),
              num_samples_total_fraction = sum(Simplfied_Sample_Fraction == "Total"),
              num_Samples_dissolved_fraction = sum(Simplfied_Sample_Fraction == "Dissolved"),
              num_excursions_all = sum(excursion),
              num_excursions_total_fraction = sum(excursion[Simplfied_Sample_Fraction == "Total"]),
              num_excursions_dissolved_fraction = sum(excursion[Simplfied_Sample_Fraction == "Dissolved"]),
              num_samples_crit_excursion_calc = ifelse(criteria_fraction == "Total", num_samples_total_fraction + num_excursions_dissolved_fraction, 
                                                       num_Samples_dissolved_fraction + (num_samples_total_fraction - num_excursions_total_fraction )),
              critical_excursions = binomial_excursions(num_samples_crit_excursion_calc, type = "Toxics")) %>%
    # Assign categories
    mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                   num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~ "5",
                                   num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~ "5",
                                   num_samples < 10 & num_excursions_all >= 1 ~ "3B",
                                   criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ "3B",
                                   num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ "3",
                                   
                                   num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ "2"
    ),
    Rationale = case_when(percent_3d == 100 ~ paste0("Insufficient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                          
                          num_samples >= 2 & criteria_fraction == "Dissolved" & num_excursions_dissolved_fraction >= critical_excursions ~  paste0("Impaired: ", num_excursions_dissolved_fraction,
                                                                                                                                                   " excursion of dissolved fraction results. ",
                                                                                                                                                   num_samples, " total samples"),
                          num_samples >= 2 & criteria_fraction == "Total" & num_excursions_all >= critical_excursions ~  paste0(num_excursions_all, " excursions is less than ",
                                                                                                                                critical_excursions, " needed to list- ",
                                                                                                                                num_samples, " total samples"),
                          num_samples < 10 & num_excursions_all >= 1 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                              " excursion of criteria with ",
                                                                              num_samples, " total samples"),
                          criteria_fraction == "Dissolved" & num_excursions_total_fraction > 0 & num_Samples_dissolved_fraction == 0 ~ paste0("Insufficient data: ", "Only total fraction results available, criteria is 'Dissolved' ",
                                                                                                                                              num_excursions_all, " total excursions of ", 
                                                                                                                                              num_samples, " total samples"),
                          num_samples_crit_excursion_calc < 10 & num_excursions_all == 0 ~ paste0("Insufficient data: ", num_excursions_all,
                                                                                                  " excursion of criteria with ",
                                                                                                  num_samples, " total samples"),
                          
                          num_samples >= 10 & num_excursions_dissolved_fraction < critical_excursions ~ paste0("Attaining: ", num_excursions_all, " excursions is less than ",
                                                                                                               critical_excursions, " needed to list- ",
                                                                                                               num_samples, " total samples")
    )) %>%
    mutate(IR_category = factor(IR_category, levels=c("3D", "3", "3B", "2", "5" ), ordered=TRUE)) |> 
    mutate(period = NA_character_) |> 
    mutate(Delist_eligability = case_when(num_samples_crit_excursion_calc >= 18 & num_excursions_dissolved_fraction <= binomial_delisting(num_samples, 'Toxics')  ~ 1,
                                          TRUE ~ 0)) 
  
  
    
  return(Results_tox_AL_CU_cats)
  
}


# Watershed assessment --------------------------------------------------------------------------------------------


AL_Tox_CU_WS <- AL_tox_CU_assess_fun(df_data = Results_censored, AU_type = "WS")



WS_GNIS_rollup <- AL_Tox_CU_WS %>%
  ungroup() %>%
  mutate(Rationale = paste0(MLocID, "; ",  Rationale)) |> 
  group_by(AU_ID, AU_GNIS_Name, Char_Name, Pollu_ID, wqstd_code, period) %>%
  summarise(stations =  stringr::str_c(unique(na.omit(stations)), collapse = "; "),
            IR_category_GNIS_26 = max(IR_category),
            Rationale_GNIS = str_c(na.omit(Rationale),collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5", '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS") |> 
  mutate(Char_Name = 'Copper') |> 
  relocate(Char_Name, .after = AU_GNIS_Name)


### Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')

## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name)   
WS_AU_rollup <- WS_AU_prev_list(WS_AU_rollup) 




# Other assessment ------------------------------------------------------------------------------------------------


AL_Tox_CU_other <- AL_tox_CU_assess_fun(df_data = Results_censored, AU_type = "other") |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))


other_category <- join_prev_assessments(AL_Tox_CU_other, AU_type = 'Other') |> 
  mutate(Char_Name = 'Copper') |> 
  relocate(Char_Name, .after = AU_ID)

other_category_delist <-  assess_delist(other_category, type = "Other") |> 
  ungroup()



# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)

AU_display <- bind_rows(AU_display_other, AU_display_ws)|> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
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



AL_copper_list <- list(data = Results_censored,
                        AU_Decisions = AU_display,
                        Other_AU_categorization = other_category_delist,
                        WS_Station_cat = AL_Tox_CU_WS,
                        WS_GNIS_cat = WS_GNIS_rollup_delist)

return(AL_copper_list)


}


