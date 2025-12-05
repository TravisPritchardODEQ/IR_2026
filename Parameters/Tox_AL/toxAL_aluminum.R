library(tidyverse)
library(odeqIRtools) 
library(odbc)
library(DBI)
library(glue)
library(deqalcrit)


aluminum_assessment <- function(database){
# Testing ---------------------------------------------------------------------------------------------------------

#database <- "IR_Dev"  

# Database fetch --------------------------------------------------------------------------------------------------

# Connect to the IR database
IR.sql <-   DBI::dbConnect(odbc::odbc(), database)

# Pull selected data from InputRaw.
IR_Res_qry <-
  "Select  
*
  FROM [IntegratedReport].[dbo].[VW_Aluminum]" 

Results_import <- DBI::dbGetQuery(IR.sql, glue::glue_sql(IR_Res_qry, .con = IR.sql))

Results_import_no_NAs <- Results_import %>%
  filter(!is.na(MLocID))

print(paste("Returned", nrow(Results_import), 
            "results from", length(unique(Results_import$MLocID)), "monitoring locations"))

Results_import_no_NAs <- odeqIRtools::data_aggregation(Results_import_no_NAs)

#Create a vector of monitoring locations with metals data. This list is used as a filter for the hardness query
mlocs <- unique(Results_import_no_NAs$MLocID)




# Get ancillary data ----------------------------------------------------------------------------------------------

print("Fetch ancillary data from IR database")

ancillary_params <- c("Organic carbon",
                      "pH",
                      'Hardness, Ca, Mg',
                      'Hardness, non-carbonate',
                      'Calcium',
                      "Magnesium",
                      "Conductivity",
                      "Specific conductance")

ancillary_qry <- glue::glue_sql("SELECT [MLocID]
                                ,[SampleStartDate]
                                ,chr_uid
                                ,[Char_Name]
                                ,[Char_Speciation]
                                ,SampleMedia
                                ,SampleSubmedia
                                ,[Sample_Fraction]
                                ,[IRResultNWQSunit]
                                ,[Result_Depth]
                                ,[IRWQSUnitName]
                                FROM [IntegratedReport].[dbo].[ResultsRawWater]
                                WHERE Char_Name in ({ancillary_params*}) 
                                AND (Statistical_Base IS NULL)
                                AND MLocID in ({mlocs*})
                                AND IRResultNWQSunit IS NOT NULL", .con = IR.sql)

#Query to get ancillary data
Results_ancillary <- DBI::dbGetQuery(IR.sql, ancillary_qry)

unique(Results_ancillary$Char_Name)

ancillary_data <- Results_ancillary %>%
  filter(!(Char_Name == 'Organic carbon' & Sample_Fraction == 'Bed Sediment')) %>%
  mutate(Char_Name = ifelse(chr_uid %in% c(544, 100331), 'Alkalinity',
                            ifelse(chr_uid %in% c(1097, 1099), 'Hardness',
                                   ifelse(chr_uid == 2174 & Sample_Fraction %in% c("Total", 'Total Recoverable','Suspended')  , 'TOC',
                                          ifelse(chr_uid == 2174 & Sample_Fraction == "Dissolved", 'DOC', Char_Name ))))) %>%
  mutate(IRResultNWQSunit = ifelse(IRResultNWQSunit == 'ug/l', IRResultNWQSunit / 1000, IRResultNWQSunit),
         Result_Unit = ifelse(IRWQSUnitName == 'ug/l', "mg/L", IRWQSUnitName)) %>%
  mutate(Simplified_Sample_fraction = ifelse(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  |
                                               is.na(Sample_Fraction), 'Total',
                                             ifelse(Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  , "Dissolved", "Error"))) %>%
  group_by(MLocID,  SampleStartDate,Char_Name,SampleMedia, SampleSubmedia ) %>%
  mutate(Has_dissolved = ifelse(min(Simplified_Sample_fraction) == "Dissolved", 1, 0 )) %>%
  ungroup() %>%
  filter((Has_dissolved == 1 & Simplified_Sample_fraction == "Dissolved") | Has_dissolved == 0) %>%
  select(-Has_dissolved) %>%
  #mutate(Char_Name = paste0(Char_Name, "-", Simplified_Sample_fraction)) %>%
  group_by(MLocID,  SampleStartDate,Char_Name,SampleMedia, SampleSubmedia ) %>%
  summarise(result = max(IRResultNWQSunit)) %>%
  arrange(MLocID, SampleStartDate) %>%
  pivot_wider(names_from = Char_Name, values_from = result) 


 colnames(ancillary_data) <- make.names(names(ancillary_data), unique = TRUE, allow_ = TRUE)
 



 ancillary_data_calculated <- ancillary_data %>%
   rename(Specific_conductance = Specific.conductance) %>%
   mutate(Specific_conductance = case_when(!is.na(Specific_conductance) ~ Specific_conductance,
                                       is.na(Specific_conductance) & !is.na(Conductivity) ~ Conductivity,
                                       TRUE ~ NA_real_),
          DOC = case_when(!is.na(DOC) ~ DOC,
                           is.na(DOC) & !is.na(TOC) ~ TOC * 0.83,
                           TRUE ~ NA_real_),
          DOC_cmt = case_when(!is.na(DOC) ~ NA_character_,
                              is.na(DOC) & !is.na(TOC) ~ "Calculated DOC from TOC",
                              TRUE ~ "No organic carbon data- Used default DOC"),
          Hardness =  case_when(!is.na(Hardness) ~ Hardness,
                                 !is.na(Calcium) & !is.na(Magnesium) ~  2.497*Calcium + 4.1189*Magnesium,
                                 !is.na(Specific_conductance) ~ exp(1.06*log(Specific_conductance) - 1.26),
                                 TRUE ~ NA_real_),
          Hardness_cmt =  case_when(!is.na(Hardness) ~ NA_character_,
                                    !is.na(Calcium) & !is.na(Magnesium) ~ "Hardness based on Ca and Mg",
                                    !is.na(Specific_conductance) ~ "Hardness based on sp conductivity",
                                    TRUE ~ "No hardness value")
   )


Al_fractions <- Results_import_no_NAs %>%
  mutate(Simplified_Sample_fraction = case_when(Sample_Fraction %in% c("Total", "Extractable",
                                                                    "Total Recoverable","Total Residual",
                                                                    "None", "volatile", "Semivolatile",
                                                                    "Acid Soluble", "Suspended")  ~ 'Total',
                                                Sample_Fraction == "Dissolved"  |
                                                      Sample_Fraction == "Filtered, field"  |
                                                      Sample_Fraction == "Filtered, lab"  ~ "Dissolved", 
                                                Sample_Fraction == "Bioavailable" ~ "Bioavailable",
                                                TRUE ~ "ERROR")) %>%
  group_by(MLocID, SampleStartDate) %>%
  mutate(has_bioavailable = case_when(any(Simplified_Sample_fraction == "Bioavailable", na.rm = TRUE) ~ 1,
                                      TRUE ~ 0),
         has_total = case_when(any(Simplified_Sample_fraction == "Total", na.rm = TRUE) ~ 1,
                                       TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(keep = case_when(has_bioavailable == 1 & Simplified_Sample_fraction == "Bioavailable" ~ 1,
                          has_bioavailable == 0 & has_total == 1 & Simplified_Sample_fraction == "Total" ~ 1,
                          has_bioavailable == 0 & has_total == 0 & Simplified_Sample_fraction == "Dissolved" ~ 1,
                          TRUE ~ 0)) %>%
  arrange(MLocID, SampleStartDate) %>%
  filter(keep == 1) %>%
  select(-keep, -has_bioavailable, -has_total)

al_ancillary_combined <- Al_fractions %>%
  left_join(ancillary_data_calculated, by = c("MLocID", "SampleMedia", "SampleSubmedia", "SampleStartDate"))

default_DOC <- al_ancillary_combined %>%
  ungroup() %>%
  filter(is.na(DOC)) %>%
  select(MLocID, Lat_DD, Long_DD) %>%
  distinct(MLocID, .keep_all = TRUE) %>%
  rowwise() %>%
  mutate(def_DOC = deqalcrit::al_default_DOC(Lat_DD, Long_DD)) %>%
  select(MLocID, def_DOC) %>%
  ungroup() 

al_ancillary_combined_2 <- al_ancillary_combined %>%
  ungroup() %>%
  left_join(default_DOC, by = "MLocID") %>%
  mutate(DOC = case_when(!is.na(DOC) ~ DOC,
                         TRUE ~ def_DOC),
         al_ancillary_cmt = case_when(!is.na(DOC_cmt) & !is.na(Hardness_cmt) ~ str_c(DOC_cmt, Hardness_cmt,  sep = "; " ),
                                      !is.na(DOC_cmt) & is.na(Hardness_cmt) ~ str_c(DOC_cmt, sep = "; " ),
                                      is.na(DOC_cmt) & !is.na(Hardness_cmt) ~ str_c(Hardness_cmt, sep = "; " ),
                                      TRUE ~ NA_character_))


  
print("Beginning AL criteria calculations")
Al_criteria <- deqalcrit::al_crit_calculator(al_ancillary_combined_2,
                                  ph_col = "pH", hardness_col = "Hardness", DOC_col = "DOC", verbose = FALSE)

al_criteria_excursions <- Al_criteria %>%
  mutate(default_crit = case_when(stringr::str_detect(Flag, 'Default Criteria Used') ~ "Yes",
                                  TRUE ~ "No"),
         excursion = case_when(IRResultNWQSunit > Final_CCC ~ 1,
                               IRResultNWQSunit <= Final_CCC ~ 0,
                               TRUE ~ NA_real_))

# this is the data file -------------------------------------------------------------------------------------------



# Categorization --------------------------------------------------------------------------------------------------

AL_tox_aluminum_assess_fun <- function(df_data = al_criteria_excursions, AU_type){

# Testing ---------------------------------------------------------------------------------------------------------
# df_data = al_criteria_excursions
# 
# AU_type = 'other'


# 
if(AU_type == "other"){  
  group1 <- c('AU_ID', 'GNIS_Name', 'OWRD_Basin', 'Pollu_ID', 'wqstd_code', 'Char_Name' )
  
  
  inverse <- TRUE
  
  
} else if (AU_type == "WS"){
  group1 <- c('AU_ID', 'MLocID', 'AU_GNIS_Name', 'Pollu_ID', 'wqstd_code', 'Char_Name' )
  
  
  inverse <- FALSE
}


Results_tox_AL_aluminum_cats <- df_data %>%
  filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
  group_by_at(group1) %>%
  #Summarise data
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            num_samples = n(),
            percent_3d = round(sum(Result_Operator == "<" & IRResultNWQSunit > Final_CCC )/num_samples * 100),
            num_fraction_types = n_distinct(Simplified_Sample_fraction),
            num_samples_bioavailable_fraction =sum(Simplified_Sample_fraction == "Bioavailable"), 
            num_samples_bioavailable_calc_crit = sum(Simplified_Sample_fraction == "Bioavailable" & default_crit == "No"),
            num_samples_bioavailable_default_crit = sum(Simplified_Sample_fraction == "Bioavailable" & default_crit == "Yes"),
            num_samples_total_fraction = sum(Simplified_Sample_fraction == "Total"),
            num_Samples_dissolved_fraction = sum(Simplified_Sample_fraction == "Dissolved"),
            num_excursions_all = sum(excursion),
            num_excursions_bioavailable_fraction = sum(excursion[Simplified_Sample_fraction == "Bioavailable"]),
            num_excursions_bioavail_calc_crit =  sum(excursion[Simplified_Sample_fraction == "Bioavailable" & default_crit == "No"]),
            num_excursions_bioavail_default_crit= sum(excursion[Simplified_Sample_fraction == "Bioavailable" & default_crit == "Yes"]),
            num_excursions_total_fraction = sum(excursion[Simplified_Sample_fraction == "Total"]),
            num_excursions_dissolved_fraction = sum(excursion[Simplified_Sample_fraction == "Dissolved"]),
            critical_excursions_bioavail = binomial_excursions(num_samples_bioavailable_fraction, type = "Toxics"),
            num_samples_crit_excursion_3B = num_samples_total_fraction + num_excursions_dissolved_fraction,
            critical_excursions_3B = binomial_excursions(num_samples_crit_excursion_3B, type = "Toxics")) %>%
  # Assign categories
  mutate(IR_category = case_when(percent_3d == 100 ~ "3D",
                                 
                                 num_samples_bioavailable_calc_crit > 2 
                                    & num_excursions_bioavail_calc_crit > binomial_excursions(num_samples_bioavailable_calc_crit, type = "Toxics") ~ '5',
                                 
                                 num_samples_bioavailable_default_crit > 2 
                                    & num_excursions_bioavail_default_crit > binomial_excursions(num_samples_bioavailable_default_crit, type = "Toxics") ~ '3B',
                                 
                                 num_samples_bioavailable_fraction >= 10   
                                    & num_excursions_bioavailable_fraction < critical_excursions_bioavail ~ '2',
                                 
                                 num_samples_bioavailable_fraction < 10 &  num_samples_bioavailable_fraction > 0 & num_samples_total_fraction > 1
                                    & (num_excursions_bioavailable_fraction + num_excursions_total_fraction) >  binomial_excursions((num_excursions_bioavailable_fraction + num_excursions_total_fraction), type = "Toxics") ~ '3B',
                                 
                                 num_samples_bioavailable_fraction < 10 &  num_samples_bioavailable_fraction > 0 & num_samples_total_fraction > 1
                                 & (num_excursions_bioavailable_fraction + num_excursions_total_fraction) <=  binomial_excursions((num_excursions_bioavailable_fraction + num_excursions_total_fraction), type = "Toxics") ~ '3',
                                 
                                 num_samples_bioavailable_fraction == 0 & num_samples_total_fraction > 2
                                    & num_excursions_total_fraction > binomial_excursions(num_samples_total_fraction, type = "Toxics") ~ '3B',
                                 
                                 .default = '3'  
                                 ),
         Rationale =  case_when(percent_3d == 100 ~ paste0("Insufficient data: All results are non-detects with detection limits above criteria- ", num_samples, " total samples"),
                                
                                num_samples_bioavailable_calc_crit > 2 
                                & num_excursions_bioavail_calc_crit > binomial_excursions(num_samples_bioavailable_calc_crit, type = "Toxics") ~ paste0("Impaired: ", num_excursions_bioavail_calc_crit, 
                                                                                                                                                        " excursions of calculated criteria in bioavailable fraction above critical excursion value of ",
                                                                                                                                                        binomial_excursions(num_samples_bioavailable_calc_crit, type = "Toxics"), ". ",num_samples_bioavailable_fraction, " bioavailable samples." ),
                                num_samples_bioavailable_default_crit > 2 
                                    & num_excursions_bioavail_default_crit > binomial_excursions(num_samples_bioavailable_default_crit, type = "Toxics") ~ paste0('Insufficient data- Potential Concern. ', 
                                                                                                                                                                  num_excursions_bioavail_default_crit, " excursions of regional default criteria. ",
                                                                                                                                                                  num_samples_bioavailable_fraction, " samples of bioavailable fraction. ",
                                                                                                                                                                  num_samples_bioavailable_default_crit, " samples used default criteria."),
                                 
                                 num_samples_bioavailable_fraction >= 10   
                                    & num_excursions_bioavailable_fraction < critical_excursions_bioavail ~ paste0('Attaining. ',
                                                                                                                   num_samples_bioavailable_fraction, 
                                                                                                                   " samples of the bioavailable fraction. ",
                                                                                                                   num_excursions_bioavailable_fraction, 
                                                                                                                   " excursions of criteria. ",
                                                                                                                   critical_excursions_bioavail, 
                                                                                                                   " needed to list."),
                                 
                                 num_samples_bioavailable_fraction < 10 &  num_samples_bioavailable_fraction > 0 & num_samples_total_fraction > 1
                                    & (num_excursions_bioavailable_fraction + num_excursions_total_fraction) >  binomial_excursions((num_excursions_bioavailable_fraction + num_excursions_total_fraction), type = "Toxics") ~ paste0('Insuffcient Data: Mixed bioavailable and total recoverable samples.',
                                                                                                                                                                                                                                      num_samples_bioavailable_fraction, ' samples in the bioavailable fraction, ',
                                                                                                                                                                                                                                      num_samples_total_fraction, ' samples in the total fraction.',
                                                                                                                                                                                                                                      (num_excursions_bioavailable_fraction + num_excursions_total_fraction), 
                                                                                                                                                                                                                                      " total excursions (",
                                                                                                                                                                                                                                      num_excursions_bioavailable_fraction, 
                                                                                                                                                                                                                                      ' bioavilable excursions, ',
                                                                                                                                                                                                                                      num_excursions_total_fraction, 
                                                                                                                                                                                                                                      ' total fraction excurions'
                                                                                                                                                                                                                                      ),
                                 
                                 num_samples_bioavailable_fraction < 10 &  num_samples_bioavailable_fraction > 0 & num_samples_total_fraction > 1
                                 & (num_excursions_bioavailable_fraction + num_excursions_total_fraction) <=  binomial_excursions((num_excursions_bioavailable_fraction + num_excursions_total_fraction), type = "Toxics") ~ paste0('Insuffcient Data: Mixed bioavailable and total recoverable samples.',
                                                                                                                                                                                                                                    num_samples_bioavailable_fraction, ' samples in the bioavailable fraction, ',
                                                                                                                                                                                                                                    num_samples_total_fraction, ' samples in the total fraction.',
                                                                                                                                                                                                                                    (num_excursions_bioavailable_fraction + num_excursions_total_fraction), 
                                                                                                                                                                                                                                    " total excursions (",
                                                                                                                                                                                                                                    num_excursions_bioavailable_fraction, 
                                                                                                                                                                                                                                    ' bioavilable excursions, ',
                                                                                                                                                                                                                                    num_excursions_total_fraction, 
                                                                                                                                                                                                                                    ' total fraction excurions'
                                 ),
                                 
                                 num_samples_bioavailable_fraction == 0 & num_samples_total_fraction > 2
                                    & num_excursions_total_fraction > binomial_excursions(num_samples_total_fraction, type = "Toxics") ~ paste0('Insufficient Data. No bioavailavble fraction available. ',
                                                                                                                                                num_excursions_total_fraction ,' excursions of total fraction results are less than number needed for Cat3B (',
                                                                                                                                                binomial_excursions(num_samples_total_fraction, type = "Toxics"), 
                                                                                                                                                'needed to list).'),
                                 
                                 .default = paste0("Insufficient data: ", 
                                              num_excursions_bioavailable_fraction + num_excursions_total_fraction + num_Samples_dissolved_fraction,
                                              " total excursions of criteria. ",
                                              num_excursions_bioavailable_fraction, 
                                              " excursions of ",
                                              num_samples_bioavailable_fraction, " bioavailable samples, ",
                                              num_excursions_total_fraction,
                                              " excursions of ",
                                              num_samples_total_fraction, 
                                              " total fraction samples, ",
                                              num_excursions_dissolved_fraction,
                                              " excursions of ",
                                              num_Samples_dissolved_fraction,
                                              " dissolved fraction samples. ",
                                              num_samples, 
                                              " total samples.")
                                )) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code)) |> 
  mutate(period = NA_character_) |> 
  mutate(Delist_eligability = case_when(num_samples_bioavailable_fraction >= 18 & num_excursions_bioavailable_fraction <= binomial_delisting(num_samples_bioavailable_fraction, 'Toxics')  ~ 1,
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

AL_Tox_AL_WS <- AL_tox_aluminum_assess_fun(df_data = al_criteria_excursions, AU_type = "WS")

## GNIS rollup -----------------------------------------------------------------------------------------------------


WS_GNIS_rollup <- AL_Tox_AL_WS %>%
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

AL_Tox_AL_other <-AL_tox_aluminum_assess_fun(df_data = al_criteria_excursions, AU_type = "other")


other_category <- join_prev_assessments(AL_Tox_AL_other, AU_type = 'Other')|> 
  select(-Char_Name) |> 
  left_join(Char_rename) |> 
  relocate(Char_Name, .after = AU_ID)

other_category_delist <-  assess_delist(other_category, type = "Other")  

# prep data for export --------------------------------------------------------------------------------------------



# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  ungroup() |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale,stations, recordID, status_change, Year_listed,  year_last_assessed)

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



# Export ----------------------------------------------------------------------------------------------------------

Results_tox_Al_AL <- list(data = al_criteria_excursions,
                            AU_Decisions = AU_display,
                            Other_AU_categorization = other_category_delist,
                            WS_Station_cat = AL_Tox_AL_WS,
                            WS_GNIS_cat = WS_GNIS_rollup_delist)




return(Results_tox_Al_AL)

}
