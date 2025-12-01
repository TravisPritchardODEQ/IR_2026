


join_prev_assessments_biocriteria <- function(df, AU_type){
  
  # test dataset ----------------------------------------------------------------------------------------------------
  
  # df <- WS_GNIS_rollup
  # AU_type <- "other"
  
  if(AU_type == "WS"){
    
    df_names <- names(df)
    
    param_GNIS <- prev_list_GNIS |>
      filter(Pollu_ID %in% df$Pollu_ID,
             wqstd_code %in% df$wqstd_code,
             period %in% df$period)
    
    GNIS_join <- df |>
      ungroup() |>
      mutate(#AU_GNIS = str_c(AU_ID, AU_GNIS_Name, sep = ";"),
        Pollu_ID = as.character(Pollu_ID),
        wqstd_code = as.character(wqstd_code)) |>
      full_join(select(param_GNIS, -Pollutant), join_by(AU_ID, AU_GNIS_Name, Pollu_ID, wqstd_code, period)) |>
      mutate(IR_category_GNIS_26 = case_when(is.na(IR_category_GNIS_26) |IR_category_GNIS_26== "" ~ "Unassessed",
                                             TRUE ~ IR_category_GNIS_26)) |>
      mutate(prev_GNIS_rationale = case_when(!is.na(prev_GNIS_rationale) | prev_GNIS_rationale == "" ~ paste(prev_GNIS_rationale),
                                             !is.na(prev_GNIS_category) & (is.na(prev_GNIS_rationale) | prev_GNIS_rationale == "") ~ "See previous IR reports",
                                             TRUE ~ prev_GNIS_rationale),
             prev_GNIS_category = case_when(is.na(prev_GNIS_category) | prev_GNIS_category == ""~ "Unassessed",
                                            TRUE ~ prev_GNIS_category)) |>
      mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE),
             prev_GNIS_category = factor(prev_GNIS_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(final_GNIS_cat = case_when(IR_category_GNIS_26 == "Unassessed" ~ prev_GNIS_category,
                                        TRUE ~ IR_category_GNIS_26) ) |>
      arrange(AU_ID, AU_GNIS_Name)
    
    GNIS_join_names <- names(GNIS_join)
    
    
    
    overall_join <- GNIS_join %>%
      left_join(select(prev_list_AU, -Pollutant)) %>%
      select(all_of(GNIS_join_names), prev_category, prev_rationale) |>
      rename(prev_AU_category = prev_category,
             prev_AU_rationale = prev_rationale)
    
    
    
    
  } else {
    
    # non-watershed ---------------------------------------------------------------------------------------------------
    
    
    
    df_names <- names(df)
    
    param_AU_previous_categories <- prev_list_AU |>
      filter(Pollu_ID %in% df$Pollu_ID,
             wqstd_code %in% df$wqstd_code,
             period %in% df$period) |>
      filter(str_detect(AU_ID, "WS", negate = TRUE))
    
    
    
    overall_join <- df %>%
      mutate(Pollu_ID = as.character(Pollu_ID),
             wqstd_code = as.character(wqstd_code)) %>%
      full_join(select(param_AU_previous_categories, -Pollutant)) |>
      mutate(IR_category = case_when( is.na(IR_category) ~ "Unassessed",
                                      TRUE ~IR_category )) |>
      mutate(prev_category = case_when(is.na(prev_category) ~ "Unassessed",
                                       TRUE ~ prev_category)) |>
      mutate(IR_category = factor(IR_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE),
             prev_category = factor(prev_category, levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(final_AU_cat =case_when(IR_category == "Unassessed" ~ prev_category,
                                     TRUE ~ IR_category))
  }
  
  
  
  # if(nrow(df) != nrow(overall_join)){
  #   
  #   warning("Previous IR category join error. Input and output dataframes are not the same length.")
  # }
  
  
  return(overall_join)
}


assess_delist_biocriteria <- function(df, type = NULL){
  
  if (type == 'WS'){
    
    delist_assessment <- df |>
      mutate(Delist_eligability = case_when( prev_GNIS_category %in% c('5',  '4A', '5C') & IR_category_GNIS_26 == '2' & Delist_eligability ==1 ~ "Delist Eligible",
                                             prev_GNIS_category %in% c('5',  '4A', '5C') & IR_category_GNIS_26 == '2' & (Delist_eligability < 1 | is.na(Delist_eligability)) ~ 'Insuffcient data to delist')) |>
      mutate(final_GNIS_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
                                        TRUE ~ final_GNIS_cat),
             Rationale_GNIS = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale_GNIS),
                                        TRUE ~ Rationale_GNIS)) |>
      mutate(final_GNIS_cat = factor(final_GNIS_cat,levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(status_change = case_when(final_GNIS_cat == prev_GNIS_category & IR_category_GNIS_26 == 'Unassessed' ~ "No change in status- No new assessment",
                                       final_GNIS_cat == prev_GNIS_category ~ "No change in status- Assessed",
                                       final_GNIS_cat == '2' & prev_GNIS_category %in% c('5','4A','4B', '4C', '5C') ~ "Delist",
                                       prev_GNIS_category == 'Unassessed' ~ "New Assessment",
                                       prev_GNIS_category == '2' & final_GNIS_cat  %in% c('5','4A','4B', '4C', '5C') ~ "Attain to Impaired",
                                       prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('5','4A','4B', '4C', '5C') ~ "Insufficient to Impaired",
                                       prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in% c('2') ~ "Insufficient to Attain",
                                       prev_GNIS_category %in% c('3D') & final_GNIS_cat %in% c('3') ~ "3D to Insufficient",
                                       prev_GNIS_category %in% c('4A') & final_GNIS_cat %in% c('5', '5C') ~ "4A to Category 5",
                                       prev_GNIS_category %in% c('5','4A','4B', '4C', '5C') & final_GNIS_cat %in% c('3D','3','3B', '3C') ~ "Impaired to Insufficient",
                                       prev_GNIS_category %in% c('3D','3','3B', '3C') & final_GNIS_cat %in%  c('3D','3','3B', '3C') ~ "Insufficient to Insufficient (Different Types)",
                                       prev_GNIS_category %in% c('2') & final_GNIS_cat %in% c('3D','3','3B', '3C')  ~ "Attain to Insufficient",
                                       TRUE ~ paste0(prev_GNIS_category, ' to ', final_GNIS_cat)
         )) |>
      dplyr::relocate(final_GNIS_cat, .after = period) |>
      dplyr::relocate(Rationale_GNIS, .after = final_GNIS_cat)
  } else {
    delist_assessment <- df |>
      mutate(Delist_eligability = case_when( prev_category %in% c('5',  '4A', '5C') & IR_category == '2' & Delist_eligability ==1 ~ "Delist Eligible",
                                             prev_category %in% c('5',  '4A', '5C') & IR_category == '2' & (Delist_eligability < 1 | is.na(Delist_eligability)) ~ 'Insuffcient data to delist')) |>
      mutate(final_AU_cat = case_when(Delist_eligability == "Delist Eligible" ~ '2',
                                      TRUE ~ final_AU_cat),
             Rationale = case_when(Delist_eligability %in% c("Delist Eligible",'Insuffcient data to delist')  ~paste0(Delist_eligability, "- ", Rationale),
                                   TRUE ~ Rationale)) |>
      mutate(final_AU_cat = factor(final_AU_cat,levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |>
      mutate(status_change = case_when(final_AU_cat == prev_category & IR_category == 'Unassessed' ~ "No change in status- No new assessment",
                                       final_AU_cat == prev_category ~ "No change in status- Assessed",
                                       final_AU_cat == '2' & prev_category %in% c('5','4A','4B', '4C', '5C') ~ "Delist",
                                       prev_category == 'Unassessed' ~ "New Assessment",
                                       prev_category == '2' & final_AU_cat  %in% c('5','4A','4B', '4C', '5C') ~ "Attain to Impaired",
                                       prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('5','4A','4B', '4C', '5C') ~ "Insufficient to Impaired",
                                       prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in% c('2') ~ "Insufficient to Attain",
                                       prev_category %in% c('3D') & final_AU_cat %in% c('3') ~ "3D to Insufficient",
                                       prev_category %in% c('4A') & final_AU_cat %in% c('5', '5C') ~ "4A to Category 5",
                                       prev_category %in% c('5','4A','4B', '4C', '5C') & final_AU_cat %in% c('3D','3','3B', '3C') ~ "Impaired to Insufficient",
                                       prev_category %in% c('3D','3','3B', '3C') & final_AU_cat %in%  c('3D','3','3B', '3C') ~ "Insufficient to Insufficient (Different Types)",
                                       prev_category %in% c('2') & final_AU_cat %in% c('3D','3','3B', '3C')  ~ "Attain to Insufficient",
                                       TRUE ~ paste0(prev_category, ' to ', final_AU_cat)
      )) |>
      dplyr::relocate(final_AU_cat, .after = period) |>
      dplyr::relocate(Rationale, .after = final_AU_cat) |> 
      dplyr::mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                                                TRUE ~ Rationale),
                    Char_Name =  'BioCriteria')
    
  }
  
  return(delist_assessment)
  
}




rollup_WS_AU_biocriteria <- function(df, char_name_field){
  
  WS_AU_rollup <- df |>
    ungroup() %>%
    mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
                                      TRUE ~ Rationale_GNIS)) |>
    group_by(AU_ID, {{char_name_field}}, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
    summarise( stations =  stringr::str_c(unique(na.omit(stations)), collapse = "; "),
               IR_category_AU_26 = max(final_GNIS_cat),
               Rationale_AU = str_c(na.omit(Rationale_GNIS),collapse =  " ~ " ) ) %>%
    mutate(stations = case_when(stations == "" ~ NA_character_,
                                TRUE ~ stations),
           Rationale_AU = case_when(Rationale_AU == "" ~ NA_character_,
                                    TRUE ~ Rationale_AU)) |> 
    ungroup() |>
    # mutate(IR_category_AU_26 = case_when( str_detect(IR_category_AU_26, "3") & str_detect(prev_AU_category, "2|5") ~ prev_AU_category,
    #                                       TRUE ~ IR_category_AU_26)) |>
    mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period )) |>
    mutate(status_change = case_when(IR_category_AU_26 == prev_AU_category & is.na(Rationale_AU) ~ "No change in status- No new assessment",
                                     IR_category_AU_26 == prev_AU_category & !is.na(Rationale_AU)~  "No change in status- Assessed",
                                     IR_category_AU_26 == '2' & prev_AU_category %in% c('5','4A','4B', '4C') ~ "Delist",
                                     prev_AU_category == 'Unassessed' | is.na(prev_AU_category)  ~ "New Assessment",
                                     prev_AU_category == '2' & IR_category_AU_26  %in% c('5','4A','4B', '4C') ~ "Attain to Impaired",
                                     prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_26 %in% c('5','4A','4B', '4C') ~ "Insufficient to Impaired",
                                     prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_26 %in% c('2') ~ "Insufficient to Attain",
                                     prev_AU_category %in% c('3D') & IR_category_AU_26 %in% c('3') ~ "3D to Insufficient",
                                     prev_AU_category %in% c('4A') & IR_category_AU_26 %in% c('5', '5C') ~ "4A to Category 5",
                                     prev_AU_category %in% c('5','4A','4B', '4C', '5C') & IR_category_AU_26 %in% c('3D','3','3B', '3C') ~ "Impaired to Insufficient",
                                     prev_AU_category %in% c('3D','3','3B', '3C') & IR_category_AU_26 %in%  c('3D','3','3B', '3C') ~ "Insufficient to Insufficient (Different Types)",
                                     prev_AU_category %in% c('2') & IR_category_AU_26 %in% c('3D','3','3B', '3C')  ~ "Attain to Insufficient",
                                     TRUE ~ paste(prev_AU_category, ' to ', IR_category_AU_26 )
    ))
  
  
}
