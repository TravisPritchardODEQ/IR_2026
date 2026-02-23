
# Biocriteria updater -----------------------------------------------------

library(openxlsx)
library(janitor)
workbook_url <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Code outputs/Draft Outputs/biocriteria2025-10-24.xlsx"
getSheetNames(workbook_url)



AU_Decisions_import <- read.xlsx(workbook_url,
          sheet = 'Other_AU_categorization',
          check.names=TRUE)

GNIS_Decisions_import <- read.xlsx(workbook_url,
                                 sheet = "WS GNIS categorization",
                                 check.names=TRUE)




# Update the columns ------------------------------------------------------

other_decisions_update <- AU_Decisions_import |> 
  rename(Updated_Rationale = Updated_Rationale. ) |> 
  mutate(IR_category_26 = case_when(!is.na(Updated_Category) ~ Updated_Category,
                                    TRUE ~ IR_category_26),
         Rationale = case_when(!is.na(Updated_Rationale ) ~ Updated_Rationale,
                             TRUE ~ Rationale),
         final_AU_cat = case_when(!is.na(Updated_Category) ~ Updated_Category,
                                  TRUE ~ final_AU_cat)) |> 
  select(-LAM.comments, - Review.comment, -Review.code, -SLH.comments, -SLH.review.code, -Updated_Category, -Updated_Rationale   )


GNIS_decisions_update <- GNIS_Decisions_import |> 
  rename(Updated_Rationale = Updated_Rationale. ) |>
  mutate(IR_category_GNIS_26 = case_when(!is.na(Updated_Category) ~ Updated_Category,
                                          TRUE ~ IR_category_GNIS_26),
         Rationale_GNIS = case_when(!is.na(Updated_Rationale) ~ Updated_Rationale,
                                    TRUE ~ Rationale_GNIS),
         final_GNIS_cat = case_when(!is.na(Updated_Category) ~ Updated_Category,
                                   TRUE ~ final_GNIS_cat)) |> 
  select(-LAM.comments, -Review.comment, -Review.code,  -SLH.comments, -SLH.review.code, 
         -Updated_Category, -Updated_Rationale)
  


# Function to rollup GNIS -------------------------------------------------




  
  WS_AU_rollup <- GNIS_decisions_update |>
    ungroup() %>%
    mutate(Rationale_GNIS = case_when(!is.na(Rationale_GNIS) ~ paste0(AU_GNIS_Name, ": ",Rationale_GNIS ),
                                      TRUE ~ Rationale_GNIS)) |>
    group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code, period,prev_AU_category,prev_AU_rationale) %>%
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

AU_display_ws <- WS_AU_rollup |> 
  mutate(assessed_cat = IR_category_AU_26) |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)



# AU_summ -----------------------------------------------------------------





AU_display_other <- other_decisions_update |> 
  select(AU_ID, Char_Name, Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, status_change, IR_category_26, Year_listed,  year_last_assessed) |> 
  rename(assessed_cat = IR_category_26)


AU_display <- bind_rows(AU_display_other, AU_display_ws)  |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale)) |> 
  mutate(period = as.character(period)) |> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                 TRUE ~  Year_listed)) |> 
  mutate(status_change = case_when(final_AU_cat == '4A' & prev_category == '4A' & year_last_assessed == '2026' ~ 'No change in status- Assessed',
                                   final_AU_cat == '4A' & prev_category == '4A' & year_last_assessed != '2026' ~ 'No change in status- No new assessment',
                                   TRUE ~ status_change))



# Update workbook ---------------------------------------------------------


# To update: 
#   Other_AU_categorization = other_decisions_update
#   WS GNIS categorization = GNIS_decisions_update
#   AU_Decisions = AU_display




# Load existing unupdated worksheets ------------------------------------------------



AU_Decisions_import <- read.xlsx(workbook_url,
                                 sheet = 'Other_AU_categorization',
                                 check.names=TRUE)


biocriteria_wide <- read.xlsx(workbook_url,
                              sheet = 'Biocriteria Raw Data')


biocriteria_WS <- read.xlsx(workbook_url,
                            sheet = "Biocriteria WS Data")


biocriteria_AU <- read.xlsx(workbook_url,
                            sheet = "Biocriteria WS Data")


# Create new workbook -----------------------------------------------------

wb <- createWorkbook()


addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')

addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')

addWorksheet(wb, sheetName = "Biocriteria Raw Data", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Biocriteria WS Data", tabColour = 'paleturquoise2')
addWorksheet(wb, sheetName = "Biocriteria other Data", tabColour = 'paleturquoise2')


header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)

writeData(wb = wb, sheet = "Other_AU_categorization", x = other_decisions_update, headerStyle = header_st)
#writeData(wb = wb, sheet = "WS station categorization", x = WS_category, headerStyle = header_st)
writeData(wb = wb, sheet = "WS GNIS categorization", x = GNIS_decisions_update, headerStyle = header_st)

writeData(wb = wb, sheet = "Biocriteria Raw Data", x = biocriteria_wide, headerStyle = header_st)
writeData(wb = wb, sheet = "Biocriteria WS Data", x = biocriteria_WS, headerStyle = header_st )
writeData(wb = wb, sheet = "Biocriteria other Data", x = biocriteria_AU, headerStyle = header_st )


print("Writing excel doc")
saveWorkbook(wb, paste0("Parameters/Outputs/biocriteria",Sys.Date(), ".xlsx"), overwrite = TRUE) 


