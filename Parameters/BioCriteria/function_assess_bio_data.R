#  

#Travis - can make a function with the rest of the code 
#fw_bio <- function(df, write_excel = TRUE){
  
  library(runner)
  library(openxlsx)
  
  
fw_bio <- function(df, write_excel = TRUE){
#df <- Results_import
  
  
stn <- df %>%
       select(MLocID,AU_GNIS_Name,GNIS_Name) %>%
       distinct()



# Watershed Units - Mloc Assessment -------------------------------------------------------------------------------




## Single Sample MWCF ----------------------------------------------------------------------------------------------



  MWCF_SS_WS <- Results_import %>%
    filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "WS", negate = FALSE))%>%
    group_by(AU_ID,MLocID) %>% 
    mutate(num_Samples = n()) %>%
    filter(num_Samples == 1) %>%
    summarise(num_Samples = n(),
              n_over_5 = sum(as.numeric(Score >= 20)),
              n_btwn_3B = sum(as.numeric(Score) >= 15 & as.numeric(Score) <= 20),
              n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
              n_less_3C = sum(as.numeric(Score) <= 8)) %>%
    mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                              n_btwn_3B >=1 ~"3B",
                              n_btwn_3C >= 1 ~ "3C",
                              n_less_3C  >= 1 ~ "2",
                              TRUE ~ "ERROR"),
           mean_all_samples = NA,
           model = "MWCF") %>% 
    select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
           "n_btwn_3C","n_less_3C",model,"IR_Cat")


## multiple Sample at the same station or GNIS MWCF  ---------------------------------------------------------------


  MWCF_AU_MS_WS = Results_import %>%
    filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "WS", negate = FALSE)) %>%
    group_by(AU_ID,MLocID) %>% 
    mutate(num_Samples = n()) %>%
    filter(num_Samples >= 2) %>%
    summarise(num_Samples = n(),
              n_over_5 = sum(as.numeric(Score) >= 15),
              n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
              n_less_3C = sum(as.numeric(Score) <= 8),
              mean_all_samples = mean(as.numeric(Score))) %>%
    mutate(IR_Cat = case_when(mean_all_samples >= 15 ~"5",
                                  mean_all_samples >= 9 & mean_all_samples <= 14 ~ "3C",
                                  mean_all_samples <= 8 ~ "2",
                                  TRUE ~ "ERROR"),
           n_btwn_3B = NA,
           model = "MWCF") %>% 
    select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
           "n_btwn_3C","n_less_3C",model,"IR_Cat")


## single Sample WCCP ----------------------------------------------------------------------------------------------



WCCP_AU_SS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>% 
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(Score >= 27),
            n_btwn_3B = sum(Score >= 22 & Score <= 26),
            n_btwn_3C = sum(Score >= 8 & Score <= 21),
            n_less_3C = sum(Score <= 7)) %>%
  mutate(IR_Cat = case_when( n_over_5 >=1 ~"5",
                             n_btwn_3B >=1 ~"3B",
                             n_btwn_3C >= 1 ~ "3C",
                             n_less_3C >= 1 ~ "2",
                            TRUE ~ "ERROR"),
  mean_all_samples = NA,
  model = "WCCP") %>% 
  select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat")


WCCP_AU_MS_WS = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            mean_all_samples = mean(as.numeric(Score)),
            n_over_5 = sum(as.numeric(mean_all_samples) >= 22),
            n_btwn_3C = sum(as.numeric(mean_all_samples) >= 8 & as.numeric(mean_all_samples) <= 21),
            n_less_3C = sum(as.numeric(mean_all_samples) <= 7))%>%
  mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                            n_btwn_3C >= 1 ~ "3C",
                            n_less_3C >= 1 ~ "2",
                            TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "WCCP") %>% 
  select("AU_ID","MLocID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat")

WS_Station_Cat <- rbind(MWCF_SS_WS,MWCF_AU_MS_WS,WCCP_AU_SS_WS,WCCP_AU_MS_WS) %>% 
                  left_join(stn, by = "MLocID") %>% 
                  mutate(Rationale = case_when(IR_Cat == "2" ~ paste("Attaining - samples from",
                                                                       MLocID,"below applicable taxa loss benchmark for",model,"model region"),
                                               IR_Cat == "5" ~ paste("Impaired - samples from",
                                                                     MLocID,"above applicable taxa loss benchmark for",model,"model region"),
                                               IR_Cat == "3B" ~ paste("Insufficient Data; Potential Concern - samples from",
                                                                    MLocID,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                                               IR_Cat == "3C" ~ paste("Insufficient Data; Non-Reference - samples from",
                                                                     MLocID,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                                               TRUE ~ "ERROR"),
                           
                         Pollu_ID = 156,
                         wqstd_code = 5,
                         Char_Name = "BioCriteria",
                          period = NA) %>% 
                    select(AU_ID, MLocID, AU_GNIS_Name, GNIS_Name,Char_Name,Pollu_ID,wqstd_code,
                           period,IR_Cat,Rationale)%>% 
                    rename(IR_category = IR_Cat)

### GNIS rollup -----------------------------------------------------------------------------------------------------

WS_GNIS_rollup <- WS_Station_Cat %>% 
  group_by(AU_ID,AU_GNIS_Name) %>% 
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            n_mlocids = n(),
            n_cat5 = sum(IR_category == "5"),
            n_cat2 = sum(IR_category == "2"),
            n_cat3C = sum(IR_category == "3C"),
            n_cat3B = sum(IR_category == "3B"),
            Rationale_GNIS = paste(Rationale, collapse = " ; ")) %>% 
  mutate(IR_category_GNIS_24 = case_when(n_cat5 >= 1 ~ "5",
                                         n_cat2 >= 1 & n_cat5 == 0  ~ "2",
                                         n_cat3B >= 1& n_cat5 == 0  & n_cat2 == 0   ~"3B", 
                                         n_cat3C >= 1& n_cat5 == 0  & n_cat2 == 0  ~"3C"),
         Pollu_ID = 156,
         wqstd_code = 5,
         Char_Name = "BioCriteria",
         period = NA) %>% 
  select(AU_ID, AU_GNIS_Name,Char_Name,Pollu_ID,wqstd_code,period,
         IR_category_GNIS_24,Rationale_GNIS, stations) |> 
  mutate(Delist_eligability = 0)|> 
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")  |> 
  mutate(Char_Name = "BioCriteria")

### Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS')|> 
  mutate(Char_Name = 'BioCriteria') |> 
  relocate(Char_Name, .after = AU_GNIS_Name)

### AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup, char_name_field = Char_Name)


WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup)


# River / Stream units --------------------------------------------------------------------------------------------

                                            
### River Stream Units ####
MWCF_SS_SR <- Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST" & str_detect(AU_ID, "SR", negate = FALSE))%>%
  group_by(AU_ID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(as.numeric(Score >= 20)),
            n_btwn_3B = sum(as.numeric(Score) >= 15 & as.numeric(Score) <= 20),
            n_btwn_3C = sum(as.numeric(Score) >= 9 & as.numeric(Score) <= 14),
            n_less_3C = sum(as.numeric(Score) <= 8),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                            n_btwn_3C >= 1 ~ "3C",
                            n_less_3C  >= 1 ~ "2",
                            TRUE ~ "ERROR"),
         mean_all_samples = NA,
         model = "MWCF") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)

### multiple Sample at the same station or GNIS MWCF 
MWCF_MS_SR = Results_import %>%
  filter(EcoRegion2 == "MARINE WEST COAST FOREST"& str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            mean_all_samples = mean(as.numeric(Score)),
            n_over_5 = sum(as.numeric(mean_all_samples) >= 15),
            n_btwn_3C = sum(as.numeric(mean_all_samples) >= 9 & as.numeric(mean_all_samples) <= 14),
            n_less_3C = sum(as.numeric(mean_all_samples) <= 8),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when(n_over_5 >=1 ~"5",
                            n_btwn_3C >= 1 ~ "3C",
                            n_less_3C  >= 1 ~ "2",
                            TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "MWCF") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)

#### single Sample WCCP
WCCP_SS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>% 
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples == 1) %>%
  summarise(num_Samples = n(),
            n_over_5 = sum(as.numeric(Score) >= 27),
            n_btwn_3B = sum(as.numeric(Score) >= 22 & as.numeric(Score) <= 26),
            n_btwn_3C = sum(as.numeric(Score) >= 8 & as.numeric(Score) <= 21),
            n_less_3C = sum(as.numeric(Score) <= 7),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when( n_over_5 >=1 ~"5",
                             n_btwn_3B >=1 ~"3B",
                             n_btwn_3C >= 1 ~ "3C",
                             n_less_3C >= 1 ~ "2",
                             TRUE ~ "ERROR"),
         mean_all_samples = NA,
         model = "WCCP",) %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)


WCCP_MS_SR = Results_import %>%
  filter(EcoRegion2 == "WESTERN CORDILLERA"|EcoRegion2 == "COLD DESERTS") %>%
  filter(str_detect(AU_ID, "SR", negate = FALSE)) %>%
  group_by(AU_ID,MLocID) %>% 
  mutate(num_Samples = n()) %>%
  filter(num_Samples >= 2) %>%
  summarise(num_Samples = n(),
            mean_all_samples = mean(as.numeric(Score)),
            n_over_5 = sum(as.numeric(mean_all_samples) >= 22),
            n_btwn_3C = sum(as.numeric(mean_all_samples) >= 8 & as.numeric(mean_all_samples) <= 21),
            n_less_3C = sum(as.numeric(mean_all_samples) <= 7),
            MLocIDs = paste(MLocID,collapse = " ; ")) %>%
  mutate(IR_Cat = case_when( n_over_5 >=1 ~"5",
                             n_btwn_3C >= 1 ~ "3C",
                             n_less_3C >= 1 ~ "2",
                             TRUE ~ "ERROR"),
         n_btwn_3B = NA,
         model = "WCCP") %>% 
  select("AU_ID","num_Samples","mean_all_samples","n_over_5","n_btwn_3B",
         "n_btwn_3C","n_less_3C",model,"IR_Cat",MLocIDs)


Other_categories <- rbind(MWCF_SS_SR,MWCF_MS_SR,WCCP_SS_SR,WCCP_MS_SR) %>% 
  mutate(Rationale = case_when(IR_Cat == "2" ~ paste("Attaining - samples from",
                                                     MLocIDs,"below applicable taxa loss benchmark for",model,"model region"),
                               IR_Cat == "5" ~ paste("Impaired - samples from",
                                                     MLocIDs,"above applicable taxa loss benchmark for",model,"model region"),
                               IR_Cat == "3B" ~ paste("Insufficient Data; Potential Concern - samples from",
                                                      MLocIDs,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                               IR_Cat == "3C" ~ paste("Insufficient Data; Non-Reference - samples from",
                                                      MLocIDs,"within the range of applicable taxa loss benchmarks for",model,"model region"),
                               TRUE ~ "ERROR"),
         
         Pollu_ID = 156,
         wqstd_code = 5,
         Char_Name = "BioCriteria",
         period = NA) %>% 
  select(AU_ID,Pollu_ID,wqstd_code,
         period,IR_Cat,Rationale,MLocIDs)%>% 
  rename(IR_category = IR_Cat) |> 
  mutate(Delist_eligability = 0)

other_category <- join_prev_assessments(Other_categories, AU_type = 'Other')

other_category_delist <-  assess_delist(other_category, type = "Other")|> 
  mutate(Char_Name = 'BioCriteria') |> 
  relocate(Char_Name, .after = AU_ID)


# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  rename(stations = MLocIDs) |> 
  mutate(Char_Name = "BioCriteria" ) |>  
  mutate(recordID = paste0("2024-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  |> 
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup_joined |> 
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
                                 TRUE ~  Year_listed))  



WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  



Data <- Results_import %>% 
        mutate(Char_Name = "BioCriteria",
               Pollu_ID = 156,
               wqstd_code = 5,
               period = NA) %>% 
        select(AU_ID,MLocID,AU_GNIS_Name,GNIS_Name,Char_Name,Pollu_ID,wqstd_code,period, 
               EcoRegion2,Index_Name,Score)



# Write xlsx ------------------------------------------------------------------------------------------------------

if(write_excel){
  
  
  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "Data", tabColour = 'paleturquoise2')

  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_Station_Cat, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
  
  writeData(wb = wb, sheet = "Data", x = Data, headerStyle = header_st )

  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/Biocriteria-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}

}
