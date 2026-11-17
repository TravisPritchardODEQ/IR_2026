library(tidyverse)
library(openxlsx)


# ATTAINS Templates -----------------------------------------------------------------------------------------------

## Setup -----------------------------------------------------------------------------------------------------------


Attains_import <- list()
AU_all_rollup_file <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/IR_2026_State_Final_Rollup-WORKING COPY.xlsx"



AU_decisions <-read.xlsx(AU_all_rollup_file,
                         sheet = 'AU_decisions') |> 
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


# Assessments -----------------------------------------------------------------------------------------------------

###only do new assessments or where things have changed


AU_decisions_diffs <- AU_decisions |> 
  filter(!(status_change == 'No change in status- No new assessment' & 
           final_AU_cat == prev_category) | is.na(prev_category))

assessments <- AU_decisions_diffs |> 
  mutate(ASSESSMENT_UNIT_ID = AU_ID, .keep = "none") |> 
  distinct() |> 
  mutate(AGENCY_CODE = 'S',
         CYCLE_LAST_ASSESSED = '2026',
         YEAR_LAST_MONITORED = NA_character_,
         ASSESSMENT_UNIT_STATE_IR_CAT = NA_character_,
         ASSESSMENT_COMMENT = NA_character_,
         ASSESSMENT_RATIONALE = NA_character_,
         TROPHIC_STATUS = NA_character_
         
  )
  
Attains_import$assessments <- assessments




# Use Tab-----------------------------------------------------------------------------------------------------------------

AU_BU <- read.xlsx(AU_all_rollup_file,
                   sheet = "AU_BU") 


AU_BU_2024 <- AU_BU %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, ben_use) %>%
  filter(final_AU_cat == max(final_AU_cat)) %>%
  filter(row_number()==1) #%>%
  #filter(AU_ID %in% assessments$ASSESSMENT_UNIT_ID) 

Uses <- AU_BU_2024 %>%
  mutate(AU_parameter_category = factor(final_AU_cat, 
                                        levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                        ordered=TRUE)) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             ben_use == 'fishing' ~ 'fishing',
                             ben_use == 'aesthetic quality' ~ "aesthetic quality",
                             ben_use == 'water contact recreation' ~ 'water contact recreation',
                             TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use)) %>%
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name),
         pollutant_strd_cat = paste0(AU_parameter_category, ": ", pollutant_strd)) %>%
  group_by(AU_ID, ben_use) %>%
  summarise(Category = max(AU_parameter_category),
            USE_COMMENT = str_c(pollutant_strd_cat, sep = ";", collapse = "; " )) %>%
  ungroup() %>%
  mutate(USE_COMMENT = str_sub(USE_COMMENT,1,3996),
         USE_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'N',
                                         Category %in% c('2') ~ 'F',
                                         str_detect(Category, '3') ~ 'I' ,
                                         Category == 'Unassessed' ~ 'I')) %>%
  select(AU_ID, ben_use, USE_ATTAINMENT_CODE, Category, USE_COMMENT) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         USE_NAME = ben_use) %>%
  mutate(USE_AGENCY_CODE = 'S',
         USE_TREND = NA_character_,
         USE_THREATENED = NA_character_,
         USE_ASMT_BASIS = NA_character_,
         USE_MONITORING_START = NA_character_,
         USE_MONITORING_END= NA_character_,
         USE_ASMT_DATE = NA_character_,
         USE_ASSESSOR_NAME = NA_character_,
         USE_COMMENT = USE_COMMENT,
         USE_STATE_IR_CAT = case_when(Category ==  'Unassessed' ~ "3",
                                      TRUE ~ Category) ,
         USE_ORG_QUALIFIER_FLAG = NA_character_ ) %>%
  select(ASSESSMENT_UNIT_ID,USE_NAME, USE_ATTAINMENT_CODE, USE_AGENCY_CODE, USE_TREND, USE_THREATENED,USE_ASMT_BASIS,
         USE_MONITORING_START, USE_MONITORING_END, USE_ASMT_DATE, USE_ASSESSOR_NAME, USE_COMMENT,
         USE_STATE_IR_CAT, USE_ORG_QUALIFIER_FLAG)

Attains_import$uses <- Uses



# Parameters tab --------------------------------------------------------------------------------------------------
## Get attains pollutant name --------------------------------------------------------------------------------------

library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


Attains_polluname_LU <- DBI::dbGetQuery(con, "SELECT cast([Pollu_ID] AS varchar) as Pollu_ID
                                        ,[Attains_PolluName]
                                        ,[Attains_Group] 
                                        FROM [IntegratedReport].[dbo].[LU_Pollutant]
                                        "
) 




delistings <- read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report 🎉 - IR 2024/Final List/DEQ_IR2024_Delistings.xlsx",
                        sheet = "Delistings - Remove from 303(d)") %>%
  mutate(AU_ID, Pollu_ID, wqstd_code, period,  
         Attains_code=  ATTAINS_CODE , ATTAINS_delist_reason = ATTAINS_CODE, PARAM_DELISTING_COMMENT = Rationale,
         .keep = "none")

delist_dups <- delistings %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period) %>%
  summarise(n = n()) %>%
  filter(n > 1)

if(nrow(delist_dups) > 0 ) {
  stop('delising dups')
}


parameters0 <- AU_BU_2024 %>%
  left_join(Attains_polluname_LU) %>%
  left_join(delistings) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                              ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                              ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                              ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                              ben_use == "Water Contact Recreation" ~ "water contact recreation",
                              ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                              ben_use == "Livestock Watering" ~ "livestock watering",
                              ben_use == "Boating" ~ "boating",
                              ben_use == 'fishing' ~ 'fishing',
                              ben_use == 'aesthetic quality' ~ "aesthetic quality",
                              ben_use == 'water contact recreation' ~ 'water contact recreation',
                              TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use))

Parameters <- parameters0 %>%
  mutate(AU_parameter_category = factor(final_AU_cat, 
                                        levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                        ordered=TRUE)) %>%
  group_by(AU_ID, Attains_PolluName) %>%
  mutate(PARAM_STATUS_NAME =case_when(str_detect(max(AU_parameter_category), '5') | str_detect(max(AU_parameter_category), '4') ~ 'Cause',
                                      max(AU_parameter_category) %in% c('2') ~ 'Meeting Criteria',
                                      str_detect(max(AU_parameter_category), '3') ~ 'Insufficient Information',
                                      AU_parameter_category == "Unassessed" ~ 'Removed',
  ),
  PARAM_ATTAINMENT_CODE = case_when(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4') ~ 'Not meeting criteria',
                                    AU_parameter_category %in% c('2') ~ 'Meeting Criteria',
                                    str_detect(AU_parameter_category, '3') ~ 'Not enough information',
                                    AU_parameter_category == "Unassessed" ~ 'Not enough information'),
  PARAM_TREND = NA_character_,
  PARAM_COMMENT = Rationale,
  PARAM_AGENCY_CODE = "S",
  PARAM_POLLUTANT_INDICATOR = case_when(PARAM_STATUS_NAME == 'Cause' & Attains_PolluName %in% c('HABITAT ALTERATIONS', 'FLOW REGIME MODIFICATION', 'BENTHIC MACROINVERTEBRATES BIOASSESSMENTS') ~ 'N',
                                        PARAM_STATUS_NAME == 'Cause' ~ 'Y',
                                        TRUE ~ NA_character_),
  PARAM_YEAR_LISTED = case_when(PARAM_STATUS_NAME == 'Cause' ~ Year_listed,
                                TRUE ~ NA_character_),
  PARAM_TARGET_TMDL_DATE = TMDL_issue_year,
  PARAM_EXPECTED_TO_ATTAIN = NA_character_,
  PARAM_PRIORITY_RANKING = case_when(str_detect(AU_parameter_category, '4') & is.na(TMDL_Priority) ~ 'Low',
                                     TRUE ~ TMDL_Priority),
  PARAM_CONSENT_DECREE_CYCLE = NA_character_,
  PARAM_ALT_LISTING_ID = NA_character_,
  PARAM_STATE_IR_CAT = case_when(AU_parameter_category ==  'Unassessed' ~ '-',
                                 TRUE ~ AU_parameter_category),
  PARAM_ORG_QUALIFIER_FLAG = NA_character_,
  PARAM_DELISTING_REASON = Attains_code,
  PARAM_DELISTING_COMMENT = PARAM_DELISTING_COMMENT,
  PARAM_DELISTING_AGENCY = ifelse(!is.na(PARAM_DELISTING_REASON), 'S', NA_character_ )
  
  ) %>%
  select(AU_ID, Attains_PolluName, PARAM_USE_NAME = ben_use, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE,PARAM_TREND,
         PARAM_COMMENT, PARAM_AGENCY_CODE, PARAM_POLLUTANT_INDICATOR,  PARAM_YEAR_LISTED, PARAM_TARGET_TMDL_DATE,
         PARAM_EXPECTED_TO_ATTAIN, PARAM_PRIORITY_RANKING, PARAM_CONSENT_DECREE_CYCLE,PARAM_ALT_LISTING_ID, 
         PARAM_STATE_IR_CAT, PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, PARAM_DELISTING_COMMENT, 
         PARAM_DELISTING_AGENCY) |> 
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         PARAM_NAME = Attains_PolluName) |> 
  filter(!(ASSESSMENT_UNIT_ID == 'OR_SR_1709001202_88_104175' &  PARAM_USE_NAME == 'fish and aquatic life - spawning'))

Attains_import$parameters <- Parameters


# Seasons ---------------------------------------------------------------------------------------------------------


seasons_2022 <- read.csv("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/ATTAINS/2022_download/ALL_ASMT_OREGONDEQ_2022/seasons_old.csv") %>%
  rename(SEASON_START_22 = SEASON_START,
         SEASON_END_22 = SEASON_END) |> 
  select(-PARAM_ATTAINMENT_CODE)


seasons0 <- AU_BU_2024 %>%
  filter(period == "spawn") %>%
  left_join(Attains_polluname_LU)

AUs <- unique(seasons0$AU_ID)

## query spawn dates ---------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

query_language <- "SELECT distinct [AU_ID]
      ,cast(Pollu_ID AS varchar) as Pollu_ID
      ,[SpawnCode] as temp_SpawnCode
      ,[DO_SpawnCode]
      
  FROM [IntegratedReport].[dbo].[InputRaw]
WHERE AU_ID in   ({AUs*})"

spawn_query <- glue::glue_sql(query_language, .con = con)


Spawn_codes <- DBI::dbGetQuery(con, spawn_query)
Spawn_dates <-  DBI::dbGetQuery(con, "SELECT [SpawnCode]
      ,[SpawnStart]
      ,[SpawnEnd]
  FROM [IntegratedReport].[dbo].[LU_Spawn]")



Seasons_1 <- seasons0 %>%
  ungroup() |> 
  select(AU_ID, Pollu_ID, Attains_PolluName,ben_use, final_AU_cat  ) %>%
  mutate(AU_parameter_category =  factor(final_AU_cat, 
                                         levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                         ordered=TRUE)) %>%
  left_join(Spawn_codes) %>%
  mutate(SpawnCode = case_when(Attains_PolluName == 'TEMPERATURE' ~ temp_SpawnCode,
                               Attains_PolluName == "DISSOLVED OXYGEN" ~ DO_SpawnCode)) %>%
  filter(!is.na(SpawnCode),
         SpawnCode != 0,
         SpawnCode != 99 ) %>%
  select(-temp_SpawnCode, -DO_SpawnCode) %>%
  left_join(Spawn_dates) %>%
  filter(!is.na(SpawnStart)) %>%
  mutate( SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",2024 ), SpawnStart ),
          SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", 2024), SpawnEnd ),
          SpawnStart = mdy(SpawnStart),
          SpawnEnd=mdy(SpawnEnd),
          SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                             SpawnEnd),
          SpawnStart = if_else(SpawnEnd < SpawnStart, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                               SpawnStart),
          length = SpawnEnd - SpawnStart + 1
  ) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  mutate(Category = max(AU_parameter_category)) %>%
  filter(length == max(length)) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  filter(row_number()==1) %>%
  mutate(PARAM_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'Not meeting criteria',
                                           Category %in% c('2') ~ 'Meeting Criteria',
                                           str_detect(Category, '3') ~ 'Not enough information'),
         SEASON_START = SpawnStart,
         SEASON_END = SpawnEnd) %>%
  # SEASON_START = format(SpawnStart, format="%d-%b"),
  # SEASON_END = format(SpawnEnd, format="%d-%b")) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ 'fish and aquatic life - spawning',
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  ungroup() %>%
  select(AU_ID, Attains_PolluName, ben_use,PARAM_ATTAINMENT_CODE, SEASON_START, SEASON_END ) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID, 
         PARAM_NAME = Attains_PolluName,
         PARAM_USE_NAME = ben_use)

# Seasons <- Seasons_1 %>%
#   anti_join(seasons_2020)
df$Month_Yr <- format(as.Date(df$Date), "%Y-%m")


Seasons <- Seasons_1 |> 
  mutate(SEASON_START = as.character(format(as.Date(SEASON_START), "%m/%d")),
         SEASON_END = as.character(format(as.Date(SEASON_END), "%m/%d"))) |> 
  full_join(seasons_2022) |> 
  mutate(SEASON_START = case_when(is.na(SEASON_START) ~ SEASON_START_22,
                                  TRUE ~ SEASON_START),
         SEASON_END =  case_when(is.na(SEASON_END) ~ SEASON_END_22,
                                 TRUE ~ SEASON_END)) |> 
  select(-SEASON_START_22, -SEASON_END_22)

Attains_import$seasons <- Seasons


write.xlsx(Seasons, file = 'Season_24.xlsx')



# Associated-action -----------------------------------------------------------------------------------------------




library(odeqtmdl)

actions <- odeqtmdl::tmdl_actions |> 
  filter(in_attains == TRUE) |> 
  filter(TMDL_issue_year <2025)

old_actions <- read.csv("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report 🎉 - IR 2024/ATTAINS/2022_download/ALL_ASMT_OREGONDEQ_2022/associated-actions_old.csv")


assess_Parameters <- Parameters |> 
  select(ASSESSMENT_UNIT_ID, PARAM_NAME) |> 
  distinct()


tmdl_au_1 <- odeqtmdl::tmdl_au |> 
  filter(action_id %in% actions$action_id)

Associated_actions <- tmdl_au_1 |> 
  mutate(Pollu_ID = as.character(Pollu_ID)) |> 
  left_join(Attains_polluname_LU) |> 
  select(AU_ID, Attains_PolluName, action_id) |> 
  mutate(ACTION_TYPE = 'TMDL') %>%
  distinct() |> 
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         ACTION_PARAM_NAME = Attains_PolluName,
         ACTION_ID = action_id)

Associated_Actions1 <- left_join(assess_Parameters, Associated_actions, 
                                  by = c('ASSESSMENT_UNIT_ID','PARAM_NAME' =  'ACTION_PARAM_NAME')) |> 
  filter(!is.na(ACTION_ID))
  
Associated_Actions_new <- anti_join(Associated_Actions1, old_actions) |> 
  filter(ACTION_ID %in% actions$action_id)


# Associated_Actions_replacement <- anti_join(Associated_Actions1, old_actions) |> 
#   filter(ACTION_ID %in% actions$action_id) |> 
#   filter(PARAM_NAME == "TEMPERATURE",
#          ACTION_ID %in% c("10006", "10791", "11395", "12241", "30674",
#                           "30358", "32071", "33829", "35888", "35887",
#                           "35890", "9294", "39782", "39294", "39753")) 
# 
# write.xlsx(Associated_Actions_replacement, file = "Temperature_Replacement_TMDL_Dissosociations.xlsx")

Attains_import$Associated_Actions <- Associated_Actions_new


# Write csvs -------------------------------------------------------------------------------------------------


write.csv(assessments, file = 'ATTAINS/assessments.csv', row.names = FALSE,  na = "")
write.csv(Attains_import$uses, file = 'ATTAINS/TMDL_redo/uses.csv', row.names = FALSE,  na = "")
write.csv(Attains_import$parameters, file = 'ATTAINS/TMDL_redo/parameters.csv', row.names = FALSE,  na = "")
write.csv(Attains_import$seasons, file = 'ATTAINS/seasons.csv', row.names = FALSE,  na = "")
write.csv(Attains_import$Associated_Actions, file = 'ATTAINS/Associated_Actions.csv', row.names = FALSE,  na = "")
