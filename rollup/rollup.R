library(tidyverse)
library(openxlsx)
library(odeqIRtools)
library(odeqtmdl)
library(duckdb)




filepath <- 'C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Code outputs/Draft Outputs/'
#
# # Filenames -------------------------------------------------------------------------------------------------------
#
bact_coast <- 'bacteria coast contact- 2025-11-25.xlsx'
bact_fresh <- 'bacteria_freshwater_contact-2026-01-21.xlsx'
#
chl <- 'chl-a-2025-12-17.xlsx'
#
DO <- 'DO-2025-12-19.xlsx'

pH <- 'pH-2025-12-02.xlsx'
#
temp <- 'temperature-2025-12-30.xlsx'
#
tox_al <- 'Tox_AL-2026-01-07.xlsx'
#
tox_hh <- 'Tox_HH-2025-12-01.xlsx'
#
turb <- 'turbidity-2025-12-04.xlsx'

biocriteria <- 'biocriteria2026-01-20-updated_post_review.xlsx'
#
non_R <- 'non_R-2026-01-20.xlsx'

# Pull data in ----------------------------------------------------------------------------------------------------



## Bacteria --------------------------------------------------------------------------------------------------------

au_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
                           sheet = 'AU_Decisions') |>
  mutate(across(1:21, .fns = as.character))

gnis_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
                             sheet = 'WS GNIS categorization')


au_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
                           sheet = 'AU_Decisions') |>
  mutate(across(1:21, .fns = as.character))

gnis_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
                             sheet = 'WS GNIS categorization') |>
  mutate(across(1:25, .fns = as.character))

gnis_bact_fresh_entero <- read.xlsx(paste0(filepath, bact_fresh),
                             sheet = 'WS GNIS categorization_entero') |>
  mutate(across(1:25, .fns = as.character))


## Chl -------------------------------------------------------------------------------------------------------------


au_chl <- read.xlsx(paste0(filepath, chl),
                    sheet = 'AU_Decisions')|>
  mutate(across(1:21, .fns = as.character))

gnis_chl <- read.xlsx(paste0(filepath, chl),
                      sheet = 'WS GNIS categorization')|>
  mutate(across(1:24, .fns = as.character))


## DO --------------------------------------------------------------------------------------------------------------

au_do <- read.xlsx(paste0(filepath, DO),
                   sheet = 'AU_Decisions') |>
  mutate(across(1:21, .fns = as.character))

gnis_do <- read.xlsx(paste0(filepath, DO),
                     sheet = 'WS GNIS categorization')|>
  mutate(across(1:24, .fns = as.character))


## pH --------------------------------------------------------------------------------------------------------------

au_pH <- read.xlsx(paste0(filepath, pH),
                   sheet = 'AU_Decisions')|>
  mutate(across(1:21, .fns = as.character))

gnis_pH <- read.xlsx(paste0(filepath, pH),
                     sheet = 'WS GNIS categorization') |>
  mutate(across(1:24, .fns = as.character))


## temperature -----------------------------------------------------------------------------------------------------


au_temp <- read.xlsx(paste0(filepath, temp),
                     sheet = 'AU_Decisions')|>
  mutate(Char_Name = 'Temperature, water') |>
  mutate(across(1:21, .fns = as.character))

gnis_temp <- read.xlsx(paste0(filepath, temp),
                       sheet = 'WS_GNIS_categorization')  |>
  mutate(Char_Name = 'Temperature, water') |>
  mutate(across(1:24, .fns = as.character))

## tox AL -----------------------------------------------------------------------------------------------------


au_tox_al <- read.xlsx(paste0(filepath, tox_al),
                       sheet = 'AU_Decisions') |>
  mutate(across(1:21, .fns = as.character))

gnis_tox_al <- read.xlsx(paste0(filepath, tox_al),
                         sheet = 'GNIS_cat')  |>
  mutate(across(1:25, .fns = as.character))

## tox HH -----------------------------------------------------------------------------------------------------


au_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
                       sheet = 'AU_Decisions')|>
  mutate(across(1:21, .fns = as.character))

gnis_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
                         sheet = 'WS GNIS categorization') |>
  mutate(across(1:24, .fns = as.character))



## turbidity -----------------------------------------------------------------------------------------------------


au_turb <- read.xlsx(paste0(filepath, turb),
                     sheet = 'AU_Decisions') |>
  mutate(across(1:21, .fns = as.character))

gnis_turb <- read.xlsx(paste0(filepath, turb),
                       sheet = 'WS GNIS categorization') |>
  mutate(across(1:16, .fns = as.character))




# Biocriteria -----------------------------------------------------------------------------------------------------

au_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
                            sheet = 'AU_Decisions')|>
  mutate(across(1:22, .fns = as.character))

gnis_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
                              sheet = 'WS GNIS categorization') |>
  mutate(across(1:25, .fns = as.character))


# # non_R -----------------------------------------------------------------------------------------------------------
au_nonR <- read.xlsx(paste0(filepath, non_R),
                     sheet = 'AU_Decisions')|>
  mutate(across(1:21, .fns = as.character))

gnis_nonR <- read.xlsx(paste0(filepath, non_R),
                       sheet = 'WS GNIS categorization') |>
  mutate_all(.funs = as.character)



# Put all together ------------------------------------------------------------------------------------------------


AU_decisions <- bind_rows(au_bact_coast, au_bact_fresh, au_chl, au_do, au_pH, au_temp, au_tox_al, 
                          au_tox_hh, au_turb,au_biocriteria, au_nonR) 


# Get unassessed pollutants to move forward -----------------------------------------------------------------------
assessed_polluids <- AU_decisions$Pollu_ID 



unassessed_params <- odeqIRtools::prev_list_AU |> 
  filter(!Pollu_ID %in% assessed_polluids) |>
  filter(!is.na(Pollu_ID)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_AU_cat = prev_category,
         Rationale = prev_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'AU')

AU_decisions_joined <- AU_decisions |> 
  bind_rows(unassessed_params)


# Missing GNIS ----------------------------------------------------------------------------------------------------

antijoin <- odeqIRtools::prev_list_AU |> 
  anti_join(AU_decisions_joined, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_AU_cat = prev_category,
         Rationale = prev_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'AU')


AU_decisions_joined <- AU_decisions_joined |> 
  bind_rows(antijoin) 



# pollutant rename ------------------------------------------------------------------------------------------------
#open connection to database
con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
      ,[Pollutant_DEQ WQS] as Char_Name
  FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)

# Send query to database and return with the data
Char_rename <-  DBI::dbGetQuery(con, db_qry)

Char_rename <- Char_rename |> 
  mutate(Pollu_ID = as.character(Pollu_ID))



AU_decisions <- AU_decisions_joined |> 
  mutate(HUC12 = as.character(HUC12)) |> 
  select(-Char_Name) |> 
  left_join(Char_rename) |> 
  relocate(Char_Name, .after = AU_ID)|> 
  select(-AU_Name, -AU_UseCode, -HUC12) |> 
  join_AU_info() |> 
  mutate(HUC12 = as.character(HUC12)) |> 
  join_hucs() |> 
  arrange(AU_ID, Char_Name)

AU_decisions <- AU_decisions |> 
  group_by(AU_ID, Char_Name, Pollu_ID, wqstd_code, period) |> 
  filter(row_number() == 1)


# Get assessment labels -------------------------------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


wqstd_info <- tbl(con, "LU_Wqstd_Code") |> 
  collect() |> 
  mutate(wqstd_code = as.character(wqstd_code) ) |> 
  rename('Assessment' = 'wqstd')

DBI::dbDisconnect(con)

AU_decisions <- AU_decisions |> 
  left_join(wqstd_info) |> 
  relocate(Assessment, .after = Char_Name)



# fecal delistings ------------------------------------------------------------------------------------------------

# fecal_delistings <- read.xlsx(paste0(filepath, "Fecal Coliform Delist.xlsx")) |> 
#   pull(AU_ID)

# AU_decisions <- AU_decisions |> 
#   mutate(final_AU_cat = case_when(AU_ID %in% fecal_delistings & Pollu_ID == '86' & wqstd_code == '1' ~ '2',
#                                   TRUE ~ final_AU_cat),
#          Rationale = case_when(AU_ID %in% fecal_delistings & Pollu_ID == '86' & wqstd_code == '1' ~ 'Delist: Criteria Change - 2024 E. Coli assessment = Attaining - No Shell Fish Harvest Sub-Use in AU',
#                                TRUE ~ Rationale),
#          status_change = case_when(AU_ID %in% fecal_delistings & Pollu_ID == '86' & wqstd_code == '1' ~ 'Delist',
#                                    TRUE ~ status_change),
#          
#   )




# GNIS ------------------------------------------------------------------------------------------------------------

GNIS_Decisions <- bind_rows(gnis_bact_fresh, gnis_bact_fresh_entero, gnis_biocriteria, gnis_chl, gnis_do, gnis_nonR,
                            gnis_pH, gnis_temp, gnis_tox_al, gnis_tox_hh, gnis_turb)




## Get unassessed pollutants to move forward -----------------------------------------------------------------------



assessed_polluids <- GNIS_Decisions$Pollu_ID 



unassessed_params <- odeqIRtools::prev_list_GNIS |> 
  filter(!Pollu_ID %in% assessed_polluids) |>
  rename(Char_Name = Pollutant) |> 
  mutate(final_GNIS_cat = prev_GNIS_category,
         Rationale_GNIS = prev_GNIS_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'GNIS')

GNIS_decisions <- GNIS_Decisions |> 
  bind_rows(unassessed_params)


antijoin2 <- odeqIRtools::prev_list_GNIS |> 
  anti_join(GNIS_decisions, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
  rename(Char_Name = Pollutant) |> 
  mutate(final_GNIS_cat = prev_GNIS_category,
         Rationale_GNIS = prev_GNIS_rationale,
         status_change = "No change in status- No new assessment") |> 
  join_TMDL(type = 'GNIS')


GNIS_decisions <- GNIS_Decisions |> 
  bind_rows(antijoin2)

# Get assessment labels -------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


wqstd_info <- tbl(con, "LU_Wqstd_Code") |> 
  collect() |> 
  mutate(wqstd_code = as.character(wqstd_code) ) |> 
  rename('Assessment' = 'wqstd') 
DBI::dbDisconnect(con)

GNIS_decisions <- GNIS_decisions |> 
  left_join(wqstd_info) |> 
  relocate(Assessment, .after = Char_Name)







# TMDL updates from public comment --------------------------------------------------------------------------------
# 
# source(file = 'TNDL_error_update_internal_review.R')
# 
# 
# AU_decisions <- AU_decisions |> 
#   left_join(TMDL_updates, by = c('AU_ID', 'Pollu_ID', 'wqstd_code', 'period')) |> 
#   mutate(final_AU_cat = case_when(!is.na(New.category) ~ New.category,
#                                   TRUE ~ final_AU_cat),
#          TMDLs = case_when(!is.na(New.category) ~ TMDLs.new,
#                            TRUE ~ TMDLs),
#          action_ids = case_when(!is.na(New.category) ~ action_ids.new,
#                                 TRUE ~ action_ids),
#          TMDL_pollutants = case_when(!is.na(New.category) ~ TMDL_pollutants.new,
#                                      TRUE ~ TMDL_pollutants),
#          TMDL_Periods = case_when(!is.na(New.category) ~ TMDL_Periods.new,
#                                   TRUE ~ TMDL_Periods),
#          
#   ) |> 
#   select(-(32:36)) |> 
#   distinct()

# Clear all but needed --------------------------------------------------------------------------------------------

rm(list=setdiff(ls(), c("GNIS_decisions", 'AU_decisions')))

# Ben use ---------------------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

LU_BU_Assessment <- DBI::dbReadTable(con, 'LU_BU_Assessment') %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


# lookup Benuses --------------------------------------------------------------------------------------------------


LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')

names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

LU_benuses$ben_use_code <- as.numeric(LU_benuses$ben_use_code)

# Join_benuses ----------------------------------------------------------------------------------------------------

#get a list of AU ben use codes
AU_to_ben_use <- AU_decisions %>%
  select(AU_ID, AU_UseCode) %>%
  distinct() |> 
  mutate(AU_UseCode = as.character(AU_UseCode))




# This is a long form table of all the beneficial uses that apply to a given AU
all_ben_uses <- AU_to_ben_use %>%
  mutate(ben_use_code = as.numeric(AU_UseCode)) %>%
  left_join(LU_benuses, relationship = "many-to-many") %>%
  filter(!is.na(ben_use),
         ben_use != "NULL")

all_ben_uses_2 <- all_ben_uses |> 
  mutate(keep = "keep") %>%
  select(-ben_use)



AU_BU <- AU_decisions %>%
  left_join(select(LU_BU_Assessment, -Assessment), by = c("Pollu_ID", 'wqstd_code'), relationship = "many-to-many" ) %>%
  mutate(AU_UseCode = as.character(AU_UseCode)) |> 
  right_join(all_ben_uses_2) %>%
  select(-keep) %>%
  filter(!is.na(Char_Name))





BU_rollup <- AU_BU %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ ben_use
  )) |> 
  mutate(final_AU_cat = factor(final_AU_cat, 
                               levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A','5C', "5" ), ordered=TRUE)) %>%
  group_by(AU_ID, ben_use) %>%
  summarise(AU_Name = max(AU_Name, na.rm = TRUE),
            Category = max(final_AU_cat),
            parameters = stringr::str_c(unique(Char_Name), collapse = "; ")) %>%
  full_join(filter(all_ben_uses, AU_ID %in% AU_decisions$AU_ID),relationship = "many-to-many") %>%
  mutate(Category = as.character(Category),
         Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
  select(-ben_use_id) %>%
  group_by(AU_ID) %>%
  mutate(AU_Name = max(AU_Name, na.rm = TRUE)) |> 
  relocate(AU_Name, .after='AU_ID') |> 
  arrange(AU_ID)




BU_rollup_wide <- BU_rollup %>%
  select(-parameters) |> 
  #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
  spread(ben_use, Category, fill = "-") 




# Map display -----------------------------------------------------------------------------------------------------


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Add 5C to category 5 counts!!!!!!!!!!!!!!






map_display <- AU_decisions |> 
  mutate(final_AU_cat = factor(final_AU_cat, 
                               levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name
  )) |> 
  group_by(AU_ID) %>%
  summarise(AU_status = case_when(any(str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4') | str_detect(final_AU_cat, '5C'))~ 'Impaired',
                                  any(str_detect(final_AU_cat, '2')) ~ "Attaining",
                                  all(str_detect(final_AU_cat, '3')) ~ "Insufficient Data",
                                  TRUE ~ "ERROR"),
            year_last_assessed = max(year_last_assessed, na.rm = TRUE),
            Year_listed = ifelse(AU_status == 'Impaired', as.integer(min(Year_listed),  na.rm = TRUE), NA_integer_ ) ,
            Cat_5_count = length(pollutant_strd[final_AU_cat == '5' | final_AU_cat == '5C']),
            Cat_4_count = length(pollutant_strd[str_detect(final_AU_cat, '4')]),
            Impaired_count = Cat_5_count + Cat_4_count,
            Impaired_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4'))]), ", "),
            Cat_5_parameters =   str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '5') )]), ", "),
            Cat_4_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '4') )]), ", "),
            Cat_2_count = length(pollutant_strd[final_AU_cat == '2']),
            Attaining_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & final_AU_cat == '2']), ", "),
            Cat_3_count = length(pollutant_strd[final_AU_cat == '3']),
            Cat_3B_count = length(pollutant_strd[final_AU_cat == '3B']),
            Cat_3D_count = length(pollutant_strd[final_AU_cat == '3D']),
            Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
            Insufficient_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & str_detect(final_AU_cat, '3')]), ", ")
  )

map_display_GNIS <- GNIS_decisions |> 
  mutate(final_GNIS_cat = factor(final_GNIS_cat, 
                                 levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name
  )) |> 
  group_by(AU_ID, AU_GNIS_Name) %>%
  summarise(AU_status = case_when(any(str_detect(final_GNIS_cat, '5') | str_detect(final_GNIS_cat, '4')| str_detect(final_GNIS_cat, '5C'))~ 'Impaired',
                                  any(str_detect(final_GNIS_cat, '2')) ~ "Attaining",
                                  all(str_detect(final_GNIS_cat, '3')) ~ "Insufficient Data",
                                  TRUE ~ "ERROR"),
            #year_last_assessed = max(year_last_assessed, na.rm = TRUE),
            #Year_listed = ifelse(AU_status == 'Impaired', as.integer(min(Year_listed),  na.rm = TRUE), NA_integer_ ) ,
            Cat_5_count = length(pollutant_strd[final_GNIS_cat == '5' | final_GNIS_cat == '5C']),
            Cat_4_count = length(pollutant_strd[str_detect(final_GNIS_cat, '4')]),
            Impaired_count = Cat_5_count + Cat_4_count,
            Impaired_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '5') | str_detect(final_GNIS_cat, '4'))]), ", "),
            Cat_5_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '5'))]), ", "),
            Cat_4_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '4'))]), ", "),
            Cat_2_count = length(pollutant_strd[final_GNIS_cat == '2']),
            Attaining_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & final_GNIS_cat == '2']), ", "),
            Cat_3_count = length(pollutant_strd[final_GNIS_cat == '3']),
            Cat_3B_count = length(pollutant_strd[final_GNIS_cat == '3B']),
            Cat_3D_count = length(pollutant_strd[final_GNIS_cat == '3D']),
            Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
            Insufficient_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & str_detect(final_GNIS_cat, '3')]), ", ")
  )



# Write excel -----------------------------------------------------------------------------------------------------


print_list <- list('AU_decisions'    = AU_decisions      ,
                   'GNIS_decisions'  = GNIS_decisions    ,
                   'AU_BU'           = AU_BU             ,
                   'BU_rollup'       = BU_rollup         ,
                   'BU_rollup_wide'  = BU_rollup_wide    ,
                   'map_display'     = map_display      ,
                   "map_display_GNIS" = map_display_GNIS)


write.xlsx(print_list, file = paste0("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/internal_draft/IR_2026_Internal_review_Rollup-", Sys.Date(),  ".xlsx") )


save(print_list, file = 'draft_list/draft_list.Rdata')



