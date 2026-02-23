library(tidyverse)
library(odeqIRtools)
library(openxlsx)


#####This has not been run yet. Delete this when run
#####
#####
#####
#####


# TMDL UPDATER ------------------------------------------------------------

# This script updates the AU_decisions and GNIS_decisions tabs from the rollup
# file with TMDL information. It uses a restructred join_TMDL() function from 
# odeqIRtools that has been modified to work with the new structure of the 
# odeqtmdl database package. 
# 
# This script also modifies some specific TMDL issues identified in the 
# internal review process. 


## AU_decisions ------------------------------------------------------------



AU_decisions <- read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/internal_draft/IR_2026_Internal_review_Rollup-2026-01-21.xlsx",
                         sheet = 'AU_decisions')



AU_decisions_no_TMDL <- AU_decisions |> 
  select(-TMDLs, -action_ids, -TMDL_pollutants, -TMDL_Periods) |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID))



AU_decisions_update <-join_TMDL(AU_decisions_no_TMDL, type = 'AU') |> 
  mutate(status_change = case_when(prev_category == '5' & final_AU_cat == '4A' ~ 'Delist. 5 to 4A',
                                   prev_category == '4A' & final_AU_cat == '5' ~ '4A to 5',
                                   TRUE ~ status_change))


## GNIS_decisions ------------------------------------------------------------

GNIS_decisions <- read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/internal_draft/IR_2026_Internal_review_Rollup-2026-01-21.xlsx",
                          sheet = 'GNIS_decisions')



GNIS_decisions_no_TMDL <- GNIS_decisions |> 
  select(-TMDLs, -action_ids, -TMDL_pollutants, -TMDL_Periods) |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID))


GNIS_decisions_update <-join_TMDL(GNIS_decisions_no_TMDL, type = 'GNIS') |> 
  mutate(status_change = case_when(prev_GNIS_category == '5' & final_GNIS_cat == '4A' ~ 'Delist. 5 to 4A',
                                   prev_GNIS_category == '4A' & final_GNIS_cat == '5' ~ '4A to 5',
                                   TRUE ~ status_change))



# remove temp TMDL identified by michie -----------------------------------

# From a commenter:
# There are a bunch of cat 4A temperature listings in watersheds with upcoming temperature TMDL replacements. 
# The TMDLs are technically effective until replaced but Judge and EPA wanted them changed to cat 5. 
# Below is the R logic to identify these cases and change them from 4A to 5.


AU_decisions_update2 <-  AU_decisions_update |>
  mutate(final_AU_cat = case_when(Pollu_ID == 132 &
                                    final_AU_cat == "4A" &
                                    str_detect(action_ids, "10006|10007|12241|32071|33829|35887|35890|39294|39782|39753") ~ "5",
                                  TRUE ~ final_AU_cat)) |> 
  mutate(status_change = case_when(Pollu_ID == 132 &
                                     final_AU_cat == "5" &
                                     str_detect(action_ids, "10006|10007|12241|32071|33829|35887|35890|39294|39782|39753") &
                                     prev_category == '4A' & final_AU_cat == '5' &
                                     year_last_assessed == '2026' ~ '4A to 5',
                                   
                                   Pollu_ID == 132 &
                                     final_AU_cat == "5" &
                                     str_detect(action_ids, "10006|10007|12241|32071|33829|35887|35890|39294|39782|39753") &
                                     prev_category ==  final_AU_cat  &
                                     year_last_assessed == '2026' ~ "No change in status- Assessed" ,
                                   
                                   Pollu_ID == 132 &
                                     final_AU_cat == "5" &
                                     str_detect(action_ids, "10006|10007|12241|32071|33829|35887|35890|39294|39782|39753") &
                                     prev_category ==  final_AU_cat  &
                                     year_last_assessed != '2026' ~"No change in status- No new assessment"  ,
                                   
                                   TRUE ~ status_change))




