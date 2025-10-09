library(tidyverse)
library(openxlsx)

#read in ml type

# Monitoring Location Types -----------------------------------------------


my_type_LU <- read.csv("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Data Pull/AWQMS lookup tables/monitoring location type.csv")

ml_type_uids <- c(32,3,58,59,62,64,65,1,2,10,13,21,30,31,74,69,55,56,33,53,34,100002, 40,41, 100003, 100001, 42, 100004, 43,100000,44,45,46,47,48,49,16)


ml_types <- my_type_LU |> 
  filter(mltyp_uid %in% ml_type_uids)

ml_types_not_included <- my_type_LU |> 
  filter(!mltyp_uid %in% ml_type_uids)

ml_types_list <- list('ml_types_include' = ml_types, 
                      'ml_types_exclude' = ml_types_not_included)

write.xlsx(ml_types_list, "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Data Pull/AWQMS lookup tables/IR_monitoring_location_types.xlsx")



# Activity Types ----------------------------------------------------------

act_type_LU <- read.csv("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Data Pull/AWQMS lookup tables/activity_type.csv")

act_type_uids <- c(1,3,51,2,38,10,37,14,13,9,6,100001,7,5,8,12,36,11,4,100005)   

act_types_include <- act_type_LU |> 
  filter(actyp_uid %in% act_type_uids)

act_types_exclude <- act_type_LU |> 
  filter(!actyp_uid %in% act_type_uids)

act_types_list <- list('act_types_include' = act_types_include, 
                      'act_types_exclude' = act_types_exclude)


write.xlsx(act_types_list, "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Data Pull/AWQMS lookup tables/IR_activity_types.xlsx")
