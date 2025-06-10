# External data pull
library(tidyverse)
library(httr2)
library(sf)
library(tibble)
library(rlang)
library(runner)
library(odeqIRextdata)



#NWIS data pull. Ran 5/28/2025
NWIS_cont_data_pull(start.date = "2020-01-01", 
                    end.date = "2024-12-31",
                    project = "Integrated Report - Call for Data",
                    save_location = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/NWIS/")





# NERRs -------------------------------------------------------------------


#Download data from https://cdmo.baruch.sc.edu/aqs/ . Choose Zip downloads and 
# then select South Slough, OR
# Download data and then upzip in folder. Folder location is the path 
# argument in function. The function will only coniser WQ data and ignore 
# weather data. 
# 

NERRS_sum_stats(path = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/NERRS/Original Files/691968/")



# Not yet run -------------------------------------------------------------


#BES data pull. Ran 7/12/2023
  # Get password
source('External_data/ancillary files/pdx_BES_pass.R')

BES <- PDX_BES_data(userid, pass, 
                     save_location = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/BES/",
                     startdate = '2020/01/01',
                     enddate = '2024/12/31')


  
#NERRS
NERRS_sum_stats(path = 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/DataAssembly/NERRS/Orginal Files/')
  