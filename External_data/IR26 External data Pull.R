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
                    save_location = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/NWIS/extrapull/")





# NERRs -------------------------------------------------------------------


#Download data from https://cdmo.baruch.sc.edu/aqs/ . Choose Zip downloads and 
# then select South Slough, OR
# Download data and then upzip in folder. Folder location is the path 
# argument in function. The function will only coniser WQ data and ignore 
# weather data. 
# 

NERRS_sum_stats(path = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/NERRS/Original Files/691968/")


#BES data pull. Ran 6/16/2025
  
# Get password
source('External_data/ancillary files/pdx_BES_pass.R')


.BES <- PDX_BES_data(userID= userid, 
                    password = pass, 
                    save_location = "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/DataAssembly/BES/",
                    startdate = '2020/01/01',
                    enddate = '2024/12/31')



# Not yet run -------------------------------------------------------------


# OWRD --------------------------------------------------------------------

#List of stations from Dan's email
OWRD_stations <- c(10378500L,
                   10392400L,
                   11491400L,
                   11494000L,
                   11494510L,
                   11495900L,
                   11497500L,
                   11497550L,
                   11500400L,
                   11500500L,
                   11502550L,
                   11502950L,
                   11503500L,
                   11504040L,
                   11504103L,
                   11504109L,
                   11504120L,
                   11510000L,
                   13214000L,
                   13215000L,
                   13216500L,
                   13217500L,
                   13269450L,
                   13273000L,
                   13274400L,
                   13275105L,
                   13275300L,
                   13281200L,
                   13282550L,
                   13317850L,
                   13318060L,
                   13318210L,
                   13318920L,
                   13320000L,
                   13325500L,
                   13329100L,
                   13329765L,
                   13330000L,
                   14010000L,
                   14010800L,
                   14012500L,
                   14012995L,
                   14021000L,
                   14022500L,
                   14023500L,
                   14024100L,
                   14024300L,
                   14025000L,
                   14026000L,
                   14029900L,
                   14031050L,
                   14031600L,
                   14032000L,
                   14032400L,
                   14039500L,
                   14054000L,
                   14054500L,
                   14056500L,
                   14060000L,
                   14063000L,
                   14064500L,
                   14070920L,
                   14070980L,
                   14073000L,
                   14073520L,
                   14074900L,
                   14075000L,
                   14076020L,
                   14076100L,
                   14079800L,
                   14080500L,
                   14081500L,
                   14082550L,
                   14083400L,
                   14085700L,
                   14087200L,
                   14087300L,
                   14088000L,
                   14088500L,
                   14095250L,
                   14095255L,
                   14104190L,
                   14104700L,
                   14104800L,
                   14105000L,
                   14105545L,
                   14105550L,
                   14153800L,
                   14192500L,
                   14193000L,
                   14202510L,
                   14202850L,
                   14306820L,
                   14306900L,
                   14320700L,
                   14327120L,
                   14327122L,
                   14327137L,
                   14327300L,
                   14335200L,
                   14335230L,
                   14335235L,
                   14335300L,
                   14335500L,
                   14336700L,
                   14337000L,
                   14340800L,
                   14341610L,
                   14342500L,
                   14343000L,
                   14346700L,
                   14347800L,
                   14348080L,
                   14348150L,
                   14348400L,
                   14350900L,
                   14352000L,
                   14352001L,
                   14354100L,
                   14354950L,
                   14355875L,
                   14357000L,
                   14357503L,
                   14358610L,
                   14358680L,
                   14358725L,
                   14358750L,
                   14358800L,
                   14360500L,
                   14363450L,
                   14365500L,
                   14368300L,
                   14375200L,
                   14400200L)


#Split list to smaller chunks to appease the API
OWRD_list <- split(OWRD_stations, ceiling(seq_along(OWRD_stations)/50))

  
OWRD_temp_data <- purrr::map(OWRD_list, ~owrd_data(., startdate = '2020-01-01',
                                         enddate = '2024-12-31',
                                         char = 'WTEMP_MAX' )) |> 
  dplyr::bind_rows() |> 
  dplyr::filter(!is.na(Result.Value))

OWRD_status_sum <-OWRD_temp_data |> 
  group_by(published_status) |> 
  summarise(count = n(),
            percent = scales::percent(n()/nrow(OWRD_temp_data))) 

ggplot(OWRD_status_sum, aes(x="", y=count, fill=published_status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()  +
  scale_fill_brewer(palette="Set1")
  