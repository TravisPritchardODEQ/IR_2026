source("Parameters/Temperature/fun_temp_data.R")
source("Parameters/Temperature/fun_temp_assess.R")


Results_censored_temp <- temp_data("IR_Dev")



temp_assessment <- fun_temp_analysis(Results_censored_temp)
