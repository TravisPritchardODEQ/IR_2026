source("Parameters/pH/function_pH_data.R")
source("Parameters/pH/function_ten_ten.R")

library(openxlsx)

pH_data <- cont_ph_raw("IR_Dev")


pH_cont <- pH_data[["pH_cont"]]
pH_grab <- pH_data[["ph_grab"]]


PH_assessments <- pH_assessment(pH_cont, pH_grab, write_xlsx = TRUE)
