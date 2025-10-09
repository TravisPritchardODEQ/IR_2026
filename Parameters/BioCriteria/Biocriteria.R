library(tidyverse)
#devtools::install_github('TravisPritchardODEQ/odeqIRtools')
library(odeqIRtools)


source('Parameters/BioCriteria/function_bio_data.R')
source('Parameters/BioCriteria/function_assess_bio_data.R')

Results_import <- bio_data("IR_Dev")

fresh_biocriteria <- fw_bio(Results_import, write_excel = TRUE)
