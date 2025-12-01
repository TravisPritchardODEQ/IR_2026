library(tidyverse)
#devtools::install_github('TravisPritchardODEQ/odeqIRtools')
library(odeqIRtools)


source('Parameters/BioCriteria/function_bio_data.R')
source('Parameters/BioCriteria/Category_assignment_AU.R')
source('Parameters/BioCriteria/get_low_count.R')

Results_import <- bio_data("IR_Dev")
results_low_count <- get_low_count()


fresh_biocriteria <- biocriteria_assessment(Results_import, results_low_count, write_excel = TRUE)



