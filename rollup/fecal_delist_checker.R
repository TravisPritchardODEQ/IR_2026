library(tidyverse)
library(openxlsx)



# read in all freshwater contact recreation -------------------------------

freshwater_bacteriacode <- 2


con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')


freshhwater_bacteria_AUs <- tbl(con, 'VW_Bacteria') |> 
  filter(Bacteria_code == freshwater_bacteriacode) |> 
  select(AU_ID) |> 
  distinct() |> 
  collect() |> 
  pull(AU_ID)


DBI::dbDisconnect(con)




# Read in AU_decisions from rollup ----------------------------------------

rollup_file <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/internal_draft/IR_2026_Internal_review_Rollup-2026-01-20.xlsx")

AU_decisons <- read.xlsx(rollup_file,
                         sheet = 'AU_decisions')


AU_decisons_fresh_bacteria <- AU_decisons |> 
  filter(AU_ID %in% freshhwater_bacteria_AUs) |> 
  filter(wqstd_code == 1) |> 
  filter(final_AU_cat != "Unassessed") |> 
  filter(Pollu_ID %in% c(76, 86))


paired_bacteria_assessments <- AU_decisons_fresh_bacteria |> 
  group_by(AU_ID) |> 
  mutate(has_ecoli = case_when( any(Pollu_ID == 76) ~ 1,
                                TRUE ~ 0),
         has_fecal = case_when( any(Pollu_ID == 86) ~ 1,
                                 TRUE ~ 0)) |> 
  filter(has_ecoli == 1 & has_fecal == 1)
  

