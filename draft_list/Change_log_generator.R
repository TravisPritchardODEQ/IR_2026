library(tidyverse)
library(purrr)
library(openxlsx)


draft_report_file <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/Public_draft/IR_2026_Draft_Rollup-2026-03-30.xlsx"
                             


state_final_file <- 'C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/DONOTEDIT_IR_2026_Statefinal_programmatic_Rollup-2026-10-21.xlsx'


draft_report <- read.xlsx(draft_report_file,
                          sheet = 'AU_decisions') |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID),
         wqstd_code = as.numeric(wqstd_code)) 

state_final <- read.xlsx(state_final_file,
                         sheet = 'AU_decisions')





draft_compare <- draft_report |> 
  select(AU_ID, AU_Name, Char_Name, Pollu_ID, wqstd_code, period,
         final_AU_cat, Rationale, status_change) |> 
  mutate(Char_Name = str_trim(Char_Name, side = "right"))

final_compare <- state_final|> 
  select(AU_ID, AU_Name, Char_Name, Pollu_ID, wqstd_code, period,
         final_AU_cat, Rationale, status_change) |> 
  mutate(Char_Name = str_trim(Char_Name, side = "right"))
  





keys   <- c("AU_ID", "Pollu_ID", "wqstd_code", "period")
fields <- c("AU_Name", "Char_Name", "final_AU_cat", "Rationale", "status_change")

comparison <- full_join(
  draft_compare %>% rename_with(~ paste0(.x, "_draft"), all_of(fields)),
  final_compare %>% rename_with(~ paste0(.x, "_final"), all_of(fields)),
  by = keys, na_matches = "na"
)

# Change log: one string per row listing fields that differ
change_log <- map_chr(seq_len(nrow(comparison)), function(i) {
  row <- comparison[i, ]
  changed <- fields[map_lgl(fields, function(f) {
    d <- row[[paste0(f, "_draft")]]
    n <- row[[paste0(f, "_final")]]
    !identical(as.character(d), as.character(n))
  })]
  if (length(changed) == 0) "" else paste(changed, collapse = "; ")
})

comparison <- comparison %>% mutate(change_log = change_log)


updates <- comparison %>% filter(change_log != "") |> 
  rename(columns_updated = change_log) 


write.xlsx(updates, file = paste0("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/Statefinal_changelog_", Sys.Date(), ".xlsx"))
