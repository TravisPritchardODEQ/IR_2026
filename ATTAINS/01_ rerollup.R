#' ==================================================================================================================
#' Script: IR_2026_Rollup.R
#'
#' @title Oregon DEQ Integrated Report (IR) 2026 - AU/GNIS Rollup and Export
#'
#' @description
#' This script builds the final "rollup" tables used in Oregon DEQ's 2026 Water Quality
#' Integrated Report (IR). It combines Assessment Unit (AU) and GNIS-level water quality
#' decisions with beneficial use (Ben Use) lookup data pulled from the IR_Dev database,
#' then produces summary tables used for:
#'   - map display (attainment status per AU/GNIS)
#'   - the public-facing beneficial use rollup
#'   - the Clean Water Act Section 303(d) impaired waters list
#'
#' All final tables are written out to Excel workbooks and saved to disk as an R data
#' object (.Rdata) for reuse in later steps (e.g. report writing, map generation).
#'
#' @details
#' Data flow overview:
#'   1. Read raw AU-level and GNIS-level decision tables from the "working copy" Excel
#'      workbook (this is the master file analysts edit throughout the IR cycle).
#'   2. Refresh/patch AU metadata (HUC, name, etc.) from the odeqIRtools package so the
#'      working copy always reflects the latest reference data.
#'   3. Connect to the IR_Dev database and pull beneficial use lookup/assessment tables.
#'   4. Determine which beneficial uses apply to each AU, and join those to the pollutant-
#'      level decision data to build a long-format AU x Beneficial Use table (AU_BU).
#'   5. Roll that up into per-AU, per-beneficial-use summaries (BU_rollup), plus a wide
#'      version for spreadsheet display (BU_rollup_wide) and a condensed version for the
#'      interactive map (BU_AU_mapdisplay).
#'   6. Summarize each AU's/GNIS feature's overall status (Impaired / Attaining /
#'      Insufficient Data / Unassessed) for use on the public map (map_display,
#'      map_display_GNIS).
#'   7. Filter out the Category 5 (impaired, needs TMDL) waters into the 303(d) list.
#'   8. Export everything to Excel and save an .Rdata snapshot.
#'
#' @section Inputs:
#'   - "IR_2026_State_Final_Rollup-WORKING COPY.xlsx": sheets "AU_decisions" and
#'     "GNIS_decisions", the analyst-maintained master decision tables.
#'   - odeqIRtools::AU_info, odeqIRtools::HUC_info: reference/lookup data shipped in the
#'     internal odeqIRtools R package.
#'   - IR_Dev database tables "LU_BU_Assessment" and "LU_BenUseCode" (via ODBC connection
#'     "IR_Dev"), which map pollutants/standards to beneficial uses.
#'
#' @section Outputs:
#'   - "IR_2026_Draft_Rollup-<date>.xlsx": full multi-sheet workbook with every
#'     intermediate and final table (for QA/review).
#'   - "AU_decisions_update.xlsx", "map_display_update.xlsx": standalone exports of two
#'     key tables.
#'   - "draft_list/draft_list.Rdata": saved R list containing all output tables, for
#'     reuse in downstream scripts without re-running this whole pipeline.
#'
#' @note
#' This script is written to run interactively/locally (it reads and writes to a
#' OneDrive path and an ODBC data source named "IR_Dev"), so file paths and the DB
#' connection name will need to be updated if run on a different machine.
#'
#' Category codes referenced throughout (e.g. "2", "3", "3B", "3C", "3D", "4A", "4B",
#' "4C", "5", "5C") are Oregon DEQ's IR assessment categories, where roughly:
#'   - "2"  = Attaining
#'   - "3"  (and 3B/3C/3D) = Insufficient Data (various sub-reasons)
#'   - "4"  (4A/4B/4C) and "5"/"5C" = Impaired (4 = impaired but addressed some other
#'           way, e.g. existing TMDL; 5 = impaired, needs a new TMDL / 303(d) list)
#'   - "Unassessed" = no data available to assess
#' ==================================================================================================================

library(tidyverse)   # data wrangling (dplyr, tidyr, stringr, etc.)
library(openxlsx)    # reading/writing Excel files
library(odeqIRtools) # internal DEQ package with IR reference data (AU_info, HUC_info)
library(odeqtmdl)    # internal DEQ package with TMDL-related utilities/reference data
library(duckdb)      # in-process database engine (loaded for downstream use / caching)
options(scipen = 999)  # suppress scientific notation 

# Clear all but needed --------------------------------------------------------------------------------------------
# Load the two master decision tables from the analyst-maintained "working copy" workbook.
# AU_decisions: one row per Assessment Unit (AU) x pollutant/standard combination, with the
#   assigned IR category (e.g. "2", "5", "Unassessed") for that AU/pollutant.
# GNIS_decisions: the equivalent decisions rolled up to GNIS (named waterbody) level, used
#   where AU-level detail isn't the right granularity for a given waterbody.
AU_decisions <-  read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/IR_2026_State_Final_Rollup-WORKING COPY.xlsx", sheet = 'AU_decisions')

GNIS_decisions <-  read.xlsx("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/IR_2026_State_Final_Rollup-WORKING COPY.xlsx", sheet = 'GNIS_decisions')



# Update AU metainfo ------------------------------------------------------
# Pull the latest AU reference metadata (name, HUC, etc.) from the odeqIRtools package and
# use it to patch/refresh the working copy, so the workbook doesn't go stale between edits
# to the package's reference tables.

# Drop AU_UseCode from the package's AU_info table (we don't want it overwriting the
# working copy's own use-code column) and make sure HUC12 is a character (not numeric),
# so it joins cleanly and doesn't lose any leading zeros.
AU_info2 <- odeqIRtools::AU_info |> 
  select(-AU_UseCode) |> 
  mutate(HUC12 = as.character(HUC12))

# rows_update() overwrites matching rows in AU_decisions with the newer AU_info2/HUC_info
# values (matched by AU_ID, then by HUC12), leaving any AU_IDs/HUC12s that aren't found in
# the reference tables untouched ("unmatched = 'ignore'" means don't error on those).
AU_decisions2 <- AU_decisions |> 
  rows_update(AU_info2, by = 'AU_ID', unmatched = "ignore") |> 
  rows_update(odeqIRtools::HUC_info, by = 'HUC12', unmatched = "ignore")

# NOTE: AU_decisions2 is created here but AU_decisions (not AU_decisions2) is what gets
# used further down the script. If the refreshed metadata above should actually be used,
# double check whether this was intentional.



# Ben use ---------------------------------------------------------------------------------------------------------
# Connect to the "IR_Dev" ODBC data source (an Oregon DEQ SQL Server database that holds
# the master beneficial use lookup/crosswalk tables used across the IR program).
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

# LU_BU_Assessment: crosswalk table that says, for a given pollutant (Pollu_ID) and water
# quality standard (wqstd_code), which beneficial uses that standard is meant to protect.
# Pollu_ID/wqstd_code are cast to character so they join cleanly against the same columns
# elsewhere in the script (which may be read in as character from Excel).
LU_BU_Assessment <- DBI::dbReadTable(con, 'LU_BU_Assessment') %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


# lookup Benuses --------------------------------------------------------------------------------------------------

# LU_BenUseCode: lookup table translating a numeric beneficial-use code into its human-
# readable name (e.g. code -> "Fishing", "Boating", etc.).
LU_benuses <- DBI::dbReadTable(con, 'LU_BenUseCode')

# Rename the raw database column names to clearer, script-friendly names.
names(LU_benuses) <- c("ben_use_code", "ben_use_id", "ben_use")

# Ensure the use code is numeric so it matches the numeric AU_UseCode parsed below.
LU_benuses$ben_use_code <- as.numeric(LU_benuses$ben_use_code)

# Join_benuses ----------------------------------------------------------------------------------------------------

# Get the distinct list of AUs and their assigned beneficial-use code (AU_UseCode is a
# packed code representing which beneficial uses apply to that AU).
AU_to_ben_use <- AU_decisions %>%
  select(AU_ID, AU_UseCode) %>%
  distinct() |> 
  mutate(AU_UseCode = as.character(AU_UseCode))




# This is a long form table of all the beneficial uses that apply to a given AU.
# Join each AU's use code against the LU_benuses lookup to translate the code into the
# actual list of beneficial use names that apply to that AU (one row per AU x ben use).
all_ben_uses <- AU_to_ben_use %>%
  mutate(ben_use_code = as.numeric(AU_UseCode)) %>%
  left_join(LU_benuses, relationship = "many-to-many") %>%
  filter(!is.na(ben_use),      # drop AUs where no beneficial use could be matched
         ben_use != "NULL")    # drop any literal "NULL" strings that snuck in from the DB

# A trimmed copy used purely as a join "filter": adding a `keep` flag lets us right_join
# against it later to restrict AU_BU to only AU/ben-use combinations that are valid,
# without carrying the (now redundant) ben_use name column along for that join.
all_ben_uses_2 <- all_ben_uses |> 
  mutate(keep = "keep") %>%
  select(-ben_use)



# Build the master AU x Beneficial Use x Pollutant table:
#   1. Join each AU/pollutant/standard decision row to the beneficial uses that standard
#      protects (via LU_BU_Assessment).
#   2. Right-join against all_ben_uses_2 so we only keep AU/beneficial-use combinations
#      that are actually valid for that AU (i.e., drop any that don't apply there).
#   3. Drop rows with no pollutant/parameter name, since those aren't meaningful decisions.
AU_BU <- AU_decisions %>%
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) |> 
  left_join(select(LU_BU_Assessment, -Assessment), by = c("Pollu_ID", 'wqstd_code'), relationship = "many-to-many" ) %>%
  mutate(AU_UseCode = as.character(AU_UseCode)) |> 
  right_join(all_ben_uses_2) %>%
  select(-keep) %>%
  filter(!is.na(Char_Name))   # Char_Name = pollutant/parameter name; drop unmatched rows





# BU_rollup: summarize AU_BU down to one row per AU x beneficial use, giving the worst
# (highest-severity) assessment category and the list of pollutants driving that category.
BU_rollup <- AU_BU %>%
  # Standardize beneficial use names to the lower/mixed case used in public-facing output.
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ ben_use
  )) |> 
  # Turn the category into an ORDERED factor so max() below picks the most severe category
  # (categories are listed here from least to most severe/impaired).
  mutate(final_AU_cat = factor(final_AU_cat, 
                               levels=c("Unassessed", '3D',"3", "3B", "3C", "2", '4B', '4C', '4','4A','5C', "5" ), ordered=TRUE)) %>%
  group_by(AU_ID, ben_use) %>%
  summarise(AU_Name = max(AU_Name, na.rm = TRUE),
            Category = max(final_AU_cat),   # worst (most severe) category for this AU/use
            parameters = stringr::str_c(unique(Char_Name), collapse = "; ")) %>%  # pollutants driving it
  # Add back in any AU/beneficial-use combinations that had no assessed pollutants at all
  # (these become "Unassessed" below) — this keeps beneficial uses with zero data visible
  # in the rollup rather than silently dropping them.
  full_join(filter(all_ben_uses, AU_ID %in% AU_decisions$AU_ID),relationship = "many-to-many") %>%
  mutate(Category = as.character(Category),
         Category = ifelse(is.na(Category), 'Unassessed', Category )) %>%
  select(-ben_use_id) %>%
  group_by(AU_ID) %>%
  mutate(AU_Name = max(AU_Name, na.rm = TRUE)) |>   # fill AU_Name back in for the rows added by full_join
  relocate(AU_Name, .after='AU_ID') |> 
  arrange(AU_ID)

# BU_AU_mapdisplay: condensed, one-row-per-AU version listing which beneficial uses fall
# into each status bucket, for a simplified map popup/table.
# str_detect(Category, '5') also matches "5C"; str_detect(Category, '4') matches all of
# 4/4A/4B/4C; str_detect(Category, '3') matches 3/3B/3C/3D.
BU_AU_mapdisplay <- BU_rollup |> 
  group_by(AU_ID) |> 
  summarise(Impaired_Ben_Uses = str_c(ben_use[str_detect(Category, '5') |str_detect(Category, '4') ], collapse = "; "),
            Attaining_Ben_Uses = str_c(ben_use[str_detect(Category, '2')], collapse = "; "),
            Insufficient_Ben_Uses	 = str_c(ben_use[str_detect(Category, '3')], collapse = "; "),
            Unassessed_Ben_Uses = str_c(ben_use[str_detect(Category, 'Unassessed')], collapse = "; "),
  )

# BU_rollup_wide: pivot BU_rollup so each beneficial use becomes its own column (with the
# assessment category as the value), which is easier for reviewers to scan in Excel.
BU_rollup_wide <- BU_rollup %>%
  select(-parameters) |> 
  #mutate(Category = ifelse(is.na(Category), "-", Category)) %>%
  spread(ben_use, Category, fill = "-")   # fill = "-" marks AU/use combos with no category


# Build a semicolon-separated list of beneficial uses tied to each AU/pollutant/standard/
# period combination, then merge that back onto AU_decisions (replacing whatever was
# previously in its `ben_uses` column) so each decision row shows which uses it affects.
assess_ben_use <- AU_BU |> 
  group_by(AU_ID, Pollu_ID, wqstd_code, period) |> 
  summarise(ben_uses = str_c(ben_use, collapse = '; ')) |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID),
         wqstd_code = as.numeric(wqstd_code))

AU_decisions <- AU_decisions |> 
  select(-ben_uses) |>              # drop the old ben_uses column
  left_join(assess_ben_use)         # replace it with the freshly computed version



# Map display -----------------------------------------------------------------------------------------------------


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Add 5C to category 5 counts!!!!!!!!!!!!!!
# TODO (flagged by original author): Cat_5_count below already includes '5C' in its
# filter, but double-check that every downstream count/label treats 5C consistently
# with 5, per this reminder.





# map_display: one row per AU summarizing its overall status and pollutant counts/lists,
# used to drive the public map and summary tables.
map_display <- AU_decisions |> 
  # Ordered factor so string-detection comparisons below behave predictably.
  mutate(final_AU_cat = factor(final_AU_cat, 
                               levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |> 
  # Build a display label for each pollutant, adding the assessment "period" (e.g. a
  # season) or, for toxics standards (wqstd_code 15/16), whether it's an aquatic-life or
  # human-health toxics listing, so the same pollutant name isn't ambiguous across periods.
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name
  )) |> 
  mutate(Year_listed = as.integer(Year_listed)) |> 
  group_by(AU_ID) %>%
  summarise(
    # Determine overall AU status: Impaired takes priority if ANY pollutant is 4/5/5C;
    # otherwise Attaining if ANY is category 2; otherwise Insufficient Data only if ALL
    # are category 3 (any flavor); otherwise Unassessed if ALL are Unassessed; anything
    # else falls through to "ERROR" as a data-quality flag to catch unexpected combos.
    AU_status = case_when(any(str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4') | str_detect(final_AU_cat, '5C'))~ 'Impaired',
                          any(str_detect(final_AU_cat, '2')) ~ "Attaining",
                          all(str_detect(final_AU_cat, '3')) ~ "Insufficient Data",
                          all(str_detect(final_AU_cat, 'Unassessed')) ~ "Unassessed",
                          TRUE ~ "ERROR"),
    year_last_assessed = max(year_last_assessed, na.rm = TRUE),
    # Only report a listing year if the AU is actually Impaired; take the earliest
    # listing year among its impaired pollutants.
    Year_listed = ifelse(AU_status == 'Impaired', min(Year_listed,  na.rm = TRUE), NA_integer_ ) ,
    Cat_5_count = length(pollutant_strd[final_AU_cat == '5' | final_AU_cat == '5C']),
    Cat_4_count = length(pollutant_strd[str_detect(final_AU_cat, '4')]),
    Impaired_count = Cat_5_count + Cat_4_count,
    Impaired_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '5') | str_detect(final_AU_cat, '4'))]), ", "),
    Cat_5_parameters =   str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '5') )]), ", "),
    Cat_4_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & (str_detect(final_AU_cat, '4') )]), ", "),
    Cat_2_count = length(pollutant_strd[final_AU_cat == '2']),
    Attaining_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & final_AU_cat == '2']), ", "),
    Cat_3_count = length(pollutant_strd[final_AU_cat == '3']),
    Cat_3B_count = length(pollutant_strd[final_AU_cat == '3B']),
    Cat_3D_count = length(pollutant_strd[final_AU_cat == '3D']),
    Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
    Insufficient_parameters = str_flatten(unique(pollutant_strd[!is.na(final_AU_cat) & str_detect(final_AU_cat, '3')]), ", ")
  ) |> 
  left_join(BU_AU_mapdisplay)   # attach the per-AU beneficial-use summary computed earlier

# map_display_GNIS: same idea as map_display above, but rolled up to GNIS (named
# waterbody) level and grouped by both AU_ID and AU_GNIS_Name. Note this version's
# AU_status logic has no "Unassessed" branch, so an all-Unassessed GNIS feature would
# currently fall through to "ERROR" rather than "Unassessed" -- worth confirming that's
# intended and consistent with map_display above.
map_display_GNIS <- GNIS_decisions |> 
  mutate(final_GNIS_cat = factor(final_GNIS_cat, 
                                 levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), ordered=TRUE)) |> 
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name
  )) |> 
  group_by(AU_ID, AU_GNIS_Name) %>%
  summarise(AU_status = case_when(any(str_detect(final_GNIS_cat, '5') | str_detect(final_GNIS_cat, '4')| str_detect(final_GNIS_cat, '5C'))~ 'Impaired',
                                  any(str_detect(final_GNIS_cat, '2')) ~ "Attaining",
                                  any(str_detect(final_GNIS_cat, '3')) ~ "Insufficient Data",
                                  all(str_detect(final_GNIS_cat, 'Unassessed')) ~ "Unassessed",
                                  TRUE ~ "ERROR"),
            #year_last_assessed = max(year_last_assessed, na.rm = TRUE),
            #Year_listed = ifelse(AU_status == 'Impaired', as.integer(min(Year_listed),  na.rm = TRUE), NA_integer_ ) ,
            Cat_5_count = length(pollutant_strd[final_GNIS_cat == '5' | final_GNIS_cat == '5C']),
            Cat_4_count = length(pollutant_strd[str_detect(final_GNIS_cat, '4')]),
            Impaired_count = Cat_5_count + Cat_4_count,
            Impaired_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '5') | str_detect(final_GNIS_cat, '4'))]), ", "),
            Cat_5_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '5'))]), ", "),
            Cat_4_parameters =  str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & (str_detect(final_GNIS_cat, '4'))]), ", "),
            Cat_2_count = length(pollutant_strd[final_GNIS_cat == '2']),
            Attaining_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & final_GNIS_cat == '2']), ", "),
            Cat_3_count = length(pollutant_strd[final_GNIS_cat == '3']),
            Cat_3B_count = length(pollutant_strd[final_GNIS_cat == '3B']),
            Cat_3D_count = length(pollutant_strd[final_GNIS_cat == '3D']),
            Cat_3_count_total = sum(Cat_3_count, Cat_3B_count, Cat_3D_count),
            Insufficient_parameters = str_flatten(unique(pollutant_strd[!is.na(final_GNIS_cat) & str_detect(final_GNIS_cat, '3')]), ", ")
  )



# 303(d) ------------------------------------------------------------------

# The federal Clean Water Act Section 303(d) list is simply every decision row whose
# final AU category contains "5" (i.e. Category 5 or 5C -- impaired, needs a TMDL).
list_303d <- AU_decisions |> 
  filter(str_detect(final_AU_cat, "5"))


delistings <- AU_decisions |> 
  filter(str_detect(status_change, 'Delist') )

delist_rationale_errors <- AU_decisions |> 
  filter( (prev_category == '5' & final_AU_cat != '5'))

if(nrow(delistings) != nrow(delist_rationale_errors)){
  warning("mismatch in delisting rationales. Make sure all delistings have the word delist in the rationale")
}




## Add attains code --------------------------------------------------------
attains_delist_code_translator <- tibble::tribble(
                                                         ~status_change,           ~ATTAINS_CODE,
                                                   "Delist: Unassessed", "WQS_LISTING_INCORRECT",
                                                   "Delist: WQS change", "WQS_STANDARDS_CHANGED",
                                                      "Delist: 5 to 4A",          "DELISTING_4A",
                                    "Delist - Impaired to Insufficient",   "WQS_NEW_ASMT_METHOD",
                                                               "Delist",          "WQS_NEW_DATA"
                                    )
#remove biocriteria specific setting in 2028
delistings <- delistings |> 
  left_join(attains_delist_code_translator) |> 
  relocate(ATTAINS_CODE, .after = final_AU_cat) |> 
  mutate(ATTAINS_CODE = case_when(Char_Name == 'BioCriteria' & status_change == 'Delist' ~ "WQS_NEW_ASMT_METHOD" ,
                                  TRUE ~ ATTAINS_CODE))

if ( any(is.na(delistings$ATTAINS_CODE))) {
  stop("Not all delistings attains codes are relfected. Update 
       attains_delist_code_translator to ensure all status change options 
       are reflected")
}


# Write excel -----------------------------------------------------------------------------------------------------

# Bundle every table produced above into one named list, where each name becomes a sheet
# name in the output workbook.
print_list <- list('AU_decisions'    = AU_decisions      ,
                   'GNIS_decisions'  = GNIS_decisions    ,
                   'AU_BU'           = AU_BU             ,
                   'BU_rollup'       = BU_rollup         ,
                   'BU_rollup_wide'  = BU_rollup_wide    ,
                   'map_display'     = map_display      ,
                   "map_display_GNIS" = map_display_GNIS,
                   "303d" = list_303d,
                   "delistings" = delistings)

# Write the full multi-sheet workbook, timestamped with today's date so successive runs
# don't overwrite each other.
write.xlsx(print_list, file = paste0("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/DONOTEDIT_IR_2026_Statefinal_programmatic_Rollup-", Sys.Date(),  ".xlsx") )


# Save every output table as a single R object so later scripts (e.g. map-building or
# report-writing scripts) can load it directly without re-running this whole pipeline.
save(print_list, file = 'draft_list/state_final_list.Rdata')