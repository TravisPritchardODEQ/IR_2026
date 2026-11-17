#' @title Build ATTAINS bulk-upload tables for the Oregon DEQ 2026 Integrated Report
#'
#' @description
#' Transforms the final Assessment Unit (AU) rollup workbook for the 2026
#' Integrated Report (IR) into the flat tables that EPA's ATTAINS system expects
#' for a bulk import: **assessments, uses, parameters, seasons, and associated
#' actions**. Each table is stored as a named element of the list
#' `Attains_import` and written to CSV at the end of the script.
#'
#' @details
#' Terminology used throughout:
#' \itemize{
#'   \item **AU** - Assessment Unit (`AU_ID`), the spatial unit ATTAINS reports on.
#'   \item **BU / ben_use** - Beneficial use (e.g. fishing, aquatic life).
#'   \item **Pollu_ID / Char_Name** - DEQ internal pollutant ID and name.
#'   \item **wqstd_code** - Water quality standard code. In this script, 15 flags
#'     aquatic life toxics criteria and 16 flags human health toxics criteria.
#'   \item **final_AU_cat** - Final IR category for an AU/parameter/use
#'     (e.g. "2", "3", "3B", "4A", "5", "5C", "Unassessed").
#' }
#'
#' Category-to-ATTAINS logic used consistently across the script (derived from
#' the `case_when()` calls below):
#' \itemize{
#'   \item Categories starting with 4 or 5 -> not meeting criteria / not supporting
#'   \item Category 2 -> meeting criteria / fully supporting
#'   \item Categories starting with 3, or "Unassessed" -> insufficient information
#' }
#'
#' Categories are ranked with the ordered factor
#' `Unassessed < 3D < 3 < 3B < 3C < 2 < 5 < 5C < 4A < 4B < 4C`, so
#' `max()` on the factor returns the "most significant" category for
#' roll-ups. Note that 4x categories rank above 5.
#'
#' @section Inputs:
#' \itemize{
#'   \item `IR_2026_State_Final_Rollup-WORKING COPY.xlsx`, sheets `AU_decisions`
#'     and `AU_BU` (path in `AU_all_rollup_file`).
#'   \item `DEQ_IR2024_Delistings.xlsx`, sheet
#'     "Delistings - Remove from 303(d)" (2024 delistings and ATTAINS delisting
#'     reason codes).
#'   \item SQL Server database `IntegratedReport` via ODBC DSN `IR_Dev`, tables
#'     `LU_Pollutant`, `InputRaw`, and `LU_Spawn`.
#'   \item ATTAINS 2022 download files `seasons_old.csv` and
#'     `associated-actions_old.csv` (used to avoid re-submitting records that
#'     already exist in ATTAINS).
#'   \item `odeqtmdl` package data: `tmdl_actions` and `tmdl_au`.
#' }
#'
#' @section Outputs:
#' \itemize{
#'   \item `Attains_import$assessments`, `$uses`, `$parameters`, `$seasons`,
#'     `$Associated_Actions` (data frames whose column names match the ATTAINS
#'     import templates).
#'   \item CSVs under `ATTAINS/` and `ATTAINS/TMDL_redo/` (see the final section).
#'   \item `Season_24.xlsx` in the working directory (a QA copy of the seasons table).
#' }
#'
#' @section Dependencies:
#' `tidyverse` (dplyr, stringr, lubridate, etc.), `openxlsx`, `DBI`, `odbc`,
#' `glue`, and the DEQ package `odeqtmdl`.
#'
#' @note
#' The script must be run top to bottom. Later sections reuse objects created
#' earlier (`AU_BU_2024`, `Attains_polluname_LU`, `Parameters`, etc.). File paths
#' are hard-coded to one user's OneDrive, and the ODBC DSN `IR_Dev` must exist
#' on the machine running the script.

library(tidyverse)
library(openxlsx)


# ATTAINS Templates -----------------------------------------------------------------------------------------------

## Setup -----------------------------------------------------------------------------------------------------------

#' @description
#' Initializes the container for the import tables and loads the AU-level
#' assessment decisions.
#'
#' @details
#' `Attains_import` is an empty named list; each section below adds one table.
#' `AU_decisions` holds one row per AU/pollutant decision. `Pollu_ID` and
#' `wqstd_code` are coerced to character so they join cleanly to the database
#' lookups later (the database queries also cast `Pollu_ID` to varchar).
#'
#' @section Objects created:
#' `Attains_import` (list), `AU_all_rollup_file` (path), `AU_decisions` (data frame).

Attains_import <- list()
AU_all_rollup_file <- "C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report - IR_2026/Draft IR/State Final/DONOTEDIT_IR_2026_Statefinal_programmatic_Rollup-2026-10-21.xlsx"



AU_decisions <-read.xlsx(AU_all_rollup_file,
                         sheet = 'AU_decisions') |> 
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code))


# Assessments -----------------------------------------------------------------------------------------------------

#' @title ATTAINS "Assessments" table
#'
#' @description
#' Builds the list of AUs that need to be (re)submitted for the 2026 cycle,
#' with the fixed columns required by the ATTAINS assessments template.
#'
#' @details
#' Only new or changed assessments are included. An AU decision is **dropped**
#' when it is flagged "No change in status- No new assessment" *and* its final
#' category equals its previous category. It is **kept** when the status
#' changed, the category changed, or there is no previous category (a new AU).
#'
#' Caveat: because of how `filter()` treats `NA`, a row with an `NA`
#' `status_change` and a non-missing `prev_category` evaluates to `NA` and is
#' also dropped.
#'
#' The result is reduced to distinct AU IDs. `CYCLE_LAST_ASSESSED` is set to
#' 2026 and `AGENCY_CODE` to "S" (State). Optional ATTAINS columns are created
#' as `NA` so the CSV has the full template layout.
#'
#' @section Output:
#' `assessments` / `Attains_import$assessments`: one row per AU with columns
#' `ASSESSMENT_UNIT_ID`, `AGENCY_CODE`, `CYCLE_LAST_ASSESSED`,
#' `YEAR_LAST_MONITORED`, `ASSESSMENT_UNIT_STATE_IR_CAT`, `ASSESSMENT_COMMENT`,
#' `ASSESSMENT_RATIONALE`, `TROPHIC_STATUS`.

###only do new assessments or where things have changed

# 
# AU_decisions_diffs <- AU_decisions |> 
#   filter(!(status_change == 'No change in status- No new assessment' & 
#              final_AU_cat == prev_category) | is.na(prev_category))

assessments <- AU_decisions |> 
  group_by(AU_ID) |> 
  summarise(CYCLE_LAST_ASSESSED = max(year_last_assessed)) |> 
  transmute(ASSESSMENT_UNIT_ID = AU_ID,
            AGENCY_CODE = 'S',
         CYCLE_LAST_ASSESSED = CYCLE_LAST_ASSESSED,
         YEAR_LAST_MONITORED = NA_character_,
         ASSESSMENT_UNIT_STATE_IR_CAT = NA_character_,
         ASSESSMENT_COMMENT = NA_character_,
         ASSESSMENT_RATIONALE = NA_character_,
         TROPHIC_STATUS = NA_character_
         
  ) |> 
  distinct() 

Attains_import$assessments <- assessments




# Use Tab-----------------------------------------------------------------------------------------------------------------

#' @title ATTAINS "Uses" table (and the AU_BU_2024 working table)
#'
#' @description
#' Reads the AU / beneficial-use rollup, collapses it to one row per
#' AU x pollutant x standard x period x use, then summarizes to one row per
#' AU x use for the ATTAINS uses template.
#'
#' @details
#' **Step 1 - `AU_BU_2024`.** For each `AU_ID, Pollu_ID, wqstd_code, period,
#' ben_use` group, keep the row with the highest `final_AU_cat`, then break any
#' ties by keeping the first row. This object is reused by the Parameters and
#' Seasons sections. (The name is a holdover from the 2024 cycle; it contains
#' 2026 data.)
#'
#' NOTE: `final_AU_cat` is read from Excel as text here, so `max()` compares
#' strings alphabetically (e.g. "5" > "4A", and "Unassessed" > everything), not
#' by the ordered ranking used later. Confirm this is the intended tie-break.
#'
#' The commented-out `filter(AU_ID %in% assessments$ASSESSMENT_UNIT_ID)` would
#' restrict uses and parameters to new/changed AUs; it is currently disabled, so
#' uses and parameters are produced for **all** AUs.
#'
#' **Step 2 - `Uses`.**
#' \enumerate{
#'   \item Convert `final_AU_cat` to the ordered factor `AU_parameter_category`.
#'   \item Standardize `ben_use` spelling/capitalization to the ATTAINS use
#'     names; any unrecognized value becomes `"ERROR"` so it is easy to spot.
#'     Fish and aquatic life rows with `period == 'spawn'` are relabeled
#'     "fish and aquatic life - spawning".
#'   \item Build a readable pollutant label, `pollutant_strd`: the period is
#'     appended when present (e.g. "Temperature- spawn"); otherwise toxics are
#'     tagged "Aquatic Life Toxics" (`wqstd_code` 15) or "Human Health Toxics"
#'     (16). `pollutant_strd_cat` prefixes the category ("5: Temperature- spawn").
#'   \item Group by AU and use: `Category` is the worst (max) category and
#'     `USE_COMMENT` concatenates every pollutant label, truncated to 3,996
#'     characters (to stay under the ATTAINS comment length limit).
#'   \item Derive `USE_ATTAINMENT_CODE`: N (not supporting) for 4x/5x,
#'     F (fully supporting) for 2, I (insufficient information) for 3x and
#'     "Unassessed".
#'   \item Rename to ATTAINS names, add the fixed/empty template columns, set
#'     `USE_STATE_IR_CAT` (with "Unassessed" reported as "3"), and order columns.
#' }
#'
#' @section Output:
#' `Uses` / `Attains_import$uses`, one row per `ASSESSMENT_UNIT_ID` x `USE_NAME`.

AU_BU <- read.xlsx(AU_all_rollup_file,
                   sheet = "AU_BU") 


AU_BU_2026 <- AU_BU %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period, ben_use) %>%
  filter(final_AU_cat == max(final_AU_cat)) %>%
  filter(row_number()==1) #%>%
#filter(AU_ID %in% assessments$ASSESSMENT_UNIT_ID) 

Uses <- AU_BU_2026 %>%
  mutate(AU_parameter_category = factor(final_AU_cat, 
                                        levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                        ordered=TRUE)) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "livestock watering" ~ "livestock watering",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             ben_use == 'fishing' ~ 'fishing',
                             ben_use == 'aesthetic quality' ~ "aesthetic quality",
                             ben_use == 'water contact recreation' ~ 'water contact recreation',
                             TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use)) %>%
  mutate(pollutant_strd = case_when(!is.na(period) ~ paste0(Char_Name, "- ", period),
                                    wqstd_code == 15 ~  paste0(Char_Name, "- Aquatic Life Toxics"),
                                    wqstd_code == 16 ~  paste0(Char_Name, "- Human Health Toxics"),
                                    TRUE ~ Char_Name),
         pollutant_strd_cat = paste0(AU_parameter_category, ": ", pollutant_strd)) %>%
  group_by(AU_ID, ben_use) %>%
  summarise(Category = max(AU_parameter_category),
            USE_COMMENT = str_c(pollutant_strd_cat, sep = ";", collapse = "; " )) %>%
  ungroup() %>%
  mutate(USE_COMMENT = str_sub(USE_COMMENT,1,3996),
         USE_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'N',
                                         Category %in% c('2') ~ 'F',
                                         str_detect(Category, '3') ~ 'I' ,
                                         Category == 'Unassessed' ~ 'I')) %>%
  select(AU_ID, ben_use, USE_ATTAINMENT_CODE, Category, USE_COMMENT) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         USE_NAME = ben_use) %>%
  mutate(USE_AGENCY_CODE = 'S',
         USE_TREND = NA_character_,
         USE_THREATENED = NA_character_,
         USE_ASMT_BASIS = NA_character_,
         USE_MONITORING_START = NA_character_,
         USE_MONITORING_END= NA_character_,
         USE_ASMT_DATE = NA_character_,
         USE_ASSESSOR_NAME = NA_character_,
         USE_COMMENT = USE_COMMENT,
         USE_STATE_IR_CAT = case_when(Category ==  'Unassessed' ~ "3",
                                      TRUE ~ Category) ,
         USE_ORG_QUALIFIER_FLAG = NA_character_ ) %>%
  select(ASSESSMENT_UNIT_ID,USE_NAME, USE_ATTAINMENT_CODE, USE_AGENCY_CODE, USE_TREND, USE_THREATENED,USE_ASMT_BASIS,
         USE_MONITORING_START, USE_MONITORING_END, USE_ASMT_DATE, USE_ASSESSOR_NAME, USE_COMMENT,
         USE_STATE_IR_CAT, USE_ORG_QUALIFIER_FLAG)

Attains_import$uses <- Uses



# Parameters tab --------------------------------------------------------------------------------------------------
## Get attains pollutant name --------------------------------------------------------------------------------------

#' @title ATTAINS pollutant name lookup
#'
#' @description
#' Opens a connection to the `IntegratedReport` database and pulls the crosswalk
#' between DEQ pollutant IDs and ATTAINS parameter names/groups.
#'
#' @details
#' Requires the ODBC data source `IR_Dev`. `Pollu_ID` is cast to varchar in SQL
#' so it matches the character `Pollu_ID` used everywhere else in the script.
#' `Attains_polluname_LU` is joined (by `Pollu_ID`) in the Parameters, Seasons,
#' and Associated Actions sections. The connection `con` is not explicitly
#' closed and is re-created in the Seasons section.
#'
#' @section Output:
#' `Attains_polluname_LU` with columns `Pollu_ID`, `Attains_PolluName`,
#' `Attains_Group`.

library(DBI)
con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


Attains_polluname_LU <- DBI::dbGetQuery(con, "SELECT cast([Pollu_ID] AS varchar) as Pollu_ID
                                        ,[Attains_PolluName]
                                        ,[Attains_Group] 
                                        FROM [IntegratedReport].[dbo].[LU_Pollutant]
                                        "
) 



## Delistings ------------------------------------------------------------------------------------------------------

#' @description
#' Loads the delisting decisions and checks that they are unique per
#' AU/pollutant/standard/period.
#'
#' @details
#' The delistings workbook comes from the **2024** cycle (`DEQ_IR2024_Delistings.xlsx`).
#' `mutate(..., .keep = "none")` keeps only the listed columns: the four join
#' keys plus `Attains_code` (the ATTAINS delisting reason code),
#' `ATTAINS_delist_reason` (a second copy of the same code; not used later), and
#' `PARAM_DELISTING_COMMENT` (from the `Rationale` column).
#'
#' `delist_dups` counts rows per key; if any key appears more than once the
#' script stops, because a duplicate would multiply rows in the join below.
#'
#' @section Output:
#' `delistings` (data frame), `delist_dups` (diagnostic; must have 0 rows).

delistings <- read.xlsx(AU_all_rollup_file,
                        sheet = "delistings") %>%
  mutate(AU_ID, Pollu_ID, wqstd_code, period,  
         Attains_code=  ATTAINS_CODE , ATTAINS_delist_reason = ATTAINS_CODE, PARAM_DELISTING_COMMENT = Rationale,
         .keep = "none")

delist_dups <- delistings %>%
  group_by(AU_ID, Pollu_ID, wqstd_code, period) %>%
  summarise(n = n()) %>%
  filter(n > 1)

if(nrow(delist_dups) > 0 ) {
  stop('delising dups')
}


## Build the Parameters table --------------------------------------------------------------------------------------

#' @title ATTAINS "Parameters" table
#'
#' @description
#' Produces one row per AU x ATTAINS parameter x use, carrying the assessment
#' status, attainment code, listing/TMDL information, and delisting details.
#'
#' @details
#' **`parameters0`** joins `AU_BU_2024` to the pollutant lookup and the
#' delistings (both `left_join()`s use the shared column names as keys:
#' `Pollu_ID`, and `AU_ID`/`Pollu_ID`/`wqstd_code`/`period`, respectively) and
#' standardizes `ben_use` exactly as in the Uses section.
#'
#' **`Parameters`** is grouped by `AU_ID` and `Attains_PolluName`, so several
#' DEQ pollutants that map to the same ATTAINS name are evaluated together:
#' \itemize{
#'   \item `PARAM_STATUS_NAME` is a **group-level** value based on the worst
#'     category in the group: "Cause" (4x/5x), "Meeting Criteria" (2),
#'     "Insufficient Information" (3x), or "Removed" (all Unassessed).
#'   \item `PARAM_ATTAINMENT_CODE` is a **row-level** value using the same
#'     category mapping ("Not meeting criteria", "Meeting Criteria",
#'     "Not enough information").
#'   \item `PARAM_POLLUTANT_INDICATOR` is "N" for non-pollutant causes (habitat
#'     alterations, flow regime modification, benthic macroinvertebrate
#'     bioassessments), "Y" for other causes, and `NA` otherwise.
#'   \item `PARAM_YEAR_LISTED` is populated only for causes (from `Year_listed`).
#'   \item `PARAM_TARGET_TMDL_DATE` comes from `TMDL_issue_year`.
#'   \item `PARAM_PRIORITY_RANKING` uses `TMDL_Priority`, defaulting to "Low"
#'     for category 4 records that have no priority.
#'   \item `PARAM_STATE_IR_CAT` is the category ("Unassessed" -> "-").
#'   \item `PARAM_DELISTING_REASON` / `PARAM_DELISTING_COMMENT` come from the
#'     delistings join; `PARAM_DELISTING_AGENCY` is "S" only when a reason exists.
#'   \item `PARAM_COMMENT` is the `Rationale` column.
#' }
#' Columns are then renamed to ATTAINS names and a hard-coded exclusion removes
#' the spawning-use record for AU `OR_SR_1709001202_88_104175`.
#'
#' @section Output:
#' `Parameters` / `Attains_import$parameters`, keyed by `ASSESSMENT_UNIT_ID`,
#' `PARAM_NAME`, `PARAM_USE_NAME`.

delistings <- delistings |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID),
         wqstd_code = as.numeric(wqstd_code))

Attains_polluname_LU <- Attains_polluname_LU |> 
  mutate(Pollu_ID = as.numeric(Pollu_ID))


parameters0 <- AU_BU_2026 %>%
  mutate(Pollu_ID = as.numeric(Pollu_ID),
         wqstd_code = as.numeric(wqstd_code)) |> 
  left_join(Attains_polluname_LU) %>%
  left_join(delistings) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ "fish and aquatic life",
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "livestock watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             ben_use == 'fishing' ~ 'fishing',
                             ben_use == 'aesthetic quality' ~ "aesthetic quality",
                             ben_use == 'water contact recreation' ~ 'water contact recreation',
                             TRUE ~ "ERROR"
  )) %>%
  mutate(ben_use = case_when(ben_use == 'fish and aquatic life' & period == 'spawn' ~ 'fish and aquatic life - spawning',
                             TRUE ~ ben_use))

Parameters <- parameters0 %>%
  mutate(AU_parameter_category = factor(final_AU_cat, 
                                        levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                        ordered=TRUE)) %>%
  group_by(AU_ID, Attains_PolluName) %>%
  mutate(PARAM_STATUS_NAME =case_when(str_detect(max(AU_parameter_category), '5') | str_detect(max(AU_parameter_category), '4') ~ 'Cause',
                                      max(AU_parameter_category) %in% c('2') ~ 'Meeting Criteria',
                                      str_detect(max(AU_parameter_category), '3') ~ 'Insufficient Information',
                                      AU_parameter_category == "Unassessed" ~ 'Removed',
  ),
  PARAM_ATTAINMENT_CODE = case_when(str_detect(AU_parameter_category, '5') | str_detect(AU_parameter_category, '4') ~ 'Not meeting criteria',
                                    AU_parameter_category %in% c('2') ~ 'Meeting Criteria',
                                    str_detect(AU_parameter_category, '3') ~ 'Not enough information',
                                    AU_parameter_category == "Unassessed" ~ 'Not enough information'),
  PARAM_TREND = NA_character_,
  PARAM_COMMENT = Rationale,
  PARAM_AGENCY_CODE = "S",
  PARAM_POLLUTANT_INDICATOR = case_when(PARAM_STATUS_NAME == 'Cause' & Attains_PolluName %in% c('HABITAT ALTERATIONS', 'FLOW REGIME MODIFICATION', 'BENTHIC MACROINVERTEBRATES BIOASSESSMENTS') ~ 'N',
                                        PARAM_STATUS_NAME == 'Cause' ~ 'Y',
                                        TRUE ~ NA_character_),
  PARAM_YEAR_LISTED = case_when(PARAM_STATUS_NAME == 'Cause' ~ Year_listed,
                                TRUE ~ NA_character_),
  PARAM_TARGET_TMDL_DATE = NA_character_,
  PARAM_EXPECTED_TO_ATTAIN = NA_character_,
  PARAM_PRIORITY_RANKING = case_when(str_detect(AU_parameter_category, '4') & is.na(TMDL_Priority) ~ 'Low',
                                     TRUE ~ TMDL_Priority),
  PARAM_CONSENT_DECREE_CYCLE = NA_character_,
  PARAM_ALT_LISTING_ID = NA_character_,
  PARAM_STATE_IR_CAT = case_when(AU_parameter_category ==  'Unassessed' ~ '-',
                                 TRUE ~ AU_parameter_category),
  PARAM_ORG_QUALIFIER_FLAG = NA_character_,
  PARAM_DELISTING_REASON = Attains_code,
  PARAM_DELISTING_COMMENT = PARAM_DELISTING_COMMENT,
  PARAM_DELISTING_AGENCY = ifelse(!is.na(PARAM_DELISTING_REASON), 'S', NA_character_ )
  
  ) %>%
  select(AU_ID, Attains_PolluName, PARAM_USE_NAME = ben_use, PARAM_STATUS_NAME, PARAM_ATTAINMENT_CODE,PARAM_TREND,
         PARAM_COMMENT, PARAM_AGENCY_CODE, PARAM_POLLUTANT_INDICATOR,  PARAM_YEAR_LISTED, PARAM_TARGET_TMDL_DATE,
         PARAM_EXPECTED_TO_ATTAIN, PARAM_PRIORITY_RANKING, PARAM_CONSENT_DECREE_CYCLE,PARAM_ALT_LISTING_ID, 
         PARAM_STATE_IR_CAT, PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, PARAM_DELISTING_COMMENT, 
         PARAM_DELISTING_AGENCY) |> 
  rename(ASSESSMENT_UNIT_ID = AU_ID,
         PARAM_NAME = Attains_PolluName) |> 
  filter(!(ASSESSMENT_UNIT_ID == 'OR_SR_1709001202_88_104175' &  PARAM_USE_NAME == 'fish and aquatic life - spawning'))

Attains_import$parameters <- Parameters


# Seasons ---------------------------------------------------------------------------------------------------------

#' @title ATTAINS "Seasons" table
#'
#' @description
#' Creates the seasonal (spawning) date ranges attached to temperature and
#' dissolved oxygen parameters, and merges them with the seasons already
#' submitted in the 2022 cycle.
#'
#' @details
#' **Prior seasons.** `seasons_2022` is read from the 2022 ATTAINS download;
#' its dates are renamed `SEASON_START_22` / `SEASON_END_22` and its
#' `PARAM_ATTAINMENT_CODE` is dropped (it is recomputed for the new data).
#'
#' **Spawn records.** `seasons0` keeps only `period == "spawn"` rows from
#' `AU_BU_2024` and adds ATTAINS pollutant names. `AUs` is the vector of
#' distinct AU IDs used to limit the database query.
# 
# seasons_2022 <- read.csv("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/ATTAINS/2022_download/ALL_ASMT_OREGONDEQ_2022/seasons_old.csv") %>%
#   rename(SEASON_START_22 = SEASON_START,
#          SEASON_END_22 = SEASON_END) |> 
#   select(-PARAM_ATTAINMENT_CODE)
# 

seasons0 <- AU_BU_2026 %>%
  mutate(Pollu_ID = as.numeric(Pollu_ID),
         wqstd_code = as.numeric(wqstd_code)) |> 
  filter(period == "spawn") %>%
  left_join(Attains_polluname_LU)

AUs <- unique(seasons0$AU_ID)

## query spawn dates ---------------------------------------------------------------------------------------------

#' @description
#' Retrieves the spawn-period codes assigned to each AU and the calendar dates
#' that each code represents.
#'
#' @details
#' `InputRaw` supplies two spawn codes per AU/pollutant: `SpawnCode`
#' (aliased `temp_SpawnCode`, used for temperature) and `DO_SpawnCode` (used for
#' dissolved oxygen). The query is built with `glue::glue_sql()`; the
#' `{AUs*}` syntax expands the vector into a safely quoted SQL `IN (...)` list.
#' `LU_Spawn` maps each `SpawnCode` to `SpawnStart` / `SpawnEnd`, stored as
#' month/day text (e.g. "10/15").
#'
#' @section Output:
#' `Spawn_codes` (AU_ID, Pollu_ID, temp_SpawnCode, DO_SpawnCode) and
#' `Spawn_dates` (SpawnCode, SpawnStart, SpawnEnd).

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")

# query_language <- "SELECT distinct [AU_ID]
#       ,cast(Pollu_ID AS varchar) as Pollu_ID
#       ,[SpawnCode] as temp_SpawnCode
#       ,[DO_SpawnCode]
#       
#   FROM [IntegratedReport].[dbo].[InputRaw]
# WHERE AU_ID in   ({AUs*})"
# 
# spawn_query <- glue::glue_sql(query_language, .con = con)


Spawn_codes <- tbl(con, "InputRaw") |> 
  filter(AU_ID %in% AUs) |> 
  select(AU_ID, Pollu_ID, SpawnCode, DO_SpawnCode) |> 
  distinct() |> 
  collect() |> 
  rename(temp_SpawnCode = SpawnCode)
# 
# Spawn_codes <- DBI::dbGetQuery(con, spawn_query)
Spawn_dates <-  DBI::dbGetQuery(con, "SELECT [SpawnCode]
      ,[SpawnStart]
      ,[SpawnEnd]
  FROM [IntegratedReport].[dbo].[LU_Spawn]")


#' @description
#' Converts spawn codes into actual season dates and selects one season per
#' AU x parameter x use.
#'
#' @details
#' Processing steps in `Seasons_1`:
#' \enumerate{
#'   \item Join spawn codes and choose the applicable one by parameter:
#'     `temp_SpawnCode` for TEMPERATURE, `DO_SpawnCode` for DISSOLVED OXYGEN.
#'   \item Drop missing codes and codes `0` and `99` (no spawning period).
#'   \item Join to `Spawn_dates`; drop rows without a start date.
#'   \item Append the year (hard-coded `2024`, a leap year so 2/29 parses) and
#'     parse with `lubridate::mdy()`. If the end date falls before the start
#'     date (a season spanning New Year), one year is added to the end.
#'     NOTE: the following line, which is meant to subtract a year from the
#'     start, is evaluated *after* `SpawnEnd` has already been adjusted, so its
#'     condition can never be true and it has no effect.
#'   \item Compute the season `length` in days, then for each
#'     AU x parameter x use keep the longest season (first row on ties).
#'   \item `Category` is the worst category in the group; it sets
#'     `PARAM_ATTAINMENT_CODE` with the usual 4x/5x, 2, 3x mapping.
#'   \item `ben_use` is set to "fish and aquatic life - spawning" (spawning is
#'     the only use that has seasons here) and columns are renamed to ATTAINS names.
#' }

Seasons <- seasons0 %>%
  ungroup() |> 
  select(AU_ID, Pollu_ID, Attains_PolluName,ben_use, final_AU_cat  ) %>%
  mutate(AU_parameter_category =  factor(final_AU_cat, 
                                         levels=c('Unassessed', '3D',"3", "3B","3C", "2", "5",'5C', '4A', '4B', '4C'), 
                                         ordered=TRUE)) %>%
  left_join(Spawn_codes) %>%
  mutate(SpawnCode = case_when(Attains_PolluName == 'TEMPERATURE' ~ temp_SpawnCode,
                               Attains_PolluName == "DISSOLVED OXYGEN" ~ DO_SpawnCode)) %>%
  filter(!is.na(SpawnCode),
         SpawnCode != 0,
         SpawnCode != 99 ) %>%
  select(-temp_SpawnCode, -DO_SpawnCode) %>%
  left_join(Spawn_dates) %>%
  filter(!is.na(SpawnStart)) %>%
  mutate( SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",2024 ), SpawnStart ),
          SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", 2024), SpawnEnd ),
          SpawnStart = mdy(SpawnStart),
          SpawnEnd=mdy(SpawnEnd),
          SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                             SpawnEnd),
          SpawnStart = if_else(SpawnEnd < SpawnStart, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                               SpawnStart),
          length = SpawnEnd - SpawnStart + 1
  ) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  mutate(Category = max(AU_parameter_category)) %>%
  filter(length == max(length)) %>%
  group_by(AU_ID, Attains_PolluName, ben_use) %>%
  filter(row_number()==1) %>%
  mutate(PARAM_ATTAINMENT_CODE = case_when(str_detect(Category, '5') | str_detect(Category, '4') ~ 'Not meeting criteria',
                                           Category %in% c('2') ~ 'Meeting Criteria',
                                           str_detect(Category, '3') ~ 'Not enough information'),
         SEASON_START = SpawnStart,
         SEASON_END = SpawnEnd) %>%
  # SEASON_START = format(SpawnStart, format="%d-%b"),
  # SEASON_END = format(SpawnEnd, format="%d-%b")) %>%
  mutate(ben_use = case_when(ben_use == "Fishing" ~ "fishing",
                             ben_use == "Private Domestic Water Supply" ~ "Private domestic water supply",
                             ben_use == "Public Domestic Water Supply" ~ "Public domestic water supply",
                             ben_use == "Fish and Aquatic Life" ~ 'fish and aquatic life - spawning',
                             ben_use == "Water Contact Recreation" ~ "water contact recreation",
                             ben_use == "Aesthetic Quality" ~ "aesthetic quality",
                             ben_use == "Livestock Watering" ~ "livestock watering",
                             ben_use == "livestock watering" ~ "livestock watering",
                             ben_use == "Boating" ~ "boating",
                             TRUE ~ "ERROR"
  )) %>%
  ungroup() %>%
  select(AU_ID, Attains_PolluName, ben_use,PARAM_ATTAINMENT_CODE, SEASON_START, SEASON_END ) %>%
  rename(ASSESSMENT_UNIT_ID = AU_ID, 
         PARAM_NAME = Attains_PolluName,
         PARAM_USE_NAME = ben_use) |> 
  mutate(SEASON_START = as.character(format(as.Date(SEASON_START), "%m/%d")),
         SEASON_END = as.character(format(as.Date(SEASON_END), "%m/%d")))



Attains_import$seasons <- Seasons


write.xlsx(Seasons, file = 'Season_26.xlsx')



# Associated-action -----------------------------------------------------------------------------------------------

#' @title ATTAINS "Associated Actions" table
#'
#' @description
#' Links AU/parameter combinations to the TMDL actions that address them, and
#' keeps only links that are not already recorded in ATTAINS from the 2022 cycle.
#'
#' @details
#' \enumerate{
#'   \item `actions`: TMDL actions from `odeqtmdl::tmdl_actions` that are flagged
#'     `in_attains` and were issued before 2025 (`TMDL_issue_year < 2025`).
#'   \item `old_actions`: associated actions already in ATTAINS (2022 download).
#'   \item `assess_Parameters`: distinct AU x ATTAINS parameter pairs from the
#'     `Parameters` table built above, so that actions are only attached to
#'     parameters being submitted.
#'   \item `tmdl_au_1`: the AU/pollutant/action crosswalk (`odeqtmdl::tmdl_au`)
#'     limited to the selected actions.
#'   \item `Associated_actions`: crosswalk with ATTAINS pollutant names added,
#'     `ACTION_TYPE` fixed to "TMDL", and columns renamed to ATTAINS names.
#'   \item `Associated_Actions1`: join to `assess_Parameters` on AU and
#'     parameter name; rows without an `ACTION_ID` are dropped.
#'   \item `Associated_Actions_new`: `anti_join()` against `old_actions` (on
#'     all shared columns, so the column names in the old file must match) to
#'     keep only new AU/parameter/action links.
#' }
#'
#' The commented-out block at the end was a one-time fix that produced
#' dissociation records for a specific set of temperature TMDL action IDs.
#'
#' @section Output:
#' `Associated_Actions_new` / `Attains_import$Associated_Actions`.


  
  
  library(odeqtmdl)
  
  actions <- odeqtmdl::tmdl_actions |> 
    filter(in_attains == TRUE)
  
  #old_actions <- read.csv("C:/Users/tpritch/OneDrive - Oregon/DEQ - Integrated Report 🎉 - IR 2024/ATTAINS/2022_download/ALL_ASMT_OREGONDEQ_2022/associated-actions_old.csv")
  
  
  assess_Parameters <- Parameters |> 
    select(ASSESSMENT_UNIT_ID, PARAM_NAME) |> 
    distinct()
  
  
  tmdl_au_1 <- odeqtmdl::tmdl_au |> 
    filter(action_id %in% actions$action_id) |> 
    filter(TMDL_status == 'Active') |> 
    filter(TMDL_scope == 'TMDL') 
  
  Associated_actions <- tmdl_au_1 |> 
    left_join(Attains_polluname_LU) |> 
    select(AU_ID, Attains_PolluName, action_id) |> 
    mutate(ACTION_TYPE = 'TMDL') %>%
    distinct() |> 
    rename(ASSESSMENT_UNIT_ID = AU_ID,
           ACTION_PARAM_NAME = Attains_PolluName,
           ACTION_ID = action_id)
  
  Associated_Actions <- left_join(assess_Parameters, Associated_actions, 
                                   by = c('ASSESSMENT_UNIT_ID','PARAM_NAME' =  'ACTION_PARAM_NAME')) |> 
    filter(!is.na(ACTION_ID)) |> 
    rename(ACTION_PARAM_NAME = PARAM_NAME)
  
  
  
  
  # Associated_Actions_replacement <- anti_join(Associated_Actions1, old_actions) |> 
  #   filter(ACTION_ID %in% actions$action_id) |> 
  #   filter(PARAM_NAME == "TEMPERATURE",
  #          ACTION_ID %in% c("10006", "10791", "11395", "12241", "30674",
  #                           "30358", "32071", "33829", "35888", "35887",
  #                           "35890", "9294", "39782", "39294", "39753")) 
  # 
  # write.xlsx(Associated_Actions_replacement, file = "Temperature_Replacement_TMDL_Dissosociations.xlsx")
  
  Attains_import$Associated_Actions <- Associated_Actions
  
  
  # Write csvs -------------------------------------------------------------------------------------------------
  
  #' @title Export ATTAINS import files
  #'
  #' @description
  #' Writes each import table to CSV in the format ATTAINS expects: no row names
  #' and blank cells for missing values (`na = ""`).
  #'
  #' @details
  #' Relative paths are resolved from the current working directory, which must
  #' contain an `ATTAINS/` folder (and `ATTAINS/TMDL_redo/` for uses and
  #' parameters). Note that the output folders differ by table:
  #' \itemize{
  #'   \item `ATTAINS/assessments.csv`
  #'   \item `ATTAINS/TMDL_redo/uses.csv`
  #'   \item `ATTAINS/TMDL_redo/parameters.csv`
  #'   \item `ATTAINS/seasons.csv`
  #'   \item `ATTAINS/Associated_Actions.csv`
  #' }` `
  #' Existing files with the same names are overwritten without warning.
  #' 
  #
  
  upload_folder <- 'C:/Users/tpritch/OneDrive - Oregon/R Projects/IR_2026/ATTAINS/Upload_files/'
  
  today <- format(Sys.Date(), "%Y_%m_%d")
  
  upload_folder <- paste0(upload_folder, today, '/')
  
  
  if(!dir.exists(upload_folder)){
  dir.create(upload_folder)
  }
  
  

# temp---undothis remove errors -------------------------------------------
# 
#   exclude <- c("OR_EB_1710030403_01_107204",
#                "OR_SR_1709000306_05_103854",
#                "OR_SR_1710020303_05_105886",
#                "OR_SR_1710030207_02_105424",
#                "OR_SR_1710030209_02_106367",
#                "OR_SR_1801020601_05_106993",
#                "OR_WS_170601040102_02_103423",
#                "OR_WS_170601040105_02_103426",
#                "OR_WS_170601050106_02_103488",
#                "OR_WS_170702020105_05_102091",
#                "OR_WS_170702020604_05_102115",
#                "OR_WS_170702020802_05_102133",
#                "OR_WS_170702020803_05_102134",
#                "OR_WS_170702020804_05_102135",
#                "OR_WS_170702020805_05_102136",
#                "OR_WS_170703070103_05_102546",
#                "OR_WS_170900120101_02_104550",
#                "OR_WS_170900120104_02_104553",
#                "OR_WS_171003010503_02_105812",
#                "OR_WS_171003010702_02_105651",
#                "OR_WS_171003020104_02_105678",
#                "OR_WS_171003060302_02_104673")
#   
#   
#   Attains_import <-  map(Attains_import, ~ filter(.x, !ASSESSMENT_UNIT_ID %in% exclude))
  
  
  write.csv(assessments, file = paste0(upload_folder, 'assessments.csv'), row.names = FALSE,  na = "")
  write.csv(Attains_import$uses, file = paste0(upload_folder, 'uses.csv'), row.names = FALSE,  na = "")
  write.csv(Attains_import$parameters, file = paste0(upload_folder, 'parameters.csv'), row.names = FALSE,  na = "")
  write.csv(Attains_import$seasons, file = paste0(upload_folder, 'seasons.csv'), row.names = FALSE,  na = "")
  write.csv(Attains_import$Associated_Actions, file = paste0(upload_folder, 'Associated_Actions.csv'), row.names = FALSE,  na = "")
