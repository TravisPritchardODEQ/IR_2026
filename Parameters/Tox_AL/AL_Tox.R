#Needs copper assessment
#needs penta to be slip into WS and other units




source("parameters/Tox_AL/fun_toxAL_data.R")
source("parameters/Tox_AL/fun_AL_Tox_Assessment.R")
source("parameters/Tox_AL/fun_ToxAL_Pentachlorophenol_data.R")
source("parameters/Tox_AL/fun_ToxAL_Pentachlorophenol_assessment.R")
source("parameters/Tox_AL/fun_ToxAL_copper_data.R")
source("parameters/Tox_AL/fun_ToxAL_hardnessmetals.R")
source("parameters/Tox_AL/fun_ToxAL_ammonia.R")
source('Parameters/Tox_AL/toxAL_aluminum.R')
source('Parameters/Tox_AL/fun_ToxAL_Copper_assessment.R')



# Non calculated standards ------------------------------------------------

# Fetch data for analysing the parameters with non-calculated standards
Tox_AL_Censored_data <- tox_AL_data("IR_Dev")

# Run the analysis
Tox_AL_categories <- TOX_AL_analysis(Tox_AL_Censored_data)




# Hardness based standards ----------------------------------------------------------------------------------------

Tox_AL_hardness_cat <- Hardness_based_metals("IR_Dev")


# Pentachlorophenol -----------------------------------------------------------------------------------------------------------
tox_AL_penta_data <- Pentachlorophenol_data("IR_Dev")
tox_AL_penta_cat <- TOX_AL_penta_analysis(tox_AL_penta_data)

# Copper data -----------------------------------------------------------------------------------------------------


Copper_data("IR_Dev")
Copper_categories <- copper_assessment(CU_file= 'Parameters/Tox_AL/Copper_criteria_results_2024.csv')


# Ammonia ---------------------------------------------------------------------------------------------------------

tox_AL_Ammonia <- ToxAL_Ammonia("IR_Dev")


# Aluminum --------------------------------------------------------------------------------------------------------

tox_AL_aluminum  <- aluminum_assessment("IR_Dev") 

# Pull data together for export -----------------------------------------------------------------------------------


tox_AL_data                   <- Tox_AL_categories[["data"]]
tox_AL_AU_Decisions           <- Tox_AL_categories[["AU_Decisions"]] |> 
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_other_AU_cat           <- Tox_AL_categories[['Other_AU_categorization']]
tox_AL_WS_cats                <- Tox_AL_categories[["WS_Station_cat"]]
tox_AL_GNIS_cat               <- Tox_AL_categories[["WS_GNIS_cat"]]



tox_AL_hard_data               <- Tox_AL_hardness_cat[["data"]]
tox_AL_hard_AU_Decisions       <- Tox_AL_hardness_cat[['AU_Decisions']]|> 
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_hard_other_AU_cat       <- Tox_AL_hardness_cat[["Other_AU_categorization"]]
tox_AL_hard_WS_cats            <- Tox_AL_hardness_cat[["WS_Station_cat"]]
tox_AL_hard_GNIS_cat           <- Tox_AL_hardness_cat[["WS_GNIS_cat"]]

tox_AL_penta_data               <- tox_AL_penta_cat[["data"]]
tox_AL_penta_AU_Decisions       <- tox_AL_penta_cat[['AU_Decisions']]|>
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_penta_other_AU_cat       <- tox_AL_penta_cat[["Other_AU_categorization"]]
tox_AL_penta_WS_cats            <- tox_AL_penta_cat[["WS_Station_cat"]]
tox_AL_penta_GNIS_cat           <- tox_AL_penta_cat[["WS_GNIS_cat"]]

tox_AL_Ammonia_data             <- tox_AL_Ammonia[["data"]]
tox_AL_Ammonia_AU_Decisions     <- tox_AL_Ammonia[['AU_Decisions']]|>
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_Ammonia_other_AU_cat     <- tox_AL_Ammonia[["Other_AU_categorization"]]
tox_AL_Ammonia_WS_cats          <- tox_AL_Ammonia[["WS_Station_cat"]]
tox_AL_Ammonia_GNIS_cat         <- tox_AL_Ammonia[["WS_GNIS_cat"]]




tox_AL_Aluminum_data            <-  tox_AL_aluminum[["data"]]
tox_AL_Aluminum_AU_Decisions    <-  tox_AL_aluminum[['AU_Decisions']]|> 
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_Aluminum_other_AU_cat    <-  tox_AL_aluminum[["Other_AU_categorization"]]
tox_AL_Aluminum_WS_cats         <-  tox_AL_aluminum[["WS_Station_cat"]]
tox_AL_Aluminum_GNIS_cat        <-  tox_AL_aluminum[["WS_GNIS_cat"]]


tox_AL_Copper_data              <- Copper_categories[["data"]]
tox_AL_Copper_AU_Decisions      <- Copper_categories[['AU_Decisions']]|>
  mutate(Pollu_ID = as.character(Pollu_ID))
tox_AL_Copper_other_AU_cat      <- Copper_categories[["Other_AU_categorization"]]
tox_AL_Copper_WS_cats           <- Copper_categories[["WS_Station_cat"]]
tox_AL_Copper_GNIS_cat          <- Copper_categories[["WS_GNIS_cat"]]





# Combine ---------------------------------------------------------------------------------------------------------

AU_Decisions <- bind_rows(tox_AL_AU_Decisions,
                          tox_AL_hard_AU_Decisions,
                          tox_AL_penta_AU_Decisions,
                          tox_AL_Ammonia_AU_Decisions,
                          tox_AL_Aluminum_AU_Decisions, 
                          tox_AL_Copper_AU_Decisions 
                          ) |> 
  arrange(AU_ID, Char_Name)


GNIS_cat <- bind_rows(tox_AL_GNIS_cat, 
                      tox_AL_hard_GNIS_cat, 
                      #tox_AL_penta_GNIS_cat, 
                      #tox_AL_Ammonia_GNIS_cat,
                      tox_AL_Aluminum_GNIS_cat,
                      tox_AL_Copper_GNIS_cat ) |> 
  arrange(AU_ID, Char_Name)




# Write excel doc -------------------------------------------------------------------------------------------------
library(openxlsx)

header_st <- createStyle(textDecoration = "Bold", border = "Bottom")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen'            )   
addWorksheet(wb, sheetName = "GNIS_cat" , tabColour = 'lightyellow1'     )  

addWorksheet(wb, sheetName = "tox_AL_other_AU_cat"         ,tabColour = 'dodgerblue3'   ) 
addWorksheet(wb, sheetName = "tox_AL_hard_other_AU_cat"    ,tabColour = 'dodgerblue3'  ) 
#addWorksheet(wb, sheetName = "tox_AL_penta_other_AU_cat"   ,tabColour = 'dodgerblue3'  ) 
#addWorksheet(wb, sheetName = "tox_AL_Ammonia_other_AU_cat" ,tabColour = 'dodgerblue3'  ) 
addWorksheet(wb, sheetName = "tox_AL_Aluminum_other_AU_cat",tabColour = 'dodgerblue3'  ) 
addWorksheet(wb, sheetName = "tox_AL_Copper_other_AU_cat"  ,tabColour = 'dodgerblue3'  ) 

addWorksheet(wb, sheetName = "tox_AL_WS_cats"          , tabColour = 'lightblue3' ) 
addWorksheet(wb, sheetName = "tox_AL_hard_WS_cats"     , tabColour = 'lightblue3' ) 
#addWorksheet(wb, sheetName = "tox_AL_penta_WS_cats"    , tabColour = 'lightblue3' ) 
#addWorksheet(wb, sheetName = "tox_AL_Ammonia_WS_cats"  , tabColour = 'lightblue3' ) 
addWorksheet(wb, sheetName = "tox_AL_Aluminum_WS_cats" , tabColour = 'lightblue3' ) 
addWorksheet(wb, sheetName = "tox_AL_Copper_WS_cats"   , tabColour = 'lightblue3' ) 

addWorksheet(wb, sheetName = "tox_AL_data"          ,     tabColour = 'paleturquoise2' ) 
addWorksheet(wb, sheetName = "tox_AL_hard_data"     ,     tabColour = 'paleturquoise2' ) 
#addWorksheet(wb, sheetName = "tox_AL_penta_data"    ,     tabColour = 'paleturquoise2' ) 
#addWorksheet(wb, sheetName = "tox_AL_Ammonia_data"  ,     tabColour = 'paleturquoise2' ) 
addWorksheet(wb, sheetName = "tox_AL_Aluminum_data" ,     tabColour = 'paleturquoise2' ) 
addWorksheet(wb, sheetName = "tox_AL_Copper_data"   ,     tabColour = 'paleturquoise2' )  



writeData(wb = wb, sheet = "AU_Decisions"                , x = AU_Decisions, headerStyle = header_st)
writeData(wb = wb, sheet = "GNIS_cat"          ,          x = GNIS_cat , headerStyle = header_st)

writeData(wb = wb, sheet = "tox_AL_other_AU_cat"                     , x = tox_AL_other_AU_cat         , headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_hard_other_AU_cat"                , x = tox_AL_hard_other_AU_cat    , headerStyle = header_st)
#writeData(wb = wb, sheet = "tox_AL_penta_other_AU_cat"               , x = tox_AL_penta_other_AU_cat   , headerStyle = header_st)
#writeData(wb = wb, sheet = "tox_AL_Ammonia_other_AU_cat"             , x = tox_AL_Ammonia_other_AU_cat , headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Aluminum_other_AU_cat"            , x = tox_AL_Aluminum_other_AU_cat, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Copper_other_AU_cat"              , x = tox_AL_Copper_other_AU_cat  , headerStyle = header_st)

writeData(wb = wb, sheet = "tox_AL_WS_cats"                      , x = tox_AL_WS_cats         , headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_hard_WS_cats"                 , x = tox_AL_hard_WS_cats    , headerStyle = header_st)
#writeData(wb = wb, sheet = "tox_AL_penta_WS_cats"                , x = tox_AL_penta_WS_cats   , headerStyle = header_st)
#writeData(wb = wb, sheet = "tox_AL_Ammonia_WS_cats"              , x = tox_AL_Ammonia_WS_cats , headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Aluminum_WS_cats"             , x = tox_AL_Aluminum_WS_cats, headerStyle = header_st)
writeData(wb = wb, sheet = "tox_AL_Copper_WS_cats"               , x = tox_AL_Copper_WS_cats  , headerStyle = header_st)

writeData(wb = wb, sheet =  "tox_AL_data"                     , x = tox_AL_data          , headerStyle = header_st)
writeData(wb = wb, sheet =  "tox_AL_hard_data"                , x = tox_AL_hard_data     , headerStyle = header_st)
#writeData(wb = wb, sheet =  "tox_AL_penta_data"               , x = tox_AL_penta_data    , headerStyle = header_st)
#writeData(wb = wb, sheet =  "tox_AL_Ammonia_data"             , x = tox_AL_Ammonia_data  , headerStyle = header_st)
writeData(wb = wb, sheet =  "tox_AL_Aluminum_data"            , x = tox_AL_Aluminum_data , headerStyle = header_st)
writeData(wb = wb, sheet =  "tox_AL_Copper_data"              , x = tox_AL_Copper_data   , headerStyle = header_st)


print("Writing excel doc")
saveWorkbook(wb, paste0("Parameters/Outputs/Tox_AL-", Sys.Date(), ".xlsx"), overwrite = TRUE) 
