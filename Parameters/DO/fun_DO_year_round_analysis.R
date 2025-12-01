


fun_DO_year_round <- function(df, write_excel = TRUE ){


  source("Parameters/DO/DO_Delist_checker.R")
  # Testing and setup -----------------------------------------------------------------------------------------------


# 
# df <- Results_censored_DO
# write_excel <- TRUE


# Variable setup --------------------------------------------------------------------------------------------------

# Number of 30-d samples needed in a year to use continuous metrics
required_crit_30d_periods <- 15

#Number of critical period samples needed to use instantaneous metrics
req_inst_crit_samples <- 8


# Year round analysts ---------------------------------------------------------------------------------------------


Results_spawndates <- df %>%
  mutate(SampleStartDate = ymd(SampleStartDate),
         SampleStartTime = stringr::str_sub(SampleStartTime, start = 1, end=5),
         SpawnStart = ifelse(!is.na(SpawnStart), paste0(SpawnStart, "/",year(SampleStartDate) ), SpawnStart ),
         SpawnEnd= ifelse(!is.na(SpawnEnd), paste0(SpawnEnd, "/", year(SampleStartDate)), SpawnEnd ),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd=mdy(SpawnEnd),
         # If Spawn dates span a calendar year, account for year change in spawn end date
         SpawnEnd = if_else(SpawnEnd < SpawnStart & SampleStartDate >= SpawnEnd, SpawnEnd + lubridate::years(1), # add a year if in spawn period carrying to next year
                            SpawnEnd),
         SpawnStart = if_else(SpawnEnd < SpawnStart & SampleStartDate <= SpawnEnd, SpawnStart - lubridate::years(1), # subtract a year if in spawn period carrying from previous year
                              SpawnStart),
         in_spawn = ifelse(SampleStartDate >= SpawnStart & SampleStartDate <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         critstart = mdy(paste0("6/1/",year(SampleStartDate) )),
         critend = mdy(paste0("9/30/",year(SampleStartDate) )),
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 )) %>%
  filter(DO_code %in% c(2,3,4))


year_round_delist_eligability <- yr_rnd_DO_delist_checker(Results_spawndates)

# Summarize available non WS data to get a list of AU's to be analyzed using cont. data
results_cont_summary <- Results_spawndates %>%
  filter(str_detect(AU_ID, "WS", negate = TRUE)) %>%
  filter(Statistical_Base == "30DADMean") %>%
  group_by(AU_ID, year(SampleStartDate)) %>%
  summarise(tot_30d_metrics = n(),
            crit_30d_periods = sum(is.crit)) %>%
  filter(crit_30d_periods >= required_crit_30d_periods,
         !is.na(AU_ID)) %>%
  pull(AU_ID) 

results_cont_summary_WS <- Results_spawndates %>%
  filter(str_detect(AU_ID, "WS", negate = FALSE)) %>%
  filter(Statistical_Base == "30DADMean") %>%
  group_by(MLocID, year(SampleStartDate)) %>%
  summarise(tot_30d_metrics = n(),
            crit_30d_periods = sum(is.crit)) %>%
  filter(crit_30d_periods >= required_crit_30d_periods) %>%
  pull(MLocID) 




# Assessment function

## Define year round continuous function --------------------------------------------------------------------------


yr_round_cont_function <- function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type){

  
  #Testing
  # df <- Results_spawndates
  # continuous_list <- results_cont_summary
  # AU_type <- 'other'
  # 
  
  
  
  #Setting AU_type to 'other' will group the analysis by AU_ID and set the filter to discard WS units (inverse = TRUE)
  #Setting AU_type to 'WS' will group the analysis by AU_ID and MlocID, and set the filter to only keep WS units (inverse = FALSE)
if(AU_type == "other"){  
group1 <- c('AU_ID', 'DO_Class')
group2 <- c('AU_ID', 'Pollu_ID', 'wqstd_code',  'DO_Class')
group3 <- c('AU_ID', 'Pollu_ID', 'wqstd_code', 'period')
inverse <- TRUE
query_type = 'AU_ID'



} else if (AU_type == "WS"){
  group1 <- c('AU_ID', 'MLocID', 'DO_Class')
  group2 <- c('AU_ID', 'MLocID','AU_GNIS_Name', 'GNIS_Name', 'Pollu_ID', 'wqstd_code','DO_Class') 
  group3 <- c('AU_ID', 'MLocID','AU_GNIS_Name', 'GNIS_Name', 'Pollu_ID', 'wqstd_code', 'period') 
  inverse <- FALSE
  query_type = 'MLocID'
}



# Setup data

# add spawn start and end dates as dates, include indicator if actdate is within spawn
# add critical period start and end dates, include indicator is actdate is within critperiod


# Initial Continuous criteria analysis --------------------------------------------

# This initial analysis is used to see where we need to calculate DO Sat 
# Calculating the 30DADMean DO SAt is computationally expensive
# so we only calculate it at locations where it woudl influnce the
# IR category

# filter down to AUs that are to be evaluated with cont metrics
# Filter down to only 30-D, 7-Mi, and daily minimums
# Flag various violations
  
  if(AU_type == "other"){  
  
continuous_data_analysis <- df %>%
  filter(AU_ID %in% continuous_list) %>%
  filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum")) %>%
  mutate(Violation = ifelse(Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D, 1, 
                            ifelse(Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi, 1, 
                                   ifelse(Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min, 1, 0 )))) 
  } else if (AU_type == "WS"){
    continuous_data_analysis <- df %>%
      filter(MLocID %in% continuous_list) %>%
      filter(Statistical_Base %in% c("30DADMean", "7DADMin", "Minimum")) %>%
      mutate(Violation = ifelse(Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D, 1, 
                                ifelse(Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi, 1, 
                                       ifelse(Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min, 1, 0 )))) 
}

# Other Units -------------------------------------------------------------------------------------------------



# Run through initial categorization
# This all gets redone in the end
# Where percent saturation would make a difference, set category as "Check percent Sat"
continuous_data_categories_other <- continuous_data_analysis %>%
  filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
  group_by_at(group1) %>%
  summarise(Total_violations = sum(Violation),
            Sum_30D_violations = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_violations = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_violations = sum(Violation [Statistical_Base == "Minimum"])) %>%
  mutate(IR_category = case_when(DO_Class != "Cold Water" & Sum_30D_violations >= 2 ~ "5",
                                 DO_Class != "Cold Water" & Sum_7mi_violations >= 2 ~ "5",
                                 DO_Class != "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_7mi_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_abs_min_violations >= 2 ~ "5",
                                 DO_Class == "Cold Water" & Sum_30D_violations >= 2 &
                                   Sum_7mi_violations < 2 & Sum_abs_min_violations < 2 ~ "Check percent Sat",
                                 Sum_30D_violations < 2 & Sum_7mi_violations < 2 & Sum_abs_min_violations < 2 ~ "2",
                                 TRUE ~ "Error" ))
           
           


# Datatable of results that need percent saturation
cont_perc_sat_check <- continuous_data_categories_other %>%
  filter(AU_ID %in% unique(subset(continuous_data_categories_other, IR_category == "Check percent Sat" )$AU_ID) )


# Query Database --------------------------------------------------------------------------------------------------

if(nrow(cont_perc_sat_check) > 0){
  
  if(AU_type == "WS"){
  
  # List of monitoring locations that need DO sat 
  # This list is used for the sql query that follows
  continuous_mon_locs <- unique(cont_perc_sat_check$MLocID)
  
  
  # Get data from database --------------------------------------------------
  
  print("querying the IR database to get data for DO sat calculations ")
  
  # Get DO IR_database to calculate percent sat --------
  
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  DOSatQry <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater]
WHERE   Char_Name = 'Dissolved oxygen saturation' AND 
MLocID in ({continuous_mon_locs*}) AND 
Statistical_Base = 'Mean'"
  
  Dosqry <- glue::glue_sql(DOSatQry, .con = con)
  DO_sat_AWQMS <- DBI::dbGetQuery(con, Dosqry)
  
  
  Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"
  
  
  
  Doqry <- glue::glue_sql(Doqry, .con = con)
  
  perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
  
  #Get temperature data from database
  
  tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND MLocID in ({continuous_mon_locs*})"
  
  tempqry <- glue::glue_sql(tempqry, .con = con)
  
  perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
  
  # Disconnect from database
  DBI::dbDisconnect(con)
  
  print("Finished database query")
  
  } else {
    # List of monitoring locations that need DO sat 
    # This list is used for the sql query that follows
    continuous_mon_locs <- unique(cont_perc_sat_check$AU_ID)
    
    
    # Get data from database --------------------------------------------------
    
    print("querying the IR database to get data for DO sat calculations ")
    
    # Get DO IR_database to calculate percent sat --------
    
    con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
    
    DOSatQry <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater]
WHERE   Char_Name = 'Dissolved oxygen saturation' AND 
AU_ID in ({continuous_mon_locs*}) AND 
Statistical_Base = 'Mean'"
    
    Dosqry <- glue::glue_sql(DOSatQry, .con = con)
    DO_sat_AWQMS <- DBI::dbGetQuery(con, Dosqry)
    
    
    Doqry <- "SELECT * 
FROM            VW_DO
WHERE        (Statistical_Base = 'Mean') AND AU_ID in ({continuous_mon_locs*})"
    
    
    
    Doqry <- glue::glue_sql(Doqry, .con = con)
    
    perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
    
    #Get temperature data from database
    
    tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        (Statistical_Base = 'Mean') AND AU_ID in ({continuous_mon_locs*})"
    
    tempqry <- glue::glue_sql(tempqry, .con = con)
    
    perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
    
    # Disconnect from database
    DBI::dbDisconnect(con)
    
    print("Finished database query")
    
    
  }
  # Join --------------------------------------------------------------------
  
  # Pare down table to be used in join
  perc_sat_temp_join <- perc_sat_temp %>%
    select(MLocID, IRResultNWQSunit, SampleStartDate, SampleStartTime, Statistical_Base) %>%
    rename(Temp_res = IRResultNWQSunit)
  
  
  
  perc_sat_DO <- perc_sat_DO %>%
    left_join(DO_sat_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  # Rename the result to DO_res and join with the temperature
  # Calculate DOsat
  DO_sat <- perc_sat_DO %>%
    rename(DO_res =  IRResultNWQSunit) %>%
    left_join(perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base')) %>%
    mutate(DO_sat = ifelse(is.na(DO_sat),DOSat_calc(DO_res, Temp_res, ELEV_Ft ), DO_sat),
           ma.DOS.mean30 = "") %>%
    mutate(DO_sat = ifelse(DO_sat > 100, 100, DO_sat )) |> 
    mutate(SampleStartTime = stringr::str_sub(SampleStartTime, start = 1, end=5),
           DO_sat = round(DO_sat,digits = 1))
  
  # calculate 30-D averages
  
  
  
  #Create list that will be used to get data out of the loop
  monloc_do_list <- list()
  
  #Set loop for each monitoring location
  print("Beginning DO sat Calculations")
  
  for(i in 1:length(unique(DO_sat$MLocID))){
    
    print(paste("Station", i, "of", length(unique(DO_sat$MLocID))))
    
    #Name of station be be used in this loop iteration
    station = unique(DO_sat$MLocID)[i]
    
    #Filter dataset to only look at 1 monitoring location at a time
    daydat_station <- DO_sat %>%
      filter(MLocID == station) %>%
      mutate(startdate30 = as.Date(SampleStartDate) -30) %>%
      arrange(SampleStartDate)
    
    # Begin 30-d moving averages loop
    print("Begin 30 day moving averages" )
    pb <- txtProgressBar(min = 0, max = nrow(daydat_station), style = 3)
    
    for(l in 1:nrow(daydat_station)){
      
      #Beginning of 30 day window
      start30 <- daydat_station$startdate30[l]
      # End of 30 day window
      end30 <- daydat_station$SampleStartDate[l] 
      
      
      # For each row in table, crate a new datatable for taht row plus all
      # Results that are in the 30 day window
      station_30day <- daydat_station %>%
        filter(SampleStartDate <= end30 & SampleStartDate >= start30) 
      
      
      # If there are at least 29 values in the 30 day window
      # Calculate the average DO-Sat
      # Otherwise use NA
      ma.mean30 <- ifelse(length(unique(station_30day$SampleStartDate)) >= 29, mean(station_30day$DO_sat), NA )
      
      
      # Pass the 30-d DO Sat vaule back into the single monitoring location table
      # the l >+ 30 prevents the 29th day being used. 
      daydat_station[l,"ma.DOS.mean30"] <- ifelse(l >= 30, round(ma.mean30, 2), NA)
      setTxtProgressBar(pb, l)
    } #end of 30day loop
    
    # Assign dataset filtered to 1 monitoring location to a list for combining outside of for loop
    monloc_do_list[[i]] <- daydat_station
    
  }
  
  print("Finished DO Sat Calculations")
  
  # Bind rows to get DO_sat averages
  
  DO_sat_avgs <-  bind_rows(monloc_do_list)  
  
  
  # Join DOsat to 30_D metrics -----------------------------------------------
  
  # Add Statistical_Base to the DO Sat table
  # Create Date field to be used for the join
  # The Activity start dates were slighly different causing problems
  # (1/1/1900 vs 1/1/1900 00:00)
  DO_sat_join <- DO_sat_avgs %>%
    mutate(Statistical_Base = "30DADMean",
           Date = as.Date(SampleStartDate)) %>%
    select(MLocID, ma.DOS.mean30, Date,Statistical_Base) 
  
  
  # Join DO Sat back into the original data table and recalculate violations
  yr_round_cont_DO_data_analysis_other <- continuous_data_analysis %>%
    mutate(Date = as.Date(SampleStartDate)) %>%
    left_join(DO_sat_join, by = c('MLocID', 'Date', 'Statistical_Base')) %>%
    mutate(Violation = case_when(DO_Class == "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D & 
                                   (ma.DOS.mean30 < 90 | is.na(ma.DOS.mean30)) ~ 1,
                                 DO_Class != "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~  1,
                                 Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi ~ 1,
                                 Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0))
  
  
  yr_round_cont_DO_data_analysis_other <- yr_round_cont_DO_data_analysis_other %>%
    rename(DO_sat = ma.DOS.mean30)
  
  
} else {
  
  yr_round_cont_DO_data_analysis_other <- continuous_data_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    mutate(Date = as.Date(SampleStartDate)) %>%
    mutate(Violation = case_when(DO_Class == "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~ 1,
                                 DO_Class != "Cold Water"& Statistical_Base == "30DADMean" & IRResultNWQSunit < crit_30D ~ 1,
                                 Statistical_Base == "7DADMin" & IRResultNWQSunit < crit_7Mi ~ 1,
                                 Statistical_Base == "Minimum" & IRResultNWQSunit < crit_Min ~ 1,
                                 TRUE ~ 0 ))
  
}


#datatfile = yr_round_cont_DO_data_analysis_other

yr_round_cont_data_categories_other <- yr_round_cont_DO_data_analysis_other %>%
  left_join(year_round_delist_eligability, relationship = "many-to-many") |> 
  group_by_at(group2) %>%
  summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
            Total_excursions = sum(Violation),
            Sum_30D_excursions = sum(Violation [Statistical_Base == "30DADMean"]),
            Sum_7mi_excursions = sum(Violation [Statistical_Base == "7DADMin"]),
            Sum_abs_min_excursions = sum(Violation [Statistical_Base == "Minimum"]),
            Delist_eligability = max(Delist_eligability)) %>%
  mutate(period = "year_round",
         IR_category = case_when(Sum_30D_excursions >= 2 ~ "5",
                                 Sum_7mi_excursions >= 2 ~ "5",
                                 Sum_abs_min_excursions >= 2 ~ "5",
                                 Sum_30D_excursions < 2 &
                                   Sum_7mi_excursions < 2 &
                                   Sum_abs_min_excursions < 2 ~ "2",
                                 TRUE ~ "ERROR"),
         Rationale = case_when(Sum_30D_excursions >= 2 ~ paste0("Impaired: ", Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_7mi_excursions >= 2 ~ paste0("Impaired: ", Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_abs_min_excursions >= 2 ~ paste0("Impaired: ", Sum_abs_min_excursions, " excursions of alternate minimum criteria. ",
                                                                    Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                    Sum_30D_excursions, " valid excursions of 30-D metric"),
                               Sum_30D_excursions < 2 &
                                 Sum_7mi_excursions < 2 &
                                 Sum_abs_min_excursions < 2 ~  paste0("Attaining: ",
                                                                      Sum_30D_excursions, " valid excursions of 30-D metric. ",
                                                                      Sum_7mi_excursions, " valid excursions of 7-mi metric. ",
                                                                      Sum_abs_min_excursions, " excursions of alternate minimum criteria. "
                                                                      ),
                               TRUE ~ "ERROR")) %>%
  mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE)) 


yr_round_cont_categories_class <- yr_round_cont_data_categories_other |>
  mutate(Rationale = paste0(DO_Class, ": ", Rationale)) |> 
  group_by_at(group3) |> 
  summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
            Total_excursions = sum(Total_excursions),
            Sum_30D_excursions = sum(Sum_30D_excursions),
            Sum_7mi_excursions = sum(Sum_7mi_excursions),
            Sum_abs_min_excursions = sum(Sum_abs_min_excursions),
            Delist_eligability = max(Delist_eligability),
            IR_category = max(IR_category),
            Rationale = str_c(Rationale,collapse =  " ~ " )
  ) 

           
yrround_cont_list <- list(data = as.data.frame(yr_round_cont_DO_data_analysis_other),
                          AU_categories = yr_round_cont_categories_class)
           

}

## Define year round instant function --------------------------------------------------------------------------

yr_round_inst_function <- function(df = Results_spawndates, continuous_list = results_cont_summary, AU_type){
  
  #Setting AU_type to 'other' will group the analysis by AU_ID and set the filter to discard WS units (inverse = TRUE)
  #Setting AU_type to 'WS' will group the analysis by AU_ID and MlocID, and set the filter to only keep WS units (inverse = FALSE)
  if(AU_type == "other"){  
    group1 <- c('AU_ID','Char_Name',  'DO_Class')
    group2 <- c('AU_ID','Char_Name', 'Pollu_ID', 'wqstd_code', 'DO_Class')
    group3 <- c("AU_ID", 'Char_Name','period', 'Pollu_ID', 'wqstd_code')
    inverse <- TRUE
    
    
  } else if (AU_type == "WS"){
    group1 <- c('AU_ID','Char_Name', 'MLocID', 'DO_Class')
    group2 <- c('AU_ID','Char_Name', 'MLocID','AU_GNIS_Name', 'GNIS_Name', 'Pollu_ID', 'wqstd_code',  'OWRD_Basin', 'DO_Class') 
    group3 <- c("AU_ID", 'Char_Name','MLocID','AU_GNIS_Name', 'GNIS_Name','period', 'Pollu_ID', 'wqstd_code')
    inverse <- FALSE
  }
  
  
  if(AU_type == "other"){ 
    
    instant_data_analysis <- Results_spawndates %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      filter(!AU_ID %in% continuous_list) %>%
      filter(Statistical_Base %in% c("Minimum", NA)) %>%
      mutate(Violation_crit = ifelse(IRResultNWQSunit < crit_Instant, 1, 0 ))
    
  } else if (AU_type == "WS"){
    
    instant_data_analysis <- Results_spawndates %>%
      filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
      filter(!MLocID %in% continuous_list) %>%
      filter(Statistical_Base %in% c("Minimum", NA)) %>%
      mutate(Violation_crit = ifelse(IRResultNWQSunit < crit_Instant, 1, 0 ))
    
  }
  
  
  instant_data_categories <- instant_data_analysis %>%
    group_by_at(group1) %>%
    summarise(num_samples = n(),
              num_critical_samples = sum(is.crit),
              num_below_crit = sum(Violation_crit)) %>%
    mutate(critical_excursions = binomial_excursions(num_samples, type = "Conventionals")) %>%
    mutate(IR_category = ifelse(num_critical_samples < 5 & 
                                  num_below_crit > 0, "Cat 3B", 
                                ifelse(num_critical_samples < 5 & 
                                         num_below_crit == 0, "Cat 3", 
                                       ifelse(num_critical_samples >= 5 &
                                                num_below_crit >= critical_excursions &
                                                DO_Class != "Cold Water", "Cat 5", 
                                              ifelse(num_critical_samples >= 5 &
                                                       num_below_crit >= critical_excursions &
                                                       DO_Class == "Cold Water", "Check percent Sat",
                                                     ifelse(num_critical_samples >= 5 &
                                                              num_below_crit <= critical_excursions, "Cat 2", "ERROR" ))))))
  
  
  # Data to be used to check percent saturation
  inst_perc_sat_check <- instant_data_analysis %>%
    filter(AU_ID %in% unique(subset(instant_data_categories, IR_category == "Check percent Sat" )$AU_ID) ) 
  
  # vector of monitoring locations to check DO saturdation. Used for database query
  instant_mon_locs <- unique(inst_perc_sat_check$MLocID)
  
  
  
  print("querying the IR database to get data for DO sat calculations ")
  
  # Get DO and temp data from IR_database to calculate percent sat --------
  con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")
  
  DOsat_AWQMS <- "SELECT [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base],[IRResultNWQSunit] as DO_sat
FROM [IntegratedReport].[dbo].[ResultsRawWater]
WHERE ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation') OR 
((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}) AND Char_Name = 'Dissolved oxygen saturation')"
  
  DOsat_from_AWQMS <-  glue::glue_sql(DOsat_AWQMS, .con = con)
  instant_perc_sat_AWQMS <- DBI::dbGetQuery(con, DOsat_from_AWQMS)
  
  # query DO data using instant_mon_locs as a monitoring location filter
  Doqry <- "SELECT * 
FROM            VW_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"
  
  
  
  Doqry <- glue::glue_sql(Doqry, .con = con)
  
  instant_perc_sat_DO <- DBI::dbGetQuery(con, Doqry)
  
  
  
  # query temp data using instant_mon_locs as a monitoring location filter
  
  tempqry <- "SELECT * 
FROM            VW_Temp_4_DO
WHERE        ((Statistical_Base = 'Minimum') AND MLocID in ({instant_mon_locs*})) OR  ((Statistical_Base IS NULL) AND MLocID in ({instant_mon_locs*}))"
  
  tempqry <- glue::glue_sql(tempqry, .con = con)
  
  instant_perc_sat_temp <-  DBI::dbGetQuery(con, tempqry)
  
  DBI::dbDisconnect(con)
  
  print("Finished database query")
  
  instant_perc_sat_temp_join <- instant_perc_sat_temp %>%
    select(MLocID, Statistical_Base, IRResultNWQSunit, SampleStartDate, SampleStartTime, act_depth_height
    ) %>%
    rename(Temp_res = IRResultNWQSunit)
  
  
  
  instant_perc_sat_DO <- instant_perc_sat_DO %>%
    left_join(instant_perc_sat_AWQMS, by =c('MLocID', 'SampleStartDate','SampleStartTime','Statistical_Base'  ))
  
  # Join DO and temp tables and calculate DO-Sat
  instant_DO_sat <- instant_perc_sat_DO %>%
    rename(DO_res =  IRResultNWQSunit) %>%
    left_join(instant_perc_sat_temp_join, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base', 'act_depth_height')) %>%
    mutate(DO_sat = ifelse(is.na(DO_sat), DOSat_calc(DO_res, Temp_res, ELEV_Ft ),DO_sat))  %>%
    mutate(DO_sat = ifelse(DO_sat > 100, 100, DO_sat )) %>%
    select(MLocID, SampleStartDate, SampleStartTime, Statistical_Base, act_depth_height,DO_sat ) %>%
    mutate(SampleStartDate = as.Date(parse_date_time(SampleStartDate, c("mdy", "ymd")))) |> 
    group_by(MLocID, SampleStartDate, SampleStartTime , Statistical_Base, act_depth_height) |> 
    filter(row_number() == 1) |> 
    mutate(SampleStartTime = stringr::str_sub(SampleStartTime, start = 1, end=5),
           DO_sat = round(DO_sat,digits = 1))
  
  
  #DATA
  
  #Join back in and recalculate violations
  # if do sat could not be calculated, then violation if IRResultNWQSunit < 30D criteria
  Instant_data_analysis_DOS <- instant_data_analysis %>%
    filter(str_detect(AU_ID, "WS", negate = inverse)) %>%
    mutate(act_depth_height = as.character(act_depth_height)) %>%
    filter(!AU_ID %in% results_cont_summary) %>%
    filter(Statistical_Base %in% c("Minimum", NA)) %>%
    left_join(instant_DO_sat, by = c('MLocID', 'SampleStartDate', 'SampleStartTime', 'Statistical_Base', 'act_depth_height')) %>%
    mutate(Violation = case_when(DO_Class == "Cold Water" & IRResultNWQSunit < crit_Instant & (DO_sat < 90.0 | is.na(DO_sat)) ~ 1,
                                 DO_Class != "Cold Water" & IRResultNWQSunit < crit_30D ~ 1,
                                 TRUE ~ 0))
  
  
  
  # Reassign categories based on flow charts
  yr_round_instant_categories <- Instant_data_analysis_DOS %>%
    left_join(year_round_delist_eligability, relationship = "many-to-many") |> 
    group_by_at(group2) %>%
    summarise(stations =  stringr::str_c(unique(MLocID), collapse = "; "),
              OWRD_Basin = first(OWRD_Basin), 
              num_samples = n(),
              num_critical_samples = sum(is.crit),
              num_excursions = sum(Violation, na.rm = TRUE),
              Delist_eligability = max(Delist_eligability)) %>%
    mutate(period = "year_round") %>%
    mutate(critical_excursions =  binomial_excursions(num_samples, type = "Conventionals")) %>%
    mutate(IR_category = case_when(num_critical_samples < req_inst_crit_samples & num_excursions > 0 ~ "3B",
                                   num_critical_samples < req_inst_crit_samples & num_excursions == 0 ~ "3",
                                   num_critical_samples >= req_inst_crit_samples & num_excursions >= critical_excursions ~ "5",
                                   num_critical_samples >= req_inst_crit_samples & num_excursions < critical_excursions ~ "2",
                                   TRUE ~ "ERROR"),
           Rationale = case_when(num_critical_samples < req_inst_crit_samples & num_excursions > 0 ~ paste0("Insufficient data: ", num_critical_samples, 
                                                                                                            " samples in critical period is < 8 required. ",
                                                                                                            num_excursions, " total excursions. - ",
                                                                                                            num_samples, ' total samples.'),
                                 num_critical_samples < req_inst_crit_samples & num_excursions == 0 ~ paste0("Insufficient data: ", num_critical_samples, 
                                                                                                             " samples in critical period is < 8 required. ",
                                                                                                             num_excursions, " total excursions. - ",
                                                                                                             num_samples, ' total samples.'),
                                 num_critical_samples >= req_inst_crit_samples & num_excursions >= critical_excursions ~ paste0("Impaired: ", num_excursions, " excursions of criteria. ",
                                                                                                                                critical_excursions, " needed to list. - ",
                                                                                                                                num_samples, ' total samples.'),
                                 num_critical_samples >= req_inst_crit_samples & num_excursions < critical_excursions ~ paste0("Attaining: ", num_excursions, " excursions of criteria. ",
                                                                                                                               critical_excursions, " needed to list. - ",
                                                                                                                               num_samples, ' total samples.'),
                                 TRUE ~ "ERROR"))%>%
    mutate(IR_category = factor(IR_category, levels=c("3", "3B", "2", "5" ), ordered=TRUE))
  
  
  yr_round_instant_categories_class <- yr_round_instant_categories |>
    mutate(Rationale = paste0(DO_Class, ": ", Rationale)) |> 
    group_by_at(group3) |> 
    summarise(stations =  stringr::str_c(unique(stations), collapse = "; "),
              num_samples = sum(num_samples),
              num_critical_samples = sum(num_critical_samples),
              num_excursions = sum(num_excursions),
              critical_excursions = sum(critical_excursions),
              Delist_eligability = max(Delist_eligability),
              IR_category = max(IR_category),
              Rationale = str_c(Rationale,collapse =  " ~ " )
              ) 
  
  
  year_rd_inst_list <- list(data = Instant_data_analysis_DOS,
                            categories =yr_round_instant_categories_class )
  
  
  return(year_rd_inst_list)
  
}

# Run year round continuous function ---------------------------------------------------------------------------

print("Beginning continuous analysis")


## year round continuous- other ------------------------------------------------------------------------------------


year_round_cont_other <- yr_round_cont_function(df, AU_type = "other")


year_round_cont_other_data <- year_round_cont_other[['data']]

year_round_cont_other_categories <- year_round_cont_other[['AU_categories']] %>%
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID), "-",Pollu_ID,"-", wqstd_code,"-", period ))

# other_category_cont <- join_prev_assessments(year_round_cont_other_categories, AU_type = 'Other')
# 
# other_category_delist_cont <-  assess_delist(other_category_cont, type = "Other") |> 
#   mutate(Char_Name = 'Dissolved oxygen (DO)') 



## Year round continuous- WS ---------------------------------------------------------------------------------------

print("Begin year round continuous- WS")
year_round_cont_WS <- yr_round_cont_function(df, continuous_list = results_cont_summary_WS, AU_type = "WS")
year_round_cont_WS_data <- year_round_cont_WS[['data']]
WS_categories_cont <- year_round_cont_WS[['AU_categories']] %>%
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID), "-",Pollu_ID,"-", wqstd_code,"-", period ))



WS_GNIS_rollup_cont <- WS_categories_cont %>%
  mutate(Char_Name = 'Dissolved oxygen (DO)') |> 
  mutate(Rationale = paste0(MLocID, ": ", Rationale)) |> 
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name,Char_Name, stations, Pollu_ID, wqstd_code, period) %>%
  summarise(IR_category_GNIS_26 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  




# Run year round instant function ---------------------------------------------------------------------------

print("Beginning instantaneous analysis")


## Run the year round instant function on other units -------------------------------------------------------------


year_round_inst_other <- yr_round_inst_function(df = Results_spawndates, AU_type = "other")

year_round_inst_other_data <- year_round_inst_other[['data']]

year_round_inst_other_categories <- year_round_inst_other[['categories']] %>%
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID), "-",Pollu_ID,"-", wqstd_code,"-", period ))

# 
# other_category_inst <- join_prev_assessments(year_round_inst_other_categories, AU_type = 'Other') 
# 
# other_category_delist_inst <-  assess_delist(other_category_inst, type = "Other")

## Run the year round instant function on WS units -----------------------------------------------------------

year_round_inst_WS <- yr_round_inst_function(df, continuous_list = results_cont_summary_WS, AU_type = "WS")
year_round_inst_WS_data <- year_round_inst_WS[['data']]
WS_categories_inst <- year_round_inst_WS[['categories']]%>%
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID), "-",Pollu_ID,"-", wqstd_code,"-", period ))



WS_GNIS_rollup_inst <- WS_categories_inst %>%
  mutate(Char_Name = 'Dissolved oxygen (DO)') |> 
  mutate(Rationale = paste0(MLocID, ": ", Rationale)) |> 
  ungroup() %>%
  group_by(AU_ID, AU_GNIS_Name,Char_Name, Pollu_ID, wqstd_code, period, stations) %>%
  summarise(IR_category_GNIS_26 = max(IR_category),
            Rationale_GNIS = str_c(Rationale,collapse =  " ~ " ),
            Delist_eligability = max(Delist_eligability)) %>% 
  mutate(Delist_eligability = case_when(Delist_eligability == 1 & IR_category_GNIS_26 == '2'~ 1,
                                        TRUE ~ 0)) |> 
  mutate(IR_category_GNIS_26 = factor(IR_category_GNIS_26, levels=c('Unassessed', "3", "3B", "2", "5" ), ordered=TRUE)) |> 
  mutate(recordID = paste0("2026-",odeqIRtools::unique_AU(AU_ID),"-", Pollu_ID, "-", wqstd_code,"-", period ))  

# WS_GNIS_rollup_inst <- join_prev_assessments(WS_GNIS_rollup_inst, AU_type = "WS") |> 
#   mutate(Char_Name = 'Dissolved oxygen (DO)') 
### Delist process --------------------------------------------------------------------------------------------------

# 
# WS_GNIS_rollup_delist_inst <- assess_delist(WS_GNIS_rollup_inst, type = 'WS')


## AU Rollup -------------------------------------------------------------------------------------------------------

# 
# WS_AU_rollup_inst <- rollup_WS_AU(WS_GNIS_rollup_delist_inst, char_name_field = Char_Name)






# Data combine ----------------------------------------------------------------------------------------------------

## Combine assessment data ----------------------------------------------------------------------------------------

# Combine mloc ----------------------------------------------------------------------------------------------------

WS_categories <- bind_rows(WS_categories_cont, WS_categories_inst)


# Combine GNIS ----------------------------------------------------------------------------------------------------

WS_GNIS_rollup <- bind_rows(WS_GNIS_rollup_cont, WS_GNIS_rollup_inst)
WS_GNIS_rollup <- join_prev_assessments(WS_GNIS_rollup, AU_type = "WS")


## Delist process --------------------------------------------------------------------------------------------------


WS_GNIS_rollup_delist <- assess_delist(WS_GNIS_rollup, type = 'WS') |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')


## AU Rollup -------------------------------------------------------------------------------------------------------


WS_AU_rollup <- rollup_WS_AU(WS_GNIS_rollup_delist, char_name_field = Char_Name)

WS_AU_rollup_joined <- WS_AU_prev_list(WS_AU_rollup) 

# Combine AU decisions --------------------------------------------------------------------------------------------
Other_categories <- bind_rows(year_round_inst_other_categories, year_round_cont_other_categories)
other_category <- join_prev_assessments(Other_categories, AU_type = 'Other')

other_category_delist <-  assess_delist(other_category, type = "Other")

# prep data for export --------------------------------------------------------------------------------------------

AU_display_other <- other_category_delist |> 
  select(AU_ID, Char_Name,  Pollu_ID, wqstd_code, period, prev_category, prev_rationale,
         final_AU_cat, Rationale, stations, recordID, status_change, Year_listed,  year_last_assessed)

AU_display_ws <- WS_AU_rollup_joined |> 
  rename(prev_category = prev_AU_category,
         prev_rationale = prev_AU_rationale,
         final_AU_cat = IR_category_AU_26,
         Rationale = Rationale_AU)

AU_display <- bind_rows(AU_display_other, AU_display_ws) |> 
  mutate(Rationale = case_when(is.na(Rationale) ~ prev_rationale,
                               .default = Rationale))|> 
  join_TMDL(type = 'AU')|> 
  join_AU_info() |> 
  relocate(prev_category, .after = year_last_assessed) |> 
  relocate(prev_rationale, .after = prev_category) |> 
  mutate(year_last_assessed = case_when(status_change != 'No change in status- No new assessment' ~ "2026",
                                        TRUE ~ year_last_assessed)) |> 
  mutate(Year_listed = case_when(final_AU_cat %in% c("5", '4A') & is.na(Year_listed) ~ '2026',
                                 TRUE ~  Year_listed))  |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')



WS_GNIS_rollup_delist <- WS_GNIS_rollup_delist |> 
  join_TMDL(type = 'GNIS') |> 
  join_AU_info()|> 
  relocate(Rationale_GNIS, .after = final_GNIS_cat) |> 
  relocate(prev_GNIS_category, .after = Rationale_GNIS) |> 
  relocate(prev_GNIS_rationale, .after = prev_GNIS_category)  |> 
  mutate(Char_Name = 'Dissolved oxygen (DO)')


Yr_Rnd_cont_data <- bind_rows(year_round_cont_other_data, year_round_cont_WS_data)
Yr_Rnd_inst_data <- bind_rows(year_round_inst_other_data, year_round_inst_WS_data)

if(write_excel){
  
  
  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "AU_Decisions", tabColour = 'forestgreen')
  
  addWorksheet(wb, sheetName = "Other_AU_categorization",tabColour = 'dodgerblue3')
  addWorksheet(wb, sheetName = "WS station categorization", tabColour = 'lightblue3')
  addWorksheet(wb, sheetName = "WS GNIS categorization", tabColour = 'lightyellow1')
  
  addWorksheet(wb, sheetName = "DO Year Round Data Inst",     tabColour = 'paleturquoise2')
  addWorksheet(wb, sheetName = "DO Year Round Data Cont",     tabColour = 'paleturquoise2')

  
  
  header_st <- createStyle(textDecoration = "Bold", border = "Bottom")
  
  writeData(wb = wb, sheet = "AU_Decisions", x = AU_display, headerStyle = header_st)
  
  writeData(wb = wb, sheet = "Other_AU_categorization", x = other_category_delist, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS station categorization", x = WS_categories, headerStyle = header_st)
  writeData(wb = wb, sheet = "WS GNIS categorization", x = WS_GNIS_rollup_delist, headerStyle = header_st)
  
  
  writeData(wb = wb, sheet = "DO Year Round Data Inst",   x = Yr_Rnd_inst_data, headerStyle = header_st )
  writeData(wb = wb, sheet = "DO Year Round Data Cont",   x = Yr_Rnd_cont_data, headerStyle = header_st )

  
  print("Writing excel doc")
  saveWorkbook(wb, paste0("Parameters/Outputs/DO Yearround-",Sys.Date(), ".xlsx"), overwrite = TRUE) 
  
}


DO_year_round <- list(AU_display = AU_display,
                      Other_AU_categorization = other_category_delist,
                      WS_GNIS_categorization = as.data.frame(WS_GNIS_rollup_delist),
                      WS_station_categorization = WS_categories,
                      cont_data = Yr_Rnd_cont_data,
                      inst_data = Yr_Rnd_inst_data)

return(DO_year_round)

}