


yr_rnd_DO_delist_checker <- function(DO_data = Results_spawndates){

# Full critical period --------------------------------------------------------------------------------------------



delist_full <- DO_data %>%
  filter(Statistical_Base == "Minimum") %>%
  mutate(spawn_length = SpawnEnd - SpawnStart + 1,
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate)) %>%
  group_by(AU_ID, MLocID, Statistical_Base, year, spawn_length, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  mutate(meets_yer_round_delisting = case_when(num_critical_days >= critical_length*.8 ~ 1,
                                               TRUE ~ 0),
         meets_spawn_delisting = case_when(num_spawn_days >= spawn_length*.8 ~ 1,
                                           TRUE ~ 0)) %>%
  group_by(AU_ID, MLocID) %>%
  summarise(years_critrical = sum(meets_yer_round_delisting),
            years_spawning = sum(meets_spawn_delisting)) |> 
  filter(years_critrical >= 3) |> 
  mutate(Delist_eligability = 1,
         delist_type = "Full season deployment") |> 
  ungroup() |> 
  select(MLocID, Delist_eligability, delist_type)





# Short Term deployment option ------------------------------------------------------------------------------------



test_func <- function(df){
  
  df2 <- df |> 
    group_by(consec_day_set = cumsum(c(TRUE, diff(SampleStartDate) > 1))) %>% 
    ungroup() 
  
}


#This will assess the conidtion of 5 consecutive days in critical period months
delist_short <- DO_data |> 
  filter(Statistical_Base == 'Minimum') |> 
  filter(is.crit == 1) |> 
  mutate(year = year(SampleStartDate),
         month = month(SampleStartDate)) |> 
  arrange(MLocID, SampleStartDate) |> 
  group_by(MLocID,year, month) |> 
  group_modify(~ test_func(.x)) |> 
  bind_rows() |> 
  select(MLocID,year, month, SampleStartDate, consec_day_set) |> 
  ungroup() |> 
  group_by(MLocID,year, month,consec_day_set) |> 
  summarise(num_consec_days = n_distinct(SampleStartDate)) |> 
  filter(num_consec_days> 5,
         month %in% c(7,8,9)) |> 
  ungroup() |> 
  group_by(MLocID, year) |> 
  summarise(month_meeting = n_distinct(month)) |> 
  filter(month_meeting >= 3) |> 
  group_by(MLocID) |> 
  summarise(year_meeting = n_distinct(year)) |> 
  filter(year_meeting >= 3) |> 
  mutate(Delist_eligability = 1,
         delist_type = "short term deployment") |> 
  select(MLocID, Delist_eligability, delist_type) |> 
  filter(!(MLocID %in% delist_full$MLocID))
  


# Grab samples ----------------------------------------------------------------------------------------------------

delist_grab <- DO_data |> 
  filter(is.na(Statistical_Base)) %>%
  mutate(spawn_length = SpawnEnd - SpawnStart + 1,
         is.crit = ifelse(SampleStartDate >= critstart & SampleStartDate <= critend, 1, 0 ),
         critical_length = critend - critstart + 1,
         year = year(SampleStartDate),
         month = month(SampleStartDate)) %>%
  filter(month %in% c(7,8,9)) |> 
  group_by(AU_ID, MLocID, Statistical_Base, year, month, spawn_length, critical_length) %>%
  summarise(num_days = n_distinct(SampleStartDate),
            num_critical_days = n_distinct(SampleStartDate[is.crit == 1]),
            num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
  mutate(month_meeting_yr = case_when(num_critical_days >= 2 ~ 1,
                                               TRUE ~ 0),
         month_meeting_spawn = case_when(num_spawn_days >= 2 ~ 1,
                                           TRUE ~ 0)) %>%
  filter(month_meeting_yr == 1) |> 
  group_by(AU_ID, year, MLocID) %>%
  summarise(month_meeting = n_distinct(month)) |> 
  filter(month_meeting >= 3) |> 
  group_by(MLocID) |> 
  summarise(year_meeting = n_distinct(year)) |> 
  filter(year_meeting >= 3) |> 
  mutate(Delist_eligability = 1,
         delist_type = "Grab Method") |> 
  select(MLocID, Delist_eligability, delist_type) |> 
  filter(!(MLocID %in% delist_full$MLocID)) |> 
  filter(!(MLocID %in% delist_short$MLocID))


# Put together ----------------------------------------------------------------------------------------------------

delist_eligability <- bind_rows(delist_full, delist_short, delist_grab)

return(delist_eligability)

}








spawn_DO_delist_checker <- function(DO_data = Results_spawndates){
  
  # DO_data = Results_spawndates
  
  # Full critical period --------------------------------------------------------------------------------------------
  
  
  
  delist_full <- DO_data %>%
    filter(Statistical_Base == "Minimum") %>%
    mutate(spawn_length = SpawnEnd - SpawnStart + 1,
           year = SpawnStart) |> 
    group_by(AU_ID, MLocID, Statistical_Base, year, spawn_length) %>%
    summarise(num_days = n_distinct(SampleStartDate),
              num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
    mutate(meets_spawn_delisting = case_when(num_spawn_days >= spawn_length*.8 ~ 1,
                                             TRUE ~ 0)) %>%
    group_by(AU_ID, MLocID) %>%
    summarise(years_spawning = sum(meets_spawn_delisting)) |> 
    filter(years_spawning >= 3) |> 
    mutate(Delist_eligability = 1,
           delist_type = "Full season deployment") |> 
    ungroup() |> 
    select(MLocID, Delist_eligability, delist_type)
  
  
  
  
  
  # Short Term deployment option ------------------------------------------------------------------------------------
  
  
  
  test_func <- function(df){
    
    df2 <- df |> 
      group_by(consec_day_set = cumsum(c(TRUE, diff(SampleStartDate) > 1))) %>% 
      ungroup() 
    
  }
  
  
  #This will assess the conidtion of 5 consecutive days in critical period months
  delist_short <- DO_data |> 
    filter(Statistical_Base == 'Minimum') |> 
    filter(in_spawn == 1) |> 
    mutate(year = SpawnStart,
           month = month(SampleStartDate)) |> 
    arrange(MLocID, SampleStartDate) |> 
    group_by(MLocID,year, month, SpawnEnd) |> 
    group_modify(~ test_func(.x)) |> 
    bind_rows() |> 
    select(MLocID,year, month,SpawnEnd, SampleStartDate, consec_day_set) |> 
    ungroup() |> 
    group_by(MLocID,year, month,SpawnEnd, consec_day_set) |> 
    summarise(num_consec_days = n_distinct(SampleStartDate)) |> 
    filter(num_consec_days> 5) |> 
    ungroup() |> 
    group_by(MLocID, year, SpawnEnd) |> 
    summarise(month_meeting = n_distinct(month)) |> 
    mutate(spawn_months = interval(year, SpawnEnd) %/% months(1)) |> 
    filter(month_meeting >= spawn_months) |> 
    group_by(MLocID) |> 
    summarise(year_meeting = n_distinct(year)) |> 
    filter(year_meeting >= 3) |> 
    mutate(Delist_eligability = 1,
           delist_type = "short term deployment") |> 
    select(MLocID, Delist_eligability, delist_type) |> 
    filter(!(MLocID %in% delist_full$MLocID))
  
  
  
  # Grab samples ----------------------------------------------------------------------------------------------------
  
  delist_grab <- DO_data |> 
    filter(is.na(Statistical_Base)) %>%
    mutate(spawn_length = SpawnEnd - SpawnStart + 1,
           year = SpawnStart,
           month = month(SampleStartDate),
           spawn_interval = interval(SpawnStart,SpawnEnd),
           spawn_months = spawn_interval %/% months(1)) %>%
    filter(in_spawn == 1) |> 
    group_by(AU_ID, MLocID, Statistical_Base, year, month, spawn_length, spawn_months) %>%
    summarise(num_days = n_distinct(SampleStartDate),
              num_spawn_days = n_distinct(SampleStartDate[in_spawn == 1])) %>%
    mutate(month_meeting_spawn = case_when(num_spawn_days >= 2 ~ 1,
                                           TRUE ~ 0)) %>%
    filter(month_meeting_spawn == 1) |> 
    group_by(AU_ID, year, MLocID, spawn_months) %>%
    summarise(month_meeting = n_distinct(month)) |> 
    filter(month_meeting >= spawn_months) |> 
    group_by(MLocID) |> 
    summarise(year_meeting = n_distinct(year)) |> 
    filter(year_meeting >= 3) |> 
    mutate(Delist_eligability = 1,
           delist_type = "Grab Method") |> 
    select(MLocID, Delist_eligability, delist_type) |> 
    filter(!(MLocID %in% delist_full$MLocID)) |> 
    filter(!(MLocID %in% delist_short$MLocID))
  
  
  # Put together ----------------------------------------------------------------------------------------------------
  
  delist_eligability <- bind_rows(delist_full, delist_short, delist_grab)
  
  return(delist_eligability)
  
}



