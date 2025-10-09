spawn_crit_preiods <- data.frame(
        stringsAsFactors = FALSE,
               SpawnCode = c(12L,13L,14L,15L,
                             16L,18L,19L,20L,21L,22L,23L,24L,25L,27L,
                             28L,29L,30L),
             Spawn.Dates = c("September 1 - May 15","September 1 - June 15",
                             "September 15 - May 15","September 15 - June 15",
                             "October 23 - April 15","October 1 - June 15","October 15 - May 15",
                             "October 15 - June 15","November 1 - May 15",
                             "November 1 - May 1","November 1 - June 15",
                             "January 1 - May 15","January 1 - June 15",
                             "August 1 - June 15","August 15 - June 15","August 15 - May 15",
                             "October 15 - March 31"),
     Total.Spawning.Days = c(257L,288L,242L,
                             273L,174L,257L,212L,243L,195L,181L,226L,134L,
                             165L,318L,305L,273L,167L),
    Fall.Critical.Period = c("9/01 - 11/30",
                             "9/01 - 11/30","9/15 - 11/30","9/15 - 11/30",
                             "10/23 - 11/30","10/01 - 11/30","10/15 - 11/30",
                             "10/15 - 11/30","11/01 - 11/30","11/01 - 11/30",
                             "11/01 - 11/30",NA_character_,NA_character_,"8/01 - 11/30","8/15 - 11/30",
                             "8/15 - 11/30","10/15 - 11/30"),
  Spring.Critical.Period = c("4/01 - 5/15",
                             "4/01 - 6/15","4/01 - 5/15","4/01 - 6/15",
                             "4/01 - 4/15","4/01 - 6/15","4/01 - 5/15","4/01 - 6/15",
                             "4/01 - 5/15","4/01 - 5/01","4/01 - 6/15",
                             "4/01 - 5/15","4/01 - 6/15","4/01 - 6/15","4/01 - 6/15",
                             "4/01 - 5/15",NA_character_)
)

test <- ws_3_year |> 
  filter(AU_ID == 'OR_WS_170501160201_05_102976')






