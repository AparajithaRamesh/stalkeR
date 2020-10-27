# In this script I will quantify the number of *crossing events* each individuals is making.
# Trick from https://stackoverflow.com/questions/30516289/how-to-detect-a-gap-in-series-data-using-r


# Packages
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(data.table)
  library(patchwork)

## 1. Data import and manipulation

  # Import data
  df <- read_delim("20201008.CSV", ";", escape_double = FALSE, trim_ws = TRUE)

# Naming my columns
  names(df) <- c( "Identifier", "Date", "Time",
                  "Unit.number", "Antenna.number", "Transponder.type",
                  "Transponder.code", "Weight", "Input.status",
                  "Output.status", "Event", "GPS.coordinates")
  
# Defining my variables
  df$Identifier <- as.integer(df$Identifier)
  df$Time <- as.character(df$Time) #this needs to be changed while reading the file
  df$Unit.number <- as.integer(df$Unit.number)
  df$Transponder.code <- as.character(df$Transponder.code)
  df$Actual_time <- dmy_hms(paste(df$Date,df$Time,sep=" "))
  
# I make a new df with a subset of the variables of interest here
  new_dataset<-subset(df, select=c(Identifier, Actual_time, Unit.number, Transponder.code))
  names(new_dataset) <- c("Identifier", "time", "antenna", "id")
  
# I split my dataframe into a list of dataframes (one object per individual)
  df_list <- split(new_dataset, f = new_dataset$id)
  nb_ind <- length(df_list)
 
  
  
## 2. I assing each read to a READING SERIES, that is, a group of uninturrepted reads following each other.
# For example, if you have: 00:00, 00:01, 00:02, 00:03, 00:17, 00:18, 00:19, you end up with the four first
# reads belonging to group 1 and the three last reads belonging to group 2.
  
  # Individual loop - I run the code below for all my individuals
  for (i in 1:nb_ind){
    
  # If an individual was read several times during the same second, I only keep one read (the first).
    df_list[[i]] <- subset(df_list[[i]], !duplicated(time))
    
  # I create the 'gap' column. In every row, the time gap between the focal read/row and the previous one is computed.
    df_list[[i]]$gap <- c(NA, with(df_list[[i]], time[-1] - time[-nrow(df_list[[i]])]))
    
  # I define the size of the time gap (in sec) to separate two different READING SERIES.
    gap_threshold <- 1
    
  # I create a new column (i.e. over_threshold) indicating if the observed gap is bigger (TRUE) or
  # or smaller (FALSE) than the gap_threshold
    df_list[[i]]$over_thresh <- df_list[[i]]$gap > gap_threshold
    
  # I assign every read to a reading series (that is, a group of uninturrepted reads) based on the 'over_threshold' column
    for (a in 1:nrow(df_list[[i]])){
      df_list[[i]]$reading_series[a] <- nrow(filter(df_list[[i]][1:a,], over_thresh == "TRUE"))
      
    } # end of the reading series loop
    } # end of individual loop
  
  
  
  
# 4. Now, I want to know how long each individual spends 
  
  View(df_list[[1]])
  

  
  
  ## 3. I now want, in addition to the number of reading series per individual, have the number of CROSSING EVENTS.
# Here, I chose a duration threshold, for which we can consider that a reading series is composed of more than one
# reading event. For example, let's say that the duration threshold if 5 seconds. Then, if I have a reading series of
# 15 uninturrepted seconds, it represents 3 crossing events.
# 14 uninturrepted seconds <- 3 reading events
# 16 uninturrepted seconds <- 4 reading events
  
duration_threshold <- 5
  
  ceiling(nrow(df[1:11,])/duration_threshold)
  
  ceiling(nrow(df[1:4,])/3) 
  
  
  
  

  
  ################ Alternative in building phase - I just make a function for ONE indiviual and ONE antenna#####
  
  # I randomly pick one fish and one antenna for testing the function
  DF <- df_list[[7]]
  DF <- split(DF, f = DF$antenna)
  DF <- DF[[3]]
  
  gap_threshold <- 1
  event_duration <- 3
  
  nb.events <- function(DF, gap_threshold, event_duration){
  # If an individual was read several times during the same second, I only keep one read (the first).
  DF <- subset(DF, !duplicated(time))
  
  # I create the 'gap' column. In every row, the time gap between the focal read/row and the previous one is computed.
  DF$gap <- c(NA, with(DF, time[-1] - time[-nrow(DF)]))
  
  
  # I create a new column (i.e. over_threshold) indicating if the observed gap is bigger (TRUE) or
  # or smaller (FALSE) than the gap_threshold
  DF$over_thresh <- DF$gap > gap_threshold
  
  # I assign every read to a reading series (that is, a group of uninturrepted reads) based on the 'over_threshold' column
  for (a in 1:nrow(DF)){
    DF$reading_series[a] <- nrow(filter(DF[1:a,], over_thresh == "TRUE"))
  }
  
  
  
  # I split my daraframe based on the number of reading series
    DF2 <- split(DF, f = DF$reading_series)
  
  # I define my objects
    events <- c()
    time <- c()
  
# For each reading series, I extract (i) the number of events, (ii) the duration spent sitting on the antenna.
  for(i in 1:length(DF2)){
  # I round up to the next unit
    events[i] <- ceiling(nrow(DF2[[i]])/event_duration)
  # I calculate the duration of each reading series
    time[i] <- difftime(tail(DF2[[i]]$time,1)  , DF2[[i]]$time[1], units = "s")
  }
# I sum the number of events and duration of all the reading series
    nb_events <- sum(events)
    tot_time <- sum(time)
    
# I obtain vector containing the the individual id, the antenna number, the number of reading series (i.e. visits),
# the duration spent at this antenna, the number of 'crossing events'.
  result <- data.frame(id = DF$id[1], 
              antenna = DF$antenna[1],
              reading_series = max(DF$reading_series)+1, 
              duration = tot_time,
              nb_events = nb_events)
  
  return(result)
  }

  
  DF <- df_list[[7]]
  DF <- split(DF, f = DF$antenna)
  DF <- DF[[4]]
  nb.events(DF, 1, 3)
  
  
  