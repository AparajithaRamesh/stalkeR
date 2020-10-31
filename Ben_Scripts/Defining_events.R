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
  new_dataset<-subset(df, select=c(Actual_time, Unit.number, Transponder.code))
  names(new_dataset) <- c("time", "antenna", "id")
  
# I split my dataframe into a list of dataframes (one object per individual)
  df_list <- split(new_dataset, f = new_dataset$id)
  nb_ind <- length(df_list)
 


# I define my objects
  nb_antennas <- length(unique(new_dataset$antenna))  # Number of antennas
  nb_individuals <- length(unique(new_dataset$id))    # Number of individuals
  gap_threshold <- 1                                  # The size of the time gap (in sec) to separate two different READING SERIES.
  event_duration <- 3                                 # The duration of one event
  
  
  
  # Function n°1
  # Input = A dataframe containing all the reads for one individual at one antenna.
  # Output = Number of reading series, duration spent sititng in the antenna, and number of 
  # crossing events for this individual at this antenna (one row dataframe).
  nb.events.ind.ant <- function(DF){
    if (nrow(DF) != 0){
      
# If an individual was read several times during the same second, I only keep one read (the first).
  DF <- subset(DF, !duplicated(time))
  
# I create the 'gap' column. In every row, the time gap between the focal read/row and the previous one is computed.
  DF$gap <- c(NA, with(DF, time[-1] - time[-nrow(DF)]))
  
# I create a new column (i.e. over_threshold) indicating if the observed gap is bigger (TRUE) or
# or smaller (FALSE) than the gap_threshold
  DF$over_thresh <- DF$gap > gap_threshold
  
# I assign every read to a reading series (that is, a group of uninturrepted reads) based on the 'over_threshold' column
  for (a in 1:nrow(DF)){
    DF$reading_series[a] <- nrow(filter(DF[1:a,], over_thresh == "TRUE")) + 1
    
  }
  
  # I split my daraframe based on the number of reading series
    DF2 <- split(DF, f = DF$reading_series)
  
  # I define my objects
    events <- c()
    time <- c()

# For each reading series, I extract (i) the number of events, (ii) the duration spent sitting on the antenna.
  for(i in 1:length(DF2)){
  
  # An event is either a simple cross (without repeats), or an individual sitting in an antenna for more than
  # a certain amount of time (e.g. 3 seconds). For instance, if an individual that passes by an antenna once, then
  # after some time, comes back to the antenna and sits there for 6s, it would represent 3 events.
  # Events are define rounding up to the unit above (if an event is 3s, 2s = 1 event, 4s = 2 events, 7s = 3 events, etc.)
    events[i] <- ceiling(nrow(DF2[[i]])/event_duration)
  
    # I also calculate the duration of each reading series. When only one read = 1 sec.
    # If a reading series starts at 00:00 and ends at 00:03, it counts as 4 seconds and not 3.
    time[i] <- (difftime(tail(DF2[[i]]$time,1), DF2[[i]]$time[1], units = "s") + 1)
  }
    
# I sum the number of events and duration of all the reading series (what I obtain oreviously is a vector, with
# each object corresponding to a reading series)
    nb_events <- sum(events)
    tot_time <- sum(time)
    
# I obtain a df containing the individual id, the antenna number, the number of reading series (i.e. visits),
# the duration spent at this antenna, the number of 'crossing events'.
  result <- data.frame(id = DF$id[1], 
              antenna = DF$antenna[1],
              reading_ser = max(DF$reading_series), 
              duration = tot_time,
              nb_ev = nb_events)
  
    
  return(result)} # end of if
  }

  # Example to run function n°1:
  nb.events.ind.ant(subset(df_list[[1]], df_list[[1]]$antenna == 13))
  
  

  
  
  
  
  # Function n°2
  # Input 1 = All the reads (at all antennas) for one individual (data frame)
  # Input 2 = The minimal duration for which two sequential reads belong two reading series. 
  # E.g., if 3 sec, then c(00:01, 00:02, 00:05, 00:06) represents one reading series, 
  # but c(00:01, 00:02, 00:07, 00:08) represents two.
  # Input 3 = the crossing event duration. Not sure I'll keep this part.
  # Output = Number of reading series, duration spent sititng in the antenna, and number of 
  # crossing events for this individual at ALL antennas (one row dataframe).
  
  nb.events.ind <- function(DF_ind,        # A dataframe containing all the reads for a focal individual
                            gap_threshold, # A number representing the minimal time (in seconds) between two reading series
                            event_duration # A number representing the duration of a crossing event (in seconds).
                            ){

# I split the DF of my focal individual into a list of data frame. Each df for one antenna.    
  DF_ind <- split(DF_ind, f = DF_ind$antenna)
  
 
  list <- list()
  
# I obtain one output from the nb.events.ind.ant per antenna
    for (i in 1:length(DF_ind)){
     if (nrow(DF_ind[[i]]) != 0){
     
    # I use the function written above to obtain the the number of reading series, the duration and 
    # the number of crossing events of the focal individuals for each antenna i.
    list[[i]] <- nb.events.ind.ant(DF_ind[[i]])
  } # end of for loop
  } # end of i
  
  # I bind these outputs and reformat it
  DF3 <- bind_rows(list)
  #a <- c(DF3$id[1], "/", sum(DF3$reading_ser),
  #                          sum(DF3$duration), sum(DF3$nb_ev))
  #DF3 <- rbind(DF3, a)
  DF3 <- data.frame(id = DF3$id[1], reading_ser = sum(DF3$reading_ser),
         duration = sum(DF3$duration), nb_ev = sum(DF3$nb_ev))
  
  
  # I end up with a one-row-dataframe containing the number of reading series, the total duration and 
  # the number of crossing events for the focal individual. 
  return(DF3)
  } # end of function loop
  
  
  # Example
  nb.events.ind(df_list[[4]], gap_threshold = 1, event_duration = 3)
  

  
  