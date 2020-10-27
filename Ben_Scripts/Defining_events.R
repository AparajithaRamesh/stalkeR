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
    
  # I create the 'gap' column. In every row, the time gap between the focal read/row and the previous one is computed.
    df_list[[i]]$gap <- c(NA, with(df_list[[i]], time[-1] - time[-nrow(df_list[[i]])]))
    
  # I define the size of the time gap (in sec) to separate two different READING SERIES.
    gap_threshold <- 2
    
  # I create a new column (i.e. over_threshold) indicating if the observed gap is bigger (TRUE) or
  # or smaller (FALSE) than the gap_threshold
    df_list[[i]]$over_thresh <- df_list[[i]]$gap > gap_threshold
    
  # I assign every read to a reading series (that is, a group of uninturrepted reads) based on the 'over_threshold' column
    for (a in 1:nrow(df_list[[i]])){
      df_list[[i]]$reading_series[a] <- nrow(filter(df_list[[i]][1:a,], over_thresh == "TRUE"))
      
    } # end of the reading series loop
    } # end of individual loop
  
  
  
  
  
  
## 3. I now want, in addition to then number of reading series per individual, have the number of CROSSING EVENTS.
# Here, I chose a duration threshold, for which we can consider that a reading series is composed of more than one
# reading event. For example, let's say that the duration threshold if 5 seconds. Then, if I have a reading series of
# 15 uninturrepted seconds, it represents 3 crossing events.
# 14 uninturrepted seconds <- 3 reading events
# 16 uninturrepted seconds <- 4 reading events
  
duration_threshold <- 5
  
  ceiling(nrow(df[1:11,])/duration_threshold)
  
  ceiling(nrow(df[1:4,])/3) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# Take rows 1 to n-1 and subtract rows 2 to n:
my_dat$gap <- c(NA, with(my_dat, time[-1] - time[-nrow(my_dat)]))

# now, how often was the gap more than some amount of time?
gap_threshold <- 30 # let's say, 30 seconds
my_dat$over_thresh <- my_dat$gap > gap_threshold



length(my_dat$over_thresh[my_dat$over_thresh == TRUE])


nrow(filter(my_dat[1:2,], over_thresh == "TRUE"))


  

for (i in 1:10){
  my_dat$time_group[i] <- nrow(filter(my_dat[1:i,], over_thresh == "TRUE"))
  
}

mu_da



