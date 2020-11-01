# Script to get the number of reading series and duration spent in presence of a conspecific, for every fish.

#############################################################################################################    
# A. Packages, data import and manipulation   


# Packages
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(data.table)
  library(patchwork)
  library(stringr)
  library(readxl)


# Import data
  df <- read_delim("20201008.CSV", ";", escape_double = FALSE, trim_ws = TRUE)


# Naming my columns
  names(df) <- c( "Identifier", "Date", "Time",
                  "Unit.number", "Antenna.number", "Transponder.type",
                  "Transponder.code", "Weight", "Input.status",
                  "Output.status", "Event", "GPS.coordinates")


# Defining my variables
  df$Identifier <- as.integer(df$Identifier)
  df$Time <- as.character(df$Time) # this needs to be changed while reading the file
  df$Unit.number <- as.integer(df$Unit.number)
  df$Transponder.code <- as.character(df$Transponder.code)
  df$Actual_time <- dmy_hms(paste(df$Date,df$Time,sep=" "))

# I make a new df with a subset of the variables of interest here
  new_dataset<-subset(df, select=c(Actual_time, Unit.number, Transponder.code))
  names(new_dataset) <- c("time", "antenna", "id")
  new_dataset <- new_dataset  %>% distinct()
  
# I assign every individual to a group (Morning/Afternoon)
  x = as.POSIXct(strptime(c("090000","123000","190000"),"%H%M%S"),"UTC")
  date(x) <- new_dataset$time[1]
  
  new_dataset$time_of_day <- case_when(
    between(new_dataset$time,x[1],x[2]) ~"Morning",
    between(new_dataset$time,x[2],x[3]) ~"Afternoon")
  
  
  
# I separate my dataframe based on (i) antenna and (ii) individuals
  df_list_ant <- split(new_dataset, f = new_dataset$antenna)
  df_list_ind <- split(new_dataset, f = new_dataset$id)


# I define my objects
  time.window <- 2 # Time window in seconds
  nb.antennas <- length(df_list_ant) # Number of antennas considered
  individuals <- unique(new_dataset$id) # A character vector containing all the recorded invididuals
  nb.individuals <- length(individuals) # Number of individuals



  
 
  # Below, for every individual, I will select the *accompanied reads*, i.e. when the focal individual is read
  # simultaneously with at least one conspecific at a certain antenna. By simultaneously, I mean within
  # a certain time-window, defined above (e.g., 2 sec).
  
    
    
  
#############################################################################################################    
# B. Function n°1 - obtain accompanied reads for a focal individual at one antenna                            
    
    
    
  # Function n°1
  # Input1 = All the reads at one antenna (data frame)
  # Input2 = The reads of one focal individuals at the same antenna (data frame)
  # Output = All the accompanied reads for the focal individual (data frame)
  reads_ind_ant <- function(antenna, focal){
    
    # I define a list
    list_co_occurrences <- list()
    # If individuals have been read by an antenna, run the loop below
    
    for (i in 1:nrow(antenna)){      # Time window loop
      if (nrow(focal != 0)){
        
         # I define my antenna dataframe. 
         # First, it contains all the individuals read at this antenna except the focal individual.
         antenna <- subset(antenna, id != focal$id[1])
        
        # Second, I remove the individuals that have been read at this antenna in another pond (i.e. during 
        # another part of the day) to make the computation more efficient.
        antenna <- subset(antenna, time_of_day == focal$time_of_day[1])
        
        # A list. Each df corresponds to the friends reads for a single read of the focal individuals
        list_co_occurrences[[i]] <- focal[abs(difftime(antenna$time[i], 
                                                       focal$time, units = "s")) <= time.window, ]
        
      }} # End of time window loop and 'if'
    
    
    # I obtain a dataframe with all the reads that occurred in presence of at least one conspecific
    co_occurrences_per_ind <- bind_rows(list_co_occurrences)
    
    # Remove the repeats.
    co_occurrences_per_ind <- co_occurrences_per_ind  %>% distinct()
    
    co_occurrences_per_ind <- na.omit(co_occurrences_per_ind)
    
    return(co_occurrences_per_ind)}
  
  # Example to run the function n°1:
  #reads_ind_ant(df_list_ant[[2]], subset(df_list_ant[[2]], id =="0007A383F0"))
  
  
  
    
    
#############################################################################################################      
# C. Function n°2 - obtain accompanied reads for all individuals at one antenna       
    
    
  
  # Function n°2
  # Input = All the reads at one antenna (data frame), as for function n°1.
  # Output = All accompanied reads from all individuals at the antenna (data frame).
  reads_ant <- function(antenna.df){
    # Define my list
    list <- list()
    # For every individual read at the focal antenna...
    for(i in 1:length(unique(antenna.df$id))){
      # ... I subset all accompanied reads (output of function n°1). Here, each dataframe = accompanied reads 
      # for one individual.
      list[[i]] <- reads_ind_ant(antenna.df, subset(antenna.df, id == unique(antenna.df$id)[i]))
      
    }
    # From the list above, I make one dataframe containing all the accompanied reads from all individuals.
    list <- bind_rows(list)
    return(list)
  }
  
  # Example to run the function n°2:
  # b <- reads_ant(df_list_ant[[1]]) 
  

    
    
    
#############################################################################################################      
# D. Function n°3 - obtain accompanied reads for all individual at all antennas   
    
    
  # Function n°3. 
  # Input = A list of dataframes, where each dataframe contains all the reads for one antenna.
  # Output = A list of dataframes, where each dataframe contains all the accompanied reads for an individual.
  reads <- function(antenna){
    # Define my list
    list2 <- list()
    # For every antenna...
    for(i in 1:nb.antennas){
    # ... I wish to get the accompanied reads (output of my function n°2). I end up with a list of data frames
    # where each dataframe corresponds to one antenna.
        list2[[i]] <- reads_ant(antenna[[i]])
    }
    # I actually want to separate my data based on individual identity, and not based on antenna.
    list2 <- bind_rows(list2)
    list2 <- split(list2, f = list2$id)
    
    return(list2)
  }

    # I run function n°3 end up with a list of dataframes, where each dataframe contains all the 
    # accompanied reads for an individual.
    df2 <- reads(df_list_ant)
    
    
    
    
    
#############################################################################################################        
# E. I obtain the number of accompanied reading series and duration for all individuals.


    
  
  # Here, I will use a funtion written in the Defining_events script to obtain, from the list of reads,
  # the duration read at antennas, and the number of reading series.
  
  # I first define two lists
  accompanied_series <- list()
  non_accompanied_series <- list()
  
  # A. For every individual that has been read accompanied...
  for (i in 1:length(df2)){
    # I want to obtain the number of accompanied reading series and the time spent accompanied at antennas.
    accompanied_series[[i]] <- nb.series.ind(df2[[i]], gap_threshold = 1)
  }
  
  # B. Then, for every individual that has been read (even the lonely ones, actually)...
  for (i in 1:length(df_list_ind)){
    # I want to obtain the number of reading series and the time spent read by antennas.
    non_accompanied_series[[i]] <- nb.series.ind(df_list_ind[[i]], gap_threshold = 1)
    
  }

    # I transform my two lists as two dataframes. 
    accompanied_series_df <- bind_rows(accompanied_series)
    non_accompanied_series_df <- bind_rows(non_accompanied_series)
    
    # I merge them and rename the columns
    DF <- merge(accompanied_series_df, non_accompanied_series_df, by = 'id')
    names(DF) <- c("id", "acc_read_series", "acc_duration", "tot_read_series", "tot_duration")

    
  
  
  
