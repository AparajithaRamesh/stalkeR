# Script to get number of friends co-occurring within a certain time window


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


## 1. DATA IMPORT AND MANIPULATION
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

# A list of dataframe. Each dataframe corresponds to one antenna.
  df_list_ant <- split(new_dataset, f = new_dataset$antenna)



## 2. OBTAIN FRIENDS CO-OCCURRING WITHIN A CERTAIN TIME WINDOW
# I define my objects
  time.window <- 2 # Time window in seconds
  nb.antennas <- length(df_list_ant) # Number of antennas considered
  individuals <- unique(new_dataset$id) # A character vector containing all the recorded invididuals
  nb.individuals <- length(individuals) # Number of individuals

# Define the lists and vectors used in the loops below
  focal <- list()
  list_co_occurrences <- list()
  co_occurrences_per_ind <- list()
  nb.occ <- numeric()
  nb.ind <- numeric()
  nb.ind.list <- list()
  Shoaling.dfs <- list()

  

  
 
  # 1. NEW APPROACH. FOCUSING ON FOCAL INDIVIDUALS. The code just works, but still not clean nor annotated.
  
  
  # Here I have a function to get all the accompanied reads from afocal individual for one antenna
  reads_ind_ant <- function(antenna, focal){
    
    # If individuals have been read by an antenna, run the loop below
    
    for (i in 1:nrow(antenna)){      # Time window loop
      if (nrow(focal != 0)){
        
        antenna <- subset(antenna, id != focal$id[1])
        
        # A list. Each df corresponds to the friends reads for a single read of the focal individuals
        list_co_occurrences[[i]] <- focal[abs(difftime(antenna$time[i], focal$time, units = "s")) <= time.window, ]
        
      }} # End of time window loop and 'if'
    
    
    # I obtain a dataframe with all the reads that occurred in presence of at least one conspecific
    co_occurrences_per_ind <- bind_rows(list_co_occurrences)
    
    # Remove the repeats.
    co_occurrences_per_ind <- co_occurrences_per_ind  %>% distinct()
    
    co_occurrences_per_ind <- na.omit(co_occurrences_per_ind)
    
    return(co_occurrences_per_ind)}
  
  # reads_ind_ant(df_list_ant[[2]], subset(df_list_ant[[2]], id =="0007A383F0"))
  
  
  
  
  
  
  # Function n°2. Input = all reads from one antenna. Output = list of df, where one df is accompanied reads for one individual,
  # still for one antenna.
  reads_ant <- function(antenna.df){
    list <- list()
    
    for(i in 1:length(unique(antenna.df$id))){
      
      list[[i]] <- reads_ind_ant(antenna.df, subset(antenna.df, id == unique(antenna.df$id)[i]))
      
    }
    list <- bind_rows(list)
    return(list)
  }
  

  # a <- reads_ant(df_list_ant[[1]])
  
  
  
  # Function n°3. 
  # Input: list of df with all reads separated by antenna unit
  # Output: list of list with everythinggggg
  reads <- function(antenna){
    
    list2 <- list()
    for(i in 1:nb.antennas){
      list2[[i]] <- reads_ant(antenna[[i]])
    }
    return(list2)
  }
  
  df2 <- reads(df_list_ant)
  df2 <- bind_rows(df2)
  df2 <- df2  %>% distinct()
  df2 <- split(df2, f = df2$id)


  
  
  
  # 2. OLD APPROACH. I subset all FRIENDS READS (i.e. for each individual, all the reads from conspecifics co-occurring at an antenna)  
  
  
  for (x in 1:nb.antennas){                      # Antenna loop
    
    for (a in 1:nb.individuals){                 # Individuals loop
      
      # Each df = reads of one ind. at one antenna
      focal[[a]] <- subset(df_list_ant[[x]], id == individuals[a])
      # If individuals have been read by an antenna, run the loop below
      if (nrow(focal[[a]] != 0)){
        for (i in 1:nrow(focal[[a]])){             # Time window loop
          
          
          # A list. Each df corresponds to the friends reads for a single read of the focal individuals
          list_co_occurrences[[i]] <- subset(df_list_ant[[x]],
                                             abs(difftime(df_list_ant[[x]]$time, focal[[a]]$time[i], units = "s")) <= time.window)
          
          
          # I remove the reads from the focal individual
          list_co_occurrences[[i]] <- subset(list_co_occurrences[[i]], id != individuals[a])
        }} # End of time window loop and 'if'
      
      
      
      # I obtain a dataframe with all the friends reads
      co_occurrences_per_ind[[a]] <- bind_rows(list_co_occurrences)
      
      # Remove the repeats. I end up with a list. Each df contains all the friends reads for a focal ind, within the time window
      co_occurrences_per_ind[[a]] <- co_occurrences_per_ind[[a]]  %>% distinct()
      
      
      # I want to know the number of _ friends reads_ per focal individual (regardless of identity of friend)
      nb.occ[a] <- c(nrow(co_occurrences_per_ind[[a]]))
      
    } # end of individuals loop
    
    # Number of friends reads per individual for each antenna
    Shoaling.dfs[[x]] <- data.frame(nb.occ, individuals, df_list_ant[[x]]$antenna[1])
    
  } # end of antenna loop
  
  # I bind the rows of the list.
  Shoaling.df <- bind_rows(Shoaling.dfs)
  
  # Rename column 3
  names(Shoaling.df)[3] <- "antenna"
  
  # Spread the table horizontally. Each column for one antenna
  Shoaling.df <- spread(Shoaling.df, antenna, nb.occ)
  
  # I create my sociality index
  Shoaling.df$tot <- rowSums(Shoaling.df[,-1])
  
  
  ggplot(data=Shoaling.df, aes(tot)) +
    geom_histogram(
      aes(),
      fill="#6f7b96",
      alpha = .8) +
    labs(x="Number of co-occurrent reads", y="Count") +
    theme(axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "#f7f5f5"),
          aspect.ratio = .4) +
    ylim(0, 3)
  
  
  
  