# Script to get number of indivuals co-occurring within a certain time window
# See https://stackoverflow.com/questions/18689748/subsetting-based-on-co-occurrence-within-a-time-window
# Possible things to look at:
# 1. Total number of individuals read within this time window
# 2. Number of times at least one individuals was read within time window


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
  df$Time <- as.character(df$Time) #this needs to be changed while reading the file
  df$Unit.number <- as.integer(df$Unit.number)
  df$Transponder.code <- as.character(df$Transponder.code)
  df$Actual_time <- dmy_hms(paste(df$Date,df$Time,sep=" "))

# I make a new df with a subset of the variables of interest here
  new_dataset<-subset(df, select=c(Identifier, Actual_time, Unit.number, Transponder.code))
  names(new_dataset) <- c("Identifier", "time", "antenna", "id")

# A list of dataframe. Each dataframe corresponds to one antenna.
  df_list_ant <- split(new_dataset, f = new_dataset$antenna)

# I define my objects
time.window <- 5 # Time window in seconds
nb.antennas <- length(df_list_ant) # Number of antennas considered
individuals <- unique(new_dataset$id) # A character vector containing all the recorded invididuals
nb.individuals <- length(individuals) # Number of individuals



# Define my lists
co.occurent.ind <- list()
?with

# For loop sorting the data antenna by antenna
for (i in 1:nb.antennas){


  co.occurent.ind[[i]] <- with(df_list_ant[[i]], df_list_ant[[i]][
        (
          id == individuals[13] |
            antenna %in% antenna[id == individuals[13]]
        ) &
          apply(
            sapply(time[id == individuals[13]],
                   function(x) abs(difftime(time, x, units = "s")) <= time.window ), 1, any
          )
        ,]
      )
    # Remove the rows with the focal individuals
    co.occurent.ind[[i]] <-  subset(co.occurent.ind[[i]], id!=individuals[14])


    } # end the antenna loop



############################


# Let's scrutinize this code

# For loop sorting the data antenna by antenna
#for (i in 1:nb.antennas){

  DF3 <-
      with(
      df_list_ant[[3]], df_list_ant[[3]][
      apply(

      sapply(
      time[id == "0007A5C9A3"],
      function(x) abs(difftime (time, x, units = "s")) <= time.window
      ),

      MARGIN = 1, # Function applies to rows
      any)
      ,]
      )

  DF3 <-  subset(DF3, id != "0007A5C9A3")
  DF3

  ?sapply






  DF2 <- subset(df_list_ant[[2]], id == "0007A5C9A3")




  a <- for (i in 1:100){
  subset(df_list_ant[[2]],
  abs(difftime
      (time[i], time[i], units = "s")) <= time.window
  )}

############################

list.ind <- list()
# For loop sorting the data individual by individual
for (a in 1:13){

# For loop sorting the data antenna by antenna
for (i in 1:nb.antennas){


  co.occurent.ind[[i]] <- with(df_list_ant[[i]],df_list_ant[[i]][
    apply(

    sapply(time[id==individuals[a]],
    function(x) abs(difftime(time,x,units = "s")) <= time.window ),

    1, any)
    ,]
    )

  # Remove the rows with the focal individuals
  co.occurent.ind[[i]] <-  subset(co.occurent.ind[[i]], id!=individuals[a])
  list.ind[[a]] <- bind_rows(co.occurent.ind)

  # A list of dataframe (one dataframe per individual). Each row = one co-ocurrent individual.
  list.ind[[a]] <- data.frame(list.ind[[a]], Focal.ind = individuals[a])

  # I obtain the number of individuals recorded with the focal subjects within a certain time window
  Shoaling.df[a,] <- data.frame(individuals[a], nrow(list.ind[[a]]))
} # end the antenna loop

} # end of individual loop

