# Packages
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(readr)
  library(data.table)
  library(patchwork)


## 1. DATA IMPORT AND MANIPULATION
# Import data
  df <- read_delim("20201008.CSV", ";", escape_double = FALSE, trim_ws = TRUE)

# 2. I create the 'lat.expl' function for 'Latency to explore'.
# From the raw data, it outputs all the individual full crossings.
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




  ## 2.1 REMOVE THE REPEATED READS
  # For each individual, I reduce the input vector (e.g., 1, 1, 1, 2, 2, 3) in a
  # way that one read is kept per sequence of identical numbers (e.g., 1, 2, 3).

  # I define 'df_list_red' which is basically 'df_list' except that the input vector
  # is replaced by the output vector.
    df_list_red <- list()
    nb_ind <- length(df_list)

  # I obtain the output vectors and the associated time and Identifier
    for (i in 1:nb_ind) {
      changes          <- which(df_list[[i]]$antenna!= lag(df_list[[i]]$antenna))
      antenna          <- c(df_list[[i]]$antenna[1], df_list[[i]]$antenna[changes])
      time             <- c(df_list[[i]]$time[1], df_list[[i]]$time[changes])
      id               <- c(df_list[[i]]$id[1], df_list[[i]]$id[changes])
      Identifier       <- c(df_list[[i]]$Identifier[1], df_list[[i]]$Identifier[changes])
      df_list_red[[i]] <- data.frame(antenna, time, id)
    }

  # I bind the rows of the list (i.e. make it a dataframe, as it initially was)
    df2 <- bind_rows(df_list_red)
    df2 <- as_tibble(df2)
  # df2 is basically the initial 'new_dataset' but the repeated reads
  # have been eliminated only to keep the first one.

# All the individuals antenna changes
  Changes <- sapply(df_list_red, nrow)
# All the individuals names
  Names <- numeric()
  for(i in 1:length(df_list_red)){
    Names[i] <- (df_list_red[[i]][1,3])
  }



## 1. NUMBER OF ANTENNA CHANGES PER INDIVIDUAL
# Data frame with number of crosses per individual
  df3 <- data.frame(Names, Changes)
  df3 <- df3[-1,]

  # I check if individuals have not been recorded by the antennas
  non_read_babies <- setdiff(df$`Transponder.code`, df3$Names)
  # Normal to have an error message if there is all the indviduals have been read
  non_read_babies <- data.frame(Names = non_read_babies,Changes =  0)

# Final df containing the read (and potential non-read) individuals
  df3 <- rbind(df3, non_read_babies)




## 2. DISTANCE TRAVELLED BY EACH INDIVIDUAL
  df_result <- df_list_red[[1]] %>%
    group_by(seq = {seq = rle(antenna); rep(seq_along(seq$lengths), seq$lengths)}) %>%
    ungroup() %>%
    mutate(if_11 = case_when(lag(seq) != seq ~ as.numeric(lag(antenna) == 11),
                              TRUE ~ NA_real_),
           next_12 = case_when(lead(seq) != seq ~ as.numeric(lead(antenna) == 11),
                              TRUE ~ NA_real_)) %>%
    group_by(seq, antenna) %>%
    mutate(result = case_when(sum(if_11) + sum(next_12) == 2 ~ TRUE,
                              TRUE ~ FALSE)) %>%
    ungroup() %>%
    select(result)

  ?case_
  df_result




  df_result <- df_list_red[[1]] %>%
    group_by(seq = {seq = rle(antenna); rep(seq_along(seq$lengths), seq$lengths)}) %>%
    ungroup() %>%
    mutate(start_11 = case_when(lag(seq) != seq ~ as.numeric(lag(antenna) == 11),
                             TRUE ~ NA_real_),
           next_12 = case_when(lead(seq) != seq ~ as.numeric(antenna == 12),
                               TRUE ~ NA_real_),

           start_13 = case_when(lag(seq) != seq ~ as.numeric(lag(antenna) == 13),
                                TRUE ~ NA_real_),
           next_14 = case_when(lead(seq) != seq ~ as.numeric(antenna == 14),
                               TRUE ~ NA_real_)) %>%
    group_by(seq, antenna) %>%




    mutate(result = case_when(
            sum(start_11) + sum(next_12) == 2 ~ TRUE,
            sum(start_13) + sum(next_14) == 2 ~ TRUE,
            TRUE ~ FALSE)) %>%
    ungroup() %>%
    select(result)

  df_result
  df_list_red[[1]]
