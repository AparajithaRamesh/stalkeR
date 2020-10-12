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
   lat.expl <- function(df, pattern1, pattern2, initial_time){
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
    names(new_dataset) <- c("Identifier", "time", "name", "id")

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
      changes          <- which(df_list[[i]]$name!= lag(df_list[[i]]$name))
      name             <- c(df_list[[i]]$name[1], df_list[[i]]$name[changes])
      time             <- c(df_list[[i]]$time[1], df_list[[i]]$time[changes])
      id               <- c(df_list[[i]]$id[1], df_list[[i]]$id[changes])
      Identifier       <- c(df_list[[i]]$Identifier[1], df_list[[i]]$Identifier[changes])
      df_list_red[[i]] <- data.frame(name, time, id)
    }

  # I bind the rows of the list (i.e. make it a dataframe, as it initially was)
    df2 <- bind_rows(df_list_red)
    df2 <- as_tibble(df2)
  # df2 is basically the initial 'new_dataset' but the repeated reads
  # have been eliminated only to keep the first one.




  ## 2.2 IDENTIFY WHEN INDIVIDUALS CROSS THE BOX ENTIRELY
  # I stole the next piece of code from someone way smarter than me (see R Stack Overflow 41130912).
  # First, I keep reads when individuals go through the box the following way: 11 -> 12 -> 13 -> 14
    len_pattern = length(pattern1)
    df_abcd <- df2 %>% arrange(id, time) %>% group_by(id) %>%
    # check multiple lags condition
    mutate(ab = Reduce("&", Map("==", shift(name, n = 0:(len_pattern - 1), type = "lead"), pattern1)),
    g = cumsum(ab)) %>%
    # use reduce or to subset sequence rows having the same length as the pattern
    filter(Reduce("|", shift(ab, n = 0:(len_pattern - 1), type = "lag"))) %>%
    # make unique names
    group_by(g, add = TRUE) %>% mutate(name = paste(name, 1:n(), sep = "_")) %>%
    # pivoting the table to wide format
    select(-ab) %>% spread(name, time)


  # Second, I keep reads when individuals go through the box the other way around: 14 -> 13 -> 12 -> 11
    df_dcba <- df2 %>% arrange(id, time) %>% group_by(id) %>%
    # check multiple lags condition
    mutate(ab = Reduce("&", Map("==", shift(name, n = 0:(len_pattern - 1), type = "lead"), pattern2)),
    g = cumsum(ab)) %>%
    # use reduce or to subset sequence rows having the same length as the pattern
    filter(Reduce("|", shift(ab, n = 0:(len_pattern - 1), type = "lag"))) %>%
    # make unique names
    group_by(g, add = TRUE) %>% mutate(name = paste(name, 1:n(), sep = "_")) %>%
    # pivoting the table to wide format
    select(-ab) %>% spread(name, time)


  # I rename the columns of the two dataframes I created
  names(df_abcd) <- c("id", "g", "Ant1", "Ant2", "Ant3", "Ant4")
  names(df_dcba) <- c("id", "g", "Ant1", "Ant2", "Ant3", "Ant4")

  # I obtain a dataframe in which each row represents one individual full crossing
  df3 <- rbind(df_abcd, df_dcba)

  # I keep individuals with the smallest 'arrival' (i.e. Ant4) value.
  df3 <- df3 %>%
    group_by(id) %>%
    slice(which.min(Ant2))




  ## 2.3 CALCULATE LATENCY TIME
  # I add a new column with the time to cross the box since the start of the test
  df3 <- mutate(df3, time_since_start = Ant2 - initial_time)
  df3$time_since_start <- as.numeric(df3$time_since_start)





  return(df3)
}


# 3. I indicate the crossing sequences (a -> d and d -> a) and starting time
  p11.14 = c(11, 12, 13, 14)
  P14.11 = c(14, 13, 12, 11)
  t11.30 <- as.POSIXct("2020-10-08 11:30:00 UTC", tz="UTC")

# I create my final df given a dataset and the two crossing sequences
  final_df <- lat.expl(df, p11.14, P14.11, t11.30)

# I obtain the individuals that did not cross at all during the duration of the test
  non_expl_babies <- setdiff(df$`Transponder code`, final_df$id)


## PLOTTING
  ggplot(data=final_df, aes(time_since_start)) +
    geom_histogram(aes(),
                   fill="#6f7b96",
                   alpha = .8) +
    labs(x="Latency to explore the box (minutes)", y="Count") +
    theme(axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "#f7f5f5"))


# Plotting the non-visitors

# I obtain the individuals that did not cross at all during the duration of the test
  non_expl_babies <- setdiff(df$`Transponder code`, final_df$id)
  nb_non_expl <- length(non_expl_babies)

# I create a very simple dataframe with two columns
# The first one is the ids of the individuals that crossed
# The second one is a '1' for each row
  id <- c(non_expl_babies)
  crossings <- c(nb_non_expl)
  non_expl_df <- data.frame(id, crossings)


# I merge this dataframe with the one created above
  final_df2 <- merge(final_df, non_expl_df, by = c("id"), all = T)

  a <- ggplot(data=final_df2, aes(time_since_start)) +
    geom_histogram(
      aes(),
      fill="#6f7b96",
      alpha = .8) +
    labs(x="Latency to explore the box (minutes)", y="Count") +
    theme(axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "#f7f5f5"),
          aspect.ratio = .4) +
    ylim(0, 3)

  b <- ggplot(data=non_expl_df, aes(crossings)) +
    geom_histogram(aes(),
                   fill="#6f7b96",
                   alpha = .8) +
    labs(x = "No visit", y=" ") +
    theme(axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "#f7f5f5"),
          aspect.ratio = 14,
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylim(0, 3)

  (a + b)



