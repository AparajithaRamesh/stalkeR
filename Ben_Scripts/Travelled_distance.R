# Here I obtain the number of antenna changes as well as distance travelled between antennas per individual.


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
    library(tidyverse)


## 1. DATA IMPORT AND DEFINING MY OBJECTS
    # Import data
    df1 <-  read_delim("~/Cours/M2 - Sticklebacks/Data/1. Preliminary tests/4. Week -1/Oct 27/20201027.CSV", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
    df2 <- read_delim("~/Cours/M2 - Sticklebacks/Data/1. Preliminary tests/4. Week -1/Oct 28/20201028.CSV", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
    
    df <- rbind(df1, df2)

    # Distances between antennas
    pond.width   <- 1
    pond.length  <- 1.1
    pond.diagonal<- 1.25

    # Sequence of antenna changes corresponding to the distances above
    
                #######################
                #                     # 
                #        #   #        #
                #     #        #      #
                #   #  3      4   #   #
                #   #             #   #
                #   #             #   #
                #   #             #   #
                #   #  2      1   #   #
                #     #        #      #
                #       ## ##         #
                #                     #
                #    Central pond     #
                #         below       #
                #                     #
                #######################
    
    
    seq.width  <- c('12', '21', '34', '43')
    seq.length <- c('13', '31', '24', '42')
    seq.diag   <- c('14', '41', '23', '32')


## 2. DATA MANIPULATION
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

    # I rename my antennas with a standardised notation across ponds (see scheme above)
    new_dataset[new_dataset == 11 | new_dataset == 21 | new_dataset == 31 | new_dataset == 41] <- 1
    new_dataset[new_dataset == 12 | new_dataset == 22 | new_dataset == 32 | new_dataset == 42] <- 2
    new_dataset[new_dataset == 13 | new_dataset == 23 | new_dataset == 33 | new_dataset == 43] <- 3
    new_dataset[new_dataset == 14 | new_dataset == 24 | new_dataset == 35 | new_dataset == 44] <- 4
    
    
    
    
    
    # Now, I will remove all the reads that do not come from the fish present in the ponds (e.g. ghost reads, test reads)
    # I import my list of fish
    list_fish <- read_excel("~/Cours/M2 - Sticklebacks/Data/1. Preliminary tests/4. Week -1/Ben_PITtag_final_grouped_FINAL.xlsx", 
                            sheet = "Sheet1")

    # I identify the reads that do not  come from the ponds
    ghost_reads <- setdiff(new_dataset$id, list_fish$id)

    # I remove all of them from the dataframe
    for (i in 1:length(ghost_reads)){
    new_dataset <- new_dataset[!(new_dataset$id == ghost_reads[i]),]
    }
    
    # Now, if I run this line, I should obtain a value equal or smaller than 200.
    length(unique(new_dataset$id))
    
    
    # I assign every individual to a group (Morning/Afternoon)
    x = as.POSIXct(strptime(c("090000","123000","190000"),"%H%M%S"),"UTC")
    date(x) <- new_dataset$time[1]
    
    new_dataset$time_of_day <- case_when(
      between(new_dataset$time,x[1],x[2]) ~"Morning",
      between(new_dataset$time,x[2],x[3]) ~"Afternoon")
    
    
    time_of_day <- unique(new_dataset[,c('id','time_of_day')])

    
    trav.dist <- function(new_dataset){
    
    # I split my dataframe into a list of dataframes (one object per individual)
    df_list <- split(new_dataset, f = new_dataset$id)


    
## 3. REMOVE THE REPEATED READS AND OBTAIN THE ANTENNA CHANGE
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

    # I obtain the number of times each individual was read in two different antennas consecutively
    Changes <- sapply(df_list_red, nrow)
    id <- numeric() # I obtain the individual names
    for(i in 1:length(df_list_red)){
      id[i] <- df_list_red[[i]][1,3]}

    # I can generate a data frame with number of changes per individual
    df3 <- data.frame(id, Changes)


## 4. DISTANCE TRAVELLED BY EACH INDIVIDUAL

    # A for loop generating the total distance travelled by each individuals between the antennas it's been read at
    # I define my Dist numerical vector
    Dist <- numeric()
    for (i in 1:length(df_list_red)){
      # I make three numerical vectors, each for a distance type (i.e. width/length/diagonal)
      # Each element of this vector is the number of times an individual achieved the pattern above
      # E.g. width = c(4, 2, 3, 7) corresponds to an individuals crossing 4 times 11->12, 2 times 12->11,
      # 3 times 13->14 and 7 times 14-13.
      width <- str_count(paste(df_list_red[[i]]$antenna, collapse=""), seq.width)
      length <- str_count(paste(df_list_red[[i]]$antenna, collapse=""), seq.length)
      diagonal <- str_count(paste(df_list_red[[i]]$antenna, collapse=""), seq.diag)

      # I obtain the achieved distance per distance type (i.e. width/length/diagonal)
      dist.width <- sum(width)*pond.width
      dist.length <- sum(length)*pond.length
      dist.diagonal <- sum(diagonal)*pond.diagonal
      # Total distance
      Dist[i] <- (dist.width + dist.length + dist.diagonal)
    }

    # Total distance travalled by all individuals
    df4 <- cbind(df3, Dist)
    
    return(df4)
    }

  # I run the 'trav.dist' function
    Travelled.distance <- trav.dist(new_dataset)
    
    # I assign every individual to its pond and treatment category
    Travelled.distance <- merge(Travelled.distance, list_fish[c(2,8,9, 10)], by = "id")
    Travelled.distance <- merge(Travelled.distance, time_of_day, by = "id")
  


    # We can see that as expected, the correlation between nb of crosses and distance is strong
    plot(Travelled.distance$Changes, Travelled.distance$Dist)
    

    ggplot(data = Travelled.distance, aes(x = Treatment_seq, y = Dist, fill = Replicate)) + 
      geom_violin(position = position_dodge(width = 0.4)) + 
      #geom_boxplot(width=.1, outlier.colour=NA, position = position_dodge(width = 0.4), colour = "black") +
      scale_fill_brewer(palette="Blues") + 
      theme_classic() + 
      stat_summary(fun.data=mean_sdl, 
                   fun.args = list(mult = 1), # I show 1 SD
                   geom = "pointrange", color="#414c61", 
                   position = position_dodge(width = 0.4))
    
    
    # I check if individuals might have not been recorded at all by the antennas
    non_read_babies <- setdiff(df$`Transponder code`, Travelled.distance$id)
    non_read_babies <- data.frame(id = non_read_babies, Changes =  0, Dist =  0)
    
    # Final df containing the read (and potential non-read) individuals
    Travelled.distance <- rbind(Travelled.distance, non_read_babies)
    
    