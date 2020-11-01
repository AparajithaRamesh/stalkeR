#' Get read duration
#' Duration of a read in antenna
#'
#' @param pit_data A dataframe: data that you get from the PIT antenna
#' @param test_details Dataframe containing the details of the test including the columns "test_ID","Start_time_exp","End_time_exp", "Start_time_pond","End_time_pond"
#'
#' @return a datafrme with 5 columns containing tag_ID, test_id, cross, antenna, duration at the antenna
#'
#' @description pit_data is the data you get directly from the PIT tag readers. It can be for one or multiple tests.
#'   This function is for getting time an individual spent at any antenna - hiding, taking shelter, crossing antenna etc
#' @export
#Loop through different pairs of start and end times for each test
#needs data set, start and end times of test ----run code from above

get_read_duration<-function(pit_data,
                          test_details){

  #filter useful cols and add the date-time column here already
  pit_data<-get_useful_cols(pit_data)
  names(pit_data) <- c("serial", "time", "antenna","id")

  # I split my dataframe into a list of dataframes (one object per individual)

  df_list <- split(pit_data, f = pit_data$id)
  nb_ind <- length(df_list)

  # define my objects
  nb_antennas <- length(unique(pit_data$antenna))  # Number of antennas
  nb_individuals <- length(unique(pit_data$id))    # Number of individuals
  gap_threshold <- 1                                  # The size of the time gap (in sec) to separate two different READING SERIES.
  event_duration <- 3                                 # The duration of one event

  ##PRE-REQUISITE FUNCTION - should I incorporate in new fn?
  nb.series.ind.ant <- function(DF){
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

      # For each reading series, I extract the duration spent sitting on the antenna.
      for(i in 1:length(DF2)){

        # I calculate the duration of each reading series. When only one read = 1 sec.
        # If a reading series starts at 00:00 and ends at 00:03, it counts as 4 seconds (and not 3).
        time[i] <- (difftime(tail(DF2[[i]]$time,1), DF2[[i]]$time[1], units = "s") + 1)
      }

      # I sum the duration of all the reading series (what I obtain is a vector, with
      # each object corresponding to a reading series)
      tot_time <- sum(time)

      # I obtain a df containing the individual id, the antenna number, the number of reading series (i.e. visits),
      # and the duration spent at this antenna.
      result <- data.frame(id = DF$id[1],
                           antenna = DF$antenna[1],
                           reading_ser = max(DF$reading_series),
                           duration = tot_time)


      return(result)} # end of if
  } # end of function

  ##start empty dataframe for filling up the results
  id <- character()
  antenna <- numeric()
  reading_ser <- numeric()
  duration = numeric() ##assuming here that the sum of seconds will give a numeric. but it will give a diff time object. need to handle that
  final_data<-data.frame(id,antenna, reading_ser, duration)

  ##first loop through individuals : i.e all the subsets of dflists
  for(i in 1:as.numeric(nb_ind)){

    #get my first focal individual
    focal_ind_dataset<-subset(df_list[[i]])
    focal_ind<-focal_ind_dataset$antenna[1]

    #individual dataframe should not have 0 rows, as we are only getting data from the PIT antenna
    assertthat::assert_that(nrow(focal_ind_dataset) != 0)

    #get number of antenna the focal individual has been read at
    n_ant<-focal_ind_dataset%>%distinct(antenna)

    ##second loop through all the antenna
    for (j in 1:nrow(n_ant)){
         focal_result<- nb.series.ind.ant(subset(df_list[[i]], df_list[[i]]$antenna == n_ant[[1]][j]))

         #add the dataframe to the results
         final_data<-rbind(final_data,focal_result)
    }
  }
  final_data
}


##getting some warning about uninitialised column 'reading series' but the output seems ok ...NEED TO CHECK
