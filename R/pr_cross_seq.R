


  patterns_df <- as.data.frame(rbind(c(41, 42),
                                     c(42, 43)))


  pattern <- c(41, 44)

  start_time <-
    as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
  end_time <-
    as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")


  # Function to obtain the latency of each individual to cross one sequence
  lat_cross <- function(block_df, pattern) {
    # Define objects
    list_df <- list()

    # Split block_df per individual
    block_df_id <- split(block_df, f = block_df$id)
    # For each individual
    for (i in 1:length(block_df_id)) {
      # First reorder by time
      block_df_id[[i]][order(as.POSIXct(block_df_id[[i]]$time)),]
      # I obtain the row nb of the antenna changes
      changes          <-
        which(block_df_id[[i]]$antenna != lag(block_df_id[[i]]$antenna))
      # corresponding antenna numbers
      antenna          <-
        c(block_df_id[[i]]$antenna[1], block_df_id[[i]]$antenna[changes])
      # corresponding time
      time             <-
        c(block_df_id[[i]]$time[1], block_df_id[[i]]$time[changes])
      # name of individual
      id               <-
        c(block_df_id[[i]]$id[1], block_df_id[[i]]$id[changes])
      # Obtain a data frame with each antenna change per individual
      block_df_id[[i]] <- data.frame(antenna, time, id)

      # If there is at least one antenna change
      if (nrow(block_df_id[[i]]) > 1) {
        # reversed pattern
        pattern.rev <- rev(pattern)
        # Embed time series (?)
        w <- embed(block_df_id[[i]]$antenna, length(pattern))
        # I subset the rows corresponding to the second antenna change of the crossing sequence of interest
        list_df[[i]] <-
          block_df_id[[i]][which(rowSums(w == rep(pattern.rev, each = nrow(w))) == ncol(w)) +
                             1,]
        # I make a data frame containing the name of the individual, and its latency to reach the second
        # antenna of the sequence
        list_df[[i]] <- data.frame(id = list_df[[i]]$id[1],
                                   lat = as.numeric(difftime(list_df[[i]]$time[1], start_time, unit = 'm')))
      } # end of if
    } # end of for loop

    # I obtain a data frame containing all the individuals that crossed the sequence of interest, and
    # the corresponding latency to reach the second antenna of the sequence
    list_df <- bind_rows(list_df)
    # Remove rows with NAs
    list_df <- na.omit(list_df)
    # Reset row names
    rownames(list_df) <- NULL

    # I name the latency column based on the sequence of interest
    names(list_df)[2] <- paste0("lat_", paste0(pattern, collapse = "_"))
    return(list_df)

  } # end of lat_cross function

  # Define objects
  latency_per_seq <- list()


  # For every sequence of interest
  for (i in 1:nrow(patterns_df)) {
    # I run the lat_cross function and obtain the individuals latency to cross it
    latency_per_seq[[i]] <- lat_cross(block_df, patterns_df[i, ])
  }

  # I merge all data frames contained in latency_per_seq
  Reduce(function(x, y)
    merge(x, y, all = TRUE), latency_per_seq)

