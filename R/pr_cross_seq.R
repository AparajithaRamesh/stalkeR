

patterns_df <- as.data.frame(
  rbind(c(41, 44),
        c(42,43)))


  pattern <- c(41, 44)

  start_time <-
    as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
  end_time <-
    as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")


# Function to obtain the latency of each individual to cross one sequence
lat_cross <- function(block_df, pattern){

   # Define objects
  list_df <- list()

  # Split block_df per individual
  block_df_id <- split(block_df, f = block_df$id)
  # For each individual
  for (i in 1:length(block_df_id)) {
    # First reorder by time
    block_df_id[[i]][order(as.POSIXct(block_df_id[[i]]$time)), ]
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
      # I subset the rows corresponding to the first read of the crossing sequence of interest
      list_df[[i]] <-
        block_df_id[[i]][which(rowSums(w == rep(pattern.rev, each = nrow(w))) == ncol(w)), ]
      list_df[[i]] <- data.frame(id = list_df[[i]]$id[1],
                              lat = as.numeric(difftime(list_df[[i]]$time[1], start_time, unit = 'm')))
    }
  } # end of for loop
  list_df <- na.omit(bind_rows(list_df))
  rownames(list_df) <- NULL


  names(list_df)[2] <- paste0("lat_", paste0(pattern, collapse="_"))
  return(list_df)

  }

latency_per_seq <- list()
for (i in 1:nrow(patterns_df)){

  latency_per_seq[[i]] <- lat_cross(block_df, patterns_df[i,])

}

# I merge all data frames contained in latency_per_seq
Reduce(function(x, y) merge(x, y, all=TRUE), latency_per_seq)





