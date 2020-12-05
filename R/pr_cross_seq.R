


  pattern <- c(41, 44)
  list <- list()
  start_time <-
    as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
  end_time <-
    as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")



  block_df_id <- split(block_df, f = block_df$id)
  for (i in 1:length(block_df_id)) {
    block_df_id[[i]][order(as.POSIXct(block_df_id[[i]]$time)), ]

    changes          <-
      which(block_df_id[[i]]$antenna != lag(block_df_id[[i]]$antenna))
    antenna          <-
      c(block_df_id[[i]]$antenna[1], block_df_id[[i]]$antenna[changes])
    time             <-
      c(block_df_id[[i]]$time[1], block_df_id[[i]]$time[changes])
    id               <-
      c(block_df_id[[i]]$id[1], block_df_id[[i]]$id[changes])
    block_df_id[[i]] <- data.frame(antenna, time, id)


    if (nrow(block_df_id[[i]]) > 1) {
      pattern.rev <- rev(pattern)
      w <- embed(block_df_id[[i]]$antenna, length(pattern))
      list[[i]] <-
        block_df_id[[i]][which(rowSums(w == rep(pattern.rev, each = nrow(w))) == ncol(w)), ]
      list[[i]] <- data.frame(id = list[[i]]$id[1],
                              lat = as.numeric(difftime(list[[i]]$time[1], start_time, unit = 'm')))
    }
  }
  list <- na.omit(bind_rows(list))
  rownames(list) <- NULL


  names(list)[2] <- paste0("lat_", paste0(pattern, collapse="_"))
  list





