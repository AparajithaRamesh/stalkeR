


a <- EB_Changes
b <- subset(a, id == "0007E04FF8")



df <- data.frame()
final <- list()

for (i in 1:length(EB_list_df_id)){



  # I mark all the row numbers at which the fish was read at antenna 1 or a 4
  vec <- which(EB_list_df_id[[i]][,"antenna"] == 1 | EB_list_df_id[[i]][,"antenna"] == 4)
  # I want to cut *after* antenna 1 and 4 were read
  vec <- vec  +1
  # I split my data frame every time the individual passes by ant 1 or 4
  list <- split(EB_list_df_id[[i]], cumsum(1:nrow(EB_list_df_id[[i]]) %in% vec))

  # I only keep the data frames in which the fish reads at least 3 antennas
  # before reaching antenna 1 or antenna 4 again. In other words, the fish
  # *has* to be read both by antennas 2 and 3 before leaving the box, either
  # by antenna 1 or by antenna 4.
  cond <- lapply(list, function(x) nrow(x) >= 3)

  # I apply this condition to keep the dataframes (crossing reads) matching
  # these criteria
  list <- list[unlist(cond)]

  if (length(list) != 0){

  final[[i]] <- subset(EB_list_df_id[[i]], time == list[[1]]$time[1])


  }

}

final <- bind_rows(final)
final_morning <- subset(final, time_of_day == "Morning") %>%
  mutate(time_since_start = difftime(time, start_morning))
final_afternoon <- subset(final, time_of_day == "Afternoon") %>%
  mutate(time_since_start = difftime(time, start_afternoon))
final <- rbind(final_morning, final_afternoon)
final$time_since_start <- as.numeric(final$time_since_start)
final$time_since_start <- final$time_since_start / 60
hist(final$time_since_start)



