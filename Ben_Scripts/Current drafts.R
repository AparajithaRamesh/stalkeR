


a <- EB_Changes
b <- subset(EB_Changes, id == "00079F94CB")
View(b)

EB_Changes_list <- split(EB_Changes, f = EB_Changes$id)

EB_list_df_id[[i]] <- subset(EB_clean_df, id == "0007EA023D")









# I split my dataframe into a list of dataframes (one object per individual)
EB_list_df_id <- split(EB_clean_df, f = EB_clean_df$id)

# Define objects
df <- data.frame()
final <- list()
LIST <- list()

# For every individual
for (i in 1:length(EB_list_df_id)){



# I want to obtain the reads corresponding to an antenna change and the
# corresponding antenna number, time and fish id.
EB_list_df_id[[i]][order(as.POSIXct(EB_list_df_id[[i]]$time)),]

changes          <- which(EB_list_df_id[[i]]$antenna!= lag(EB_list_df_id[[i]]$antenna))
antenna          <- c(EB_list_df_id[[i]]$antenna[1], EB_list_df_id[[i]]$antenna[changes])
time             <- c(EB_list_df_id[[i]]$time[1], EB_list_df_id[[i]]$time[changes])
id               <- c(EB_list_df_id[[i]]$id[1], EB_list_df_id[[i]]$id[changes])
df_list_red[[i]] <- data.frame(antenna, time, id)




  # Then, I mark all the row numbers at which the fish was read at antenna 1 or a 4
  vec <- which(df_list_red[[i]][,"antenna"] == 1 | df_list_red[[i]][,"antenna"] == 4)
  # I want to cut *after* antenna 1 and 4 were read
  vec <- vec  +1
  # I split my data frame every time the individual passes by ant 1 or 4
  list <- split(df_list_red[[i]], cumsum(1:nrow(df_list_red[[i]]) %in% vec))

  # I only keep the data frames in which the fish reads at least 3 antennas
  # before reaching antenna 1 or antenna 4 again. In other words, the fish
  # *has* to be read both by antennas 2 and 3 before leaving the box, either
  # by antenna 1 or by antenna 4.
  cond1 <- lapply(list, function(x) nrow(x) >= 3)


  # I apply this condition to keep the dataframes (crossing reads) matching
  # these criteria
  list1 <- list[unlist(cond1)]

  # If there is such a sequence corresponding to a full crossing of the box
  if (length(list1) != 0){

  # Then I subset the first read of the first sequence, likely at antenna 2 or 3
  final[[i]] <- subset(list1[[1]], time == list1[[1]]$time[1])


  # At this point, my list contains, for each individual that fully crossed the box,
  # its first read at the next antenna it was read at after being read at antenna 1 or 4

  } # end of if


  cond2 <- lapply(list, function(x) nrow(x) >= 2)
  list2 <- list[unlist(cond2)]

  if (length(list2) != 0){


  for(x in 1:length(list2)){
  duration <- 0
  duration <- duration + difftime(list2[[x]][nrow(list2[[x]]), ]$time, list2[[x]][1,]$time, units='m')


  LIST[[i]] <- data.frame(list2[[1]]$id[1], duration)

  }}

} # end of for loop


# I bind the rows of my list.
final <- bind_rows(final)
final2 <- bind_rows(LIST)
final2$duration <- as.numeric(final2$duration)
hist(final2$duration)

# I add a column with the time since these fish were read after the start of the test
  final <- merge(final, EB_variables, by = 'id', all = T)

  final_morning <- subset(final, time_of_day == "Morning") %>%
    mutate(time_since_start = difftime(time, start_morning, units='m'))

  final_afternoon <- subset(final, time_of_day == "Afternoon") %>%
    mutate(time_since_start = difftime(time, start_afternoon, units='m'))

  final <- rbind(final_morning, final_afternoon)



# I make this column as numerical
  final$time_since_start <- as.numeric(final$time_since_start)

# I give the maximal value for the fish that never crossed the box
  final <- final %>%
    mutate(time_since_start = if_else(is.na(time_since_start), 150, time_since_start))


hist(final$time_since_start)













final <- bind_rows(final)


final_morning <- subset(final, time_of_day == "Morning") %>%
  mutate(time_since_start = difftime(time, start_morning, units='m'))

final_afternoon <- subset(final, time_of_day == "Afternoon") %>%
  mutate(time_since_start = difftime(time, start_afternoon, units='m'))

final <- rbind(final_morning, final_afternoon)


# I make this column as numerical
