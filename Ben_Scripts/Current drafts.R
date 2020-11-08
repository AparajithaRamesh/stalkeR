


a <- EB_Changes
b <- subset(EB_Changes, id == "00079F94CB")
View(b)

EB_Changes_list <- split(EB_Changes, f = EB_Changes$id)

EB_list_df_id[[i]] <- subset(EB_clean_df, id == "0007EA023D")









# I split my dataframe into a list of dataframes (one object per individual)
EB_list_df_id <- split(EB_clean_df, f = EB_clean_df$id)

# Define objects
df <- data.frame()
final1 <- list()
final2 <- list()

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

## 1. Latency to fully explore the box

  # From these sequences, I want to keep the sequences of reads corresponding
  # to a full crossing of the box. To do so, I keep the sequences containing at
  # least a read from antenna 2 *and* 3 before a fish is read out of the box again
  # at the antenna 1 or 4.  2 and 3 before leaving the box, either
  # by antenna 1 or by antenna 4.

  # I select the sequences for which the individual is read at antennas 2 *and* 3
  cond1 <- lapply(list, function(x) nrow(x) >= 3)


  # I apply this condition to keep the crossing sequences
  list1 <- list[unlist(cond1)]

  # If there is such a sequence corresponding to a full crossing
  if (length(list1) != 0){

  # Then I subset the first read of the first crossing, necessarily at antenna 2 or 3,
  final1[[i]] <- subset(list1[[1]], time == list1[[1]]$time[1])


  # Eventually, this list contains, for each individual that fully crossed the box,
  # its first read at antenna 2 or 3 after entering the box for a full cross.

  } # end of if


## 2. Time spent in the box

  # Now I want to obtain the time spent in the box. However, in this case, the individuals
  # do not need to fully cross the box (i.e. no need to be read by antenna 2 *and* 3 after
  # entering the box). Instead, I simply calculate the time between an individual is read at
  # antenna 2 or 3 until it is read at antenna 1 or 4 again, whatever the sequence is.

  # I select the sequences for which the individual is read at least once at antenna 2 or 3
  cond2 <- lapply(list, function(x) nrow(x) >= 2)
  # I keep the sequences corresponding to this condition
  list2 <- list[unlist(cond2)]

  # If the individual entered the box at least once
  if (length(list2) != 0){

  # For each of the sequences
  for(x in 1:length(list2)){
  # I set my initial value of a as 0
  duration <- 0
  # And sum all the time spent inside the box
  duration <- duration + difftime(list2[[x]][nrow(list2[[x]]), ]$time, list2[[x]][1,]$time, units='m')

  # I indicate this duration spent in the box per individual
  final2[[i]] <- data.frame(id = list2[[1]]$id[1], duration)

  }}

} # end of for loop


# I bind the rows of my list.
final1 <- bind_rows(final1)
final2 <- bind_rows(final2)
final2$duration <- as.numeric(final2$duration)
hist(final2$duration)

# I add a column with the time since these fish were read after the start of the test
  final <- merge(final1, final2, by = 'id', all = T)
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
    mutate(time_since_start = if_else(is.na(time_since_start), 150, time_since_start),
           duration = if_else(is.na(duration), 0, duration))


hist(final$time_since_start)






