
square_df_raw <- read_csv("~/Cours/M2 - Sticklebacks/Data/50. Corona experiment/square_final.csv")
square_df <- square_df_raw






## 1. CLEAN DATA
# Defining my variables
square_df$Identifier <- as.integer(square_df$Identifier)
square_df$Time <- as.character(square_df$Time)
square_df$Unit.number <- as.integer(square_df$`Unit.number`)
square_df$Transponder.code <- as.character(square_df$`Transponder.code`)
square_df$Actual_time <- dmy_hms(paste(square_df$Date,square_df$Time,sep=" "))

# I make a new data frame with a subset of the variables of interest here
clean_square_df<-subset(square_df, select=c(Actual_time, `Unit.number`, `Transponder.code`))
names(clean_square_df) <- c("time", "antenna", "id")


# I keep one read per second
clean_square_df <- clean_square_df  %>% distinct()

# I indicate in which pond the individuals are
clean_square_df <- clean_square_df %>%
  mutate("pond_id"= recode(antenna, "21"=1,"14"=1,"22"=2,"23"=2,"24"=3,"11"=3, "12"=4,"13"=4,
                                 "41"=5,"35"=5,"42"=6, "43"=6, "44"=7, "31"=7, "32"=8,"33"=8))
# I reorder based on time
clean_square_df <- clean_square_df[order(as.POSIXct(clean_square_df$time)),]

clean_square_df <- clean_square_df %>%
  mutate("status" = case_when((pond_id == 1 | pond_id == 2|pond_id == 3| pond_id == 4) ~ "Migrant",
                              (pond_id == 5 | pond_id == 6|pond_id == 7| pond_id == 8) ~ "Resident", TRUE ~ "NA"))



variables_df <- unique(clean_square_df[c("id", "status")])


## 2. ANTENNA CHANGES
# Define objects
clean_square_list_changes <- list()


# I split my data frame by id
clean_square_list <- split(clean_square_df, f = clean_square_df$id)

# For each individual
for(i in 1:length(clean_square_list)){

    changes          <- which(clean_square_list[[i]]$pond_id!= lag(clean_square_list[[i]]$pond_id))
    pond_id          <- c(clean_square_list[[i]]$pond_id[1], clean_square_list[[i]]$pond_id[changes])
    time             <- c(clean_square_list[[i]]$time[1], clean_square_list[[i]]$time[changes])
    id               <- c(clean_square_list[[i]]$id[1], clean_square_list[[i]]$id[changes])
    clean_square_list_changes[[i]] <- data.frame(pond_id, time, id)

  }




## 3. Obtain mean step length
step.length <- function(changes_df_id){

  if (nrow(changes_df_id) > 1){
  # data example
  # changes_df_id <- clean_square_list_changes[[4]]

# Define objects
  turnarounds <- c()
  steps <- c()

# For every antenna change
  for(n in 2:nrow(changes_df_id)) {

    # where did 'row i-1' and 'row i+1' have the same value (i.e. turn around)?
    turnarounds[n] <- c(changes_df_id$pond_id[n - 1] == changes_df_id$pond_id[n + 1])
  }

# I obtain the position of the turnarounds
  turnarounds <- which(turnarounds, arr.ind = FALSE)

# I want to cut *after* the turnarounds
  turnarounds <- turnarounds + 1

# I split my data frame each time the individual turnes around
list <- split(changes_df_id, cumsum(1:nrow(changes_df_id) %in% turnarounds))


# For every step forward (i.e. sequence with no turnaround)
for (i in 1:length(list)){
  # What how long is this step
  steps[i] <- nrow(list[[i]])
} # end of for loop

mean_steps <- data.frame(id = changes_df_id$id[1],
                         Mean_length = mean(steps),
                         SD_length = sd(steps))}


  if (nrow(changes_df_id) == 1){

    mean_steps <- data.frame(id = changes_df_id$id[1],
                             Mean_length  = 1,
                             SD_length = NA)
  }


return(mean_steps)
} # end of function



step_length_df <- data.frame(id = NA, Mean_length  = NA, SD_length = NA)
for (i in 1:40){
step_length_df[i,] <- step.length(clean_square_list_changes[[i]])}



step_length_df <- merge(step_length_df, variables_df, by = "id")



square_plot <- ggplot(data = step_length_df, aes(x = status, y = Mean_length )) +
  geom_violin(fill = "#e4e3e8", draw_quantiles = c(0.5)) +
  #geom_boxplot(fill = "white", width = 0.1, outlier.alpha = 0.3) +
  theme(panel.grid.major.y = element_line(colour = "#d4d4d4"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(colour = "#e6e6e6"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(color = "black")) +
  labs(y = "Step length", x =" ") +

  geom_point(position = position_jitter(width = 0.02), alpha = 0.1)+ ylim(0,5) + ggtitle('Square (n = 40)')









##############################################################################
# Line set up

line_data <- read_csv("~/Cours/M2 - Sticklebacks/Data/50. Corona experiment/line_data.csv")
line_data_variables <- read_csv("~/Cours/M2 - Sticklebacks/Data/50. Corona experiment/line_data_variables.csv")
line_data_variables <- unique(line_data_variables[c("tag_ID", "status")])
names(line_data_variables)[1] <- 'id'

# I make a new data frame with a subset of the variables of interest here
clean_line_data<-subset(line_data, select=c(Actual_time, `Unit.number`, `Transponder.code`))
names(clean_line_data) <- c("time", "antenna", "id")


# I keep one read per second
clean_line_data <- clean_line_data  %>% distinct()

# I indicate in which pond the individuals are

clean_line_data<-clean_line_data%>%
  mutate("pond_id"=recode(antenna, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,
                                                     "33"=4,"32"=4,"31"=5,"24"=1,"23"=2,
                                                     "22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))


# I reorder based on time
clean_line_data <- clean_line_data[order(as.POSIXct(clean_line_data$time)),]



## 2. ANTENNA CHANGES
# Define objects
clean_line_list_changes <- list()


# I split my data frame by id
clean_line_list <- split(clean_line_data, f = clean_line_data$id)

# For each individual
for(i in 1:length(clean_line_list)){

  changes          <- which(clean_line_list[[i]]$pond_id!= lag(clean_line_list[[i]]$pond_id))
  pond_id          <- c(clean_line_list[[i]]$pond_id[1], clean_line_list[[i]]$pond_id[changes])
  time             <- c(clean_line_list[[i]]$time[1], clean_line_list[[i]]$time[changes])
  id               <- c(clean_line_list[[i]]$id[1], clean_line_list[[i]]$id[changes])
  clean_line_list_changes[[i]] <- data.frame(pond_id, time, id)

}


clean_line_list_changes[[2]]


step.length(clean_line_list_changes[[2]])


step_length_df <- data.frame(id = NA, Mean_length  = NA, SD_length = NA)
for (i in 1:107){
  step_length_df[i,] <- step.length(clean_line_list_changes[[i]])}





step_length_df <- merge(step_length_df, line_data_variables, by = "id")


line_plot <- ggplot(data = step_length_df, aes(x = status, y = Mean_length )) +
  geom_violin(fill = "#e4e3e8", draw_quantiles = c(0.5)) +
  #geom_boxplot(fill = "white", width = 0.1, outlier.alpha = 0.3) +
  theme(panel.grid.major.y = element_line(colour = "#d4d4d4"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(colour = "#e6e6e6"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(color = "black")) +
  labs(y = "Step length", x =" ") +

  geom_point(position = position_jitter(width = 0.02), alpha = 0.1) + ylim(0,5) + ggtitle('Line (n = 107)')


(square_plot + line_plot)







