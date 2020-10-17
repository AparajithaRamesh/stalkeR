df <- read_csv("~/Cours/M2 - Sticklebacks/R scripts/2. Exploration box/pondR/pure_data.csv")


# Defining my variables
df$Identifier <- as.integer(df$Identifier)
df$Time <- as.character(df$Time) #this needs to be changed while reading the file
df$Unit.number <- as.integer(df$Unit.number)
df$Transponder.code <- as.character(df$Transponder.code)
df$Actual_time <- dmy_hms(paste(df$Date,df$Time,sep=" "))

# I make a new df with a subset of the variables of interest here
new_dataset<-subset(df, select=c(Identifier, Actual_time, Unit.number, Transponder.code))
names(new_dataset) <- c("Identifier", "time", "antenna", "id")

new_dataset <- subset(new_dataset, antenna==41 | antenna==43 | antenna==44 | antenna==45)

# Distances between antennas
pond.width   <- 1
pond.length  <- 1
pond.diagonal<- 1.25

# Sequence of antenna changes corresponding to the distances above
seq.width  <- c('4541', '4145', '4443', '4344')
seq.length <- c('4544', '4445', '4143', '4341')
seq.diag   <- c('4543', '4345', '4144', '4441')


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
Names <- numeric() # I obtain the individual names
for(i in 1:length(df_list_red)){
  Names[i] <- df_list_red[[i]][1,3]}

# I can generate a data frame with number of changes per individual
df3 <- data.frame(Names, Changes)


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



# I check if individuals might have not been recorded at all by the antennas
non_read_babies <- setdiff(df$`Transponder code`, Travelled.distance$Names)
non_read_babies <- data.frame(Names = non_read_babies, Changes =  0, Dist =  0)

# Final df containing the read (and potential non-read) individuals
Travelled.distance <- rbind(Travelled.distance, non_read_babies)



# We can see that as expected, the correlation between nb of crosses and distance is strong
plot(df4$Changes, df4$Dist)
abline(coef = c(0,1))


