## ----setup--------------------------------------------------------------------

library(pondr)


## -----------------------------------------------------------------------------

# I import the example data frame
data(block_ref_df)

# I show the first rows of this example
head(block_ref_df)



## -----------------------------------------------------------------------------

# I import the example data frame
data(raw_df)

# I show the first rows of this example
head(raw_df)


## -----------------------------------------------------------------------------

block_df <- pr_clean_data(raw_df, 
              block_ref_df)

head(block_df)




## ---- fig.width=4, fig.height=5-----------------------------------------------
# I run the function
data_summary <- pr_summary(block_df = block_df, 
           block_ref_df = block_ref_df)

# I create the summary table object
summary_table <- data_summary$df
head(summary_table)

# I create a heatmap showing the number of reads per individual
summary_heatmap <- data_summary$heatmap
summary_heatmap



## -----------------------------------------------------------------------------
sociality_table <- pr_sociality(block_df = block_df,
                                block_ref_df = block_ref_df,
                                cutoff = 15)

head(sociality_table)

## -----------------------------------------------------------------------------
# Antenna of interest
antenna_nb_1 <- 44

# Start and end time
start_time <-
  as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
end_time <-
  as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")

# I run the function
ant_44_table <- pr_latency_reach(
block_df = block_df,
block_ref_df = block_ref_df,
antenna_nb_1,
start_time = start_time,
end_time = end_time,
keep_NA = T,
unit = 's')

head(ant_44_table)


## -----------------------------------------------------------------------------
# Antennas of interest
antenna_nb_2 <- c(41, 42, 43, 44)

# I run my function
mult_ant_table <- pr_latency_reach(
block_df = block_df,
block_ref_df = block_ref_df,
antenna_nb_2,
start_time = start_time,
end_time = end_time,
unit = 'm')

head(mult_ant_table)

## -----------------------------------------------------------------------------
# Sequence of interest
sequence_1 <- c(41, 42)


# Start and end time
start_time <-
  as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
end_time <-
  as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")

# I run the function
pr_latency_cross(block_df, block_ref_df, sequence_1, start_time, end_time)





## -----------------------------------------------------------------------------
# Sequences of interest
sequence_2 <- as.data.frame(rbind(
 c(41, 42),
 c(42, 43)
))

# I run the function
pr_latency_cross(block_df, block_ref_df, sequence_2,
start_time, end_time, keep_NA = TRUE, unit = 's')



## -----------------------------------------------------------------------------
# I load the coordinates of four antennas
data(ant_coordinates)

ant_coordinates

## -----------------------------------------------------------------------------
pr_dist(block_df = block_df, 
        block_ref_df = block_ref_df,
        ant_coordinates = ant_coordinates)


