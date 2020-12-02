

raw_df <- readr::read_delim("Dummy data/raw_df.CSV", ";", escape_double = FALSE, trim_ws = TRUE)
id_ref_df <- readxl::read_excel("Dummy data/id_ref_df.xlsx", sheet = "Sheet1")

clean_df <- pr_clean_data(raw_df, id_ref_df)

## Inputs
# Ind names
ind_names <- subset(id_ref_df$id, id_ref_df$Pond == 7)

# Pond number
block_nb <- 7

# start and end time
start_time <- as.POSIXct(strptime(c("2020-11-05 12:30:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")
end_time <- as.POSIXct(strptime(c("2020-11-05 15:00:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")


# If given individuals names
clean_df[clean_df$id %in% ind_names,]

# If given pond number
ind_names <- subset(id_ref_df$id, id_ref_df$Pond == block_nb)
clean_df[clean_df$id %in% ind_names,]

# If given start time
subset(clean_df, start_time < time)

# If given end time
subset(clean_df, end_time > time)






pr_filter <- function(clean_df, block_nb, ind_names, start_time, end_time){


  if(!missing(block_nb)) {
    ind_names <- subset(id_ref_df$id, id_ref_df$Pond == block_nb)
    a <- clean_df[clean_df$id %in% ind_names,]
  }

  else
  if(!missing(block_nb)) {
    a <- clean_df[clean_df$id %in% ind_names,]
  }

  else
  if(!missing(start_time)) {
    a <- subset(clean_df, start_time < time)
  }

  else
  if(!missing(end_time)) {
    a <- subset(clean_df, end_time > time)
  }

  return(a)
}

#pr_filter(clean_df, ind_names = ind_names)
#pr_filter(clean_df, block_nb = 7)
#pr_filter(clean_df, start_time = start_time)
#pr_filter(clean_df, end_time = end_time)
#pr_filter(clean_df, ind_names = ind_names, block_nb = 7, start_time = start_time)
