
#' Title
#'
#' @param raw_df
#' @param id_ref_df
#'
#' @return
#' @export
#'
#' @examples
pr_clean_data <- function(raw_df, id_ref_df){

# Defining my variables
  raw_df$time <- as.character(raw_df$time)
  raw_df$antenna <- as.integer(raw_df$antenna)
  raw_df$id <- as.character(raw_df$id)
  raw_df$time <- lubridate::dmy_hms(paste(raw_df$date, raw_df$time, sep=" "))


# I make a new data frame with a subset of the variables of interest here
  clean_df <- subset(raw_df, select=c("time", "antenna", "id"))

# I only keep the variables of interest
  clean_df <- subset(raw_df, select=c(`time`, `antenna`, `id`))

# I keep one read per second
  clean_df <- dplyr::distinct(clean_df)


# I remove all the reads that do not come from the animals present in the focal experimental
# block (e.g. ghost reads, test reads). I first identify them:
  ghost_reads <- setdiff(clean_df$id, id_ref_df$id)

# Then, I remove all of them from the data frame
  for (i in 1:length(ghost_reads)){
    clean_df <- clean_df[!(clean_df$id == ghost_reads[i]),]
  }

# I reorder the data frame based on time
  clean_df <- clean_df[order(as.POSIXct(clean_df$time)),]

# Reset row names
  rownames(clean_df) <- NULL

return(clean_df)
}


#raw_df <- readr::read_delim("Dummy data/raw_df.CSV", ";", escape_double = FALSE, trim_ws = TRUE)
#id_ref_df <- readxl::read_excel("Dummy data/id_ref_df.xlsx", sheet = "Sheet1")

#pr_clean_data(raw_df, id_ref_df)




