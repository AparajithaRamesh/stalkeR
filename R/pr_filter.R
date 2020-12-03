#'  pr_filter
#'
#' @description This function subsets a clean (and potentially large) data frame based on four different possible parameters:
#' experimental block number/name, individuals id, start time and end time.
#'
#' These parameters are cumulative. If for instance, I chose experimental block 7 with "2020-11-05 12:30:00" as
#' starting time, it will take the reads, within the experimental block 7,
#' that occurred after 2020-11-05 12:30:00.
#'
#'
#' @param clean_df A clean data frame of any size, containing at least one experimental blocks, and potentially many.
#'
#' @param block_nb The number or name of an experimental block (e.g. enclosure, pond, lake).
#'
#' @param ind_names A vector containing the names of all the individuals to subset.
#'
#' @param start_time The time from which the experimental block starts (POSIXct format, see examples).
#'
#' @param end_time The time until which the experimental block lasts (POSIXct format, see examples).
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Define inputs
#' ind_names_1 <- c("0007E50321", "0007A34978", "0007DF1B76")
#' ind_names_2 <- c("Apu", "Jakob", "Franjo")
#' pond_nb <- 7
#' cage_nb <- cage5
#' start_time_5_nov <- as.POSIXct(strptime(c("2020-11-05 12:30:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")
#' end_time_5_nov <- as.POSIXct(strptime(c("2020-11-05 14:00:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")
#'
#' # Run the function with different parameters
#' pr_filter(clean_df, ind_names = ind_names_1)
#' pr_filter(clean_df, block_nb = pond_nb)
#' pr_filter(clean_df, start_time = start_time)
#' pr_filter(clean_df, start_time_5_nov = start_time_5_nov)
#' pr_filter(clean_df, ind_names = ind_names_2, block_nb = cage5, end_time = end_time_5_nov)
#'
pr_filter <- function(clean_df, block_nb, ind_names, start_time, end_time){

  # If given individuals names
  if(!missing(block_nb)) {
    ind_names <- subset(id_ref_df$id, id_ref_df$Pond == block_nb)
    clean_df <- clean_df[clean_df$id %in% ind_names,]
  }


  # If given pond number
  if(!missing(ind_names)) {
    clean_df <- clean_df[clean_df$id %in% ind_names,]
  }


  # If given start time
  if(!missing(start_time)) {
    clean_df <- subset(clean_df, start_time < time)
  }


  # If given end time
  if(!missing(end_time)) {
    clean_df <- subset(clean_df, end_time > time)
  }

  return(clean_df)
}


#raw_df <- readr::read_delim("Dummy data/raw_df.CSV", ";", escape_double = FALSE, trim_ws = TRUE)
#id_ref_df <- readxl::read_excel("Dummy data/id_ref_df.xlsx", sheet = "Sheet1")

#clean_df <- pr_clean_data(raw_df, id_ref_df)

## Inputs
#ind_names <- subset(id_ref_df$id, id_ref_df$Pond == 7)
#block_nb <- 7
#start_time <- as.POSIXct(strptime(c("2020-11-05 12:30:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")
#end_time <- as.POSIXct(strptime(c("2020-11-05 14:00:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")


#pr_filter(clean_df, ind_names = ind_names)
#pr_filter(clean_df, block_nb = 7)
#pr_filter(clean_df, start_time = start_time)
#pr_filter(clean_df, end_time = end_time)
#pr_filter(clean_df, ind_names = ind_names, block_nb = 7, end_time = end_time)
#
# roxygen2::roxygenise()
# ?pr_filter


