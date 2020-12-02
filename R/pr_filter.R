#' Title
#'
#' @param clean_df
#' @param block_nb
#' @param ind_names
#' @param start_time
#' @param end_time
#'
#' @return
#' @export
#'
#' @examples
pr_filter <- function(clean_df, block_nb, ind_names, start_time, end_time){

  # If given individuals names
  if(!missing(block_nb)) {
    ind_names <- subset(id_ref_df$id, id_ref_df$Pond == block_nb)
    a <- clean_df[clean_df$id %in% ind_names,]
  }

  else
  # If given pond number
  if(!missing(ind_names)) {
    a <- clean_df[clean_df$id %in% ind_names,]
  }

  else
  # If given start time
  if(!missing(start_time)) {
    a <- subset(clean_df, start_time < time)
  }

  else
  # If given end time
  if(!missing(end_time)) {
    a <- subset(clean_df, end_time > time)
  }

  return(a)
}


#raw_df <- readr::read_delim("Dummy data/raw_df.CSV", ";", escape_double = FALSE, trim_ws = TRUE)
#id_ref_df <- readxl::read_excel("Dummy data/id_ref_df.xlsx", sheet = "Sheet1")

#clean_df <- pr_clean_data(raw_df, id_ref_df)

## Inputs
#ind_names <- subset(id_ref_df$id, id_ref_df$Pond == 7)
#block_nb <- 7
#start_time <- as.POSIXct(strptime(c("2020-11-05 12:30:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")
#end_time <- as.POSIXct(strptime(c("2020-11-05 15:00:00"),"%Y-%m-%d %H:%M:%OS"),"UTC")


#pr_filter(clean_df, ind_names = ind_names)
#pr_filter(clean_df, block_nb = 7)
#pr_filter(clean_df, start_time = start_time)
#pr_filter(clean_df, end_time = end_time)
#pr_filter(clean_df, ind_names = ind_names, block_nb = 7, start_time = start_time)
