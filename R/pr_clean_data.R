#' Clean the raw data from the readers
#'
#' @param raw_df Raw data frame from the PIT / RFID system,
#' already formatted and containing the columns
#' "time", "date", "antenna", "id"  (already formatted). This data frame corresponds to
#' the reads for \emph{one} experimental block.
#' @param block_ref_df Reference data frame for an experimental block
#' containing at least one column "id", with  all
#' individuals and corresponding variables (already formatted)
#'
#' @description \itemize{The function does the following
#' \item{}{Keeps columns of interest (i.e. id, antenna, time)}
#' \item{}{Posixt format for time}
#' \item{}{Keep one read per individual at a certain antenna per second}
#' \item{}{Remove ghost reads}
#' \item{}{Orders the data frame based on time}
#'
#' }
#'
#' @return A data frame (\code{block_df}), with three columns (\emph{i.e.} id, antenna, time),
#' and the number of rows corresponding to the total number
#' of reads (except duplicates within a second) of a experimental block.
#'
#' @export
#'
pr_clean_data <- function(raw_df, block_ref_df) {
  # Defining my variables
  raw_df$time <- as.character(raw_df$time)
  raw_df$antenna <- as.integer(raw_df$antenna)
  raw_df$id <- as.character(raw_df$id)
  raw_df$time <-
    lubridate::dmy_hms(paste(raw_df$date, raw_df$time, sep = " "))


  # I only keep the variables of interest
  block_df <- subset(raw_df, select = c(`time`, `antenna`, `id`))

  # I keep one read per second
  block_df <- dplyr::distinct(block_df)


  # I remove all the reads that do not come from the animals present in the focal experimental
  # block (e.g. ghost reads, test reads). I first identify them:
  ghost_reads <- setdiff(block_df$id, block_ref_df$id)

  # Then, I remove all of them from the data frame


  if (length(ghost_reads) != 0) {
    for (i in 1:length(ghost_reads)) {
      block_df <- block_df[!(block_df$id == ghost_reads[i]), ]
    }
  }

  # I reorder the data frame based on time
  block_df <- block_df[order(as.POSIXct(block_df$time)), ]

  # Reset row names
  rownames(block_df) <- NULL

  return(block_df)
}


#raw_df <- readr::read_delim("Dummy data/raw_df.CSV", ";", escape_double = FALSE, trim_ws = TRUE)
#block_ref_df <- readxl::read_excel("Dummy data/block_ref_df.xlsx", sheet = "Sheet1")

#pr_clean_data(raw_df, block_ref_df)
