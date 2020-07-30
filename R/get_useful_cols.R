#' Get only the useful columns of the dataset
#'
#' This function trims the dataset from the original raw data obtained from PIT antenna
#'
#' @param dataset A dataframe: data that you get from the PIT antenna
#'
#' @details The function can work on dataframe that is the output of TROVAN RFID system
#'
#' @return The same dataframe but without the unnecessary columns
#'
#' @export

#getting only required cols from the data
get_useful_cols <- function(dataset) {
  new_dataset<-subset(dataset, select=c(Identifier, Date, Time, `Unit number`, `Transponder code`))
  new_dataset
  }
