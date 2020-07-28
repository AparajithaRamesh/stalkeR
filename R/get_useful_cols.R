#' Get only the useful columns of the dataset
#'
#' This function trims the dataset from the original raw data obtained from PIT antenna
#'
#' @param dataset A dataframe: data that you get from the PIT antenna
#'
#' @details The function can dataframe of any kind
#'
#' @return The same dataframe but without the unnecessary columns
#'
#' @export

#getting only required cols from the data
get_useful_cols <- function(dataset) {
  dataset<-subset(dataset, select=Identifier, Date, Time, `Unit number`, `Transponser code`)
}
