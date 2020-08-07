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

  dataset$Identifier<-as.integer(dataset$Identifier)
  dataset$Time<-as.character(dataset$Time) #this needs to be changed while reading the file
  dataset$Unit.number<-as.integer(dataset$Unit.number)
  dataset$Transponder.code<-as.character(dataset$Transponder.code)
  dataset$actual_time<-lubridate::dmy_hms(paste(dataset$Date,dataset$Time,sep=" "))

  new_dataset<-subset(dataset, select=c(Identifier, actual_time, Unit.number, Transponder.code))
  new_dataset
  }
