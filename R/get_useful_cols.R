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
  new_dataset<-subset(dataset, select=c(Identifier, Date, Time, Unit.number, Transponder.code))
  new_dataset$Identifier<-as.integer(new_dataset$Identifier)
  new_dataset$Time<-lubridate::seconds_to_period(new_dataset$Time) #this needs to be changed while reading the file
  new_dataset$Unit.number<-as.integer(new_dataset$Unit.number)
  new_dataset$Transponder.code<-as.character(new_dataset$Transponder.code)
  new_dataset$actual_time<-lubridate::dmy_hms(paste(new_dataset$Date,new_dataset$Time,sep=" "))
  new_dataset
  }
