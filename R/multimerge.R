#' Merge all csv files in a folder
#'
#' This function merges all the csv files that we get from the TROVAN PIT tag system.
#' It requires that all the datafiles from the same experiment be placed in a single folder
#'
#' @param mypath Path for the folder containing the datafiles.
#' @param extension File extension.
#'
#' @details
#'
#' @examples mydata<-multimerge("C:\\Users\\User\\Documents\\GitHub\\Personality-in-semi-natural-ponds\\data\\raw_data_mixed","*.CSV")
#'
#' @return The combined dataset
#'
#' @note Path should be with 'my/path/' or "my\\path\\". Here only .csv works for now.
#'
#' @export

#multimerge function - merging all the csv files

multimerge = function(mypath,extension)
{
  readfile<-function(filename){read.csv(filename, sep=";")}

  filenames= Sys.glob(file.path(mypath, extension)) ## file_dir = file containing directory
  datalist = lapply(filenames, readfile) #read all files into list datafiles
  as.data.frame(do.call(rbind, datalist)) # merge the list together
}
