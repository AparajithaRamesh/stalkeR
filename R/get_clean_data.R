#' clean_up
#'
#' @param merged_dataset A dataframe after merging all the excel files from PIT tag data
#' @return a cleaned dataframe ready for getting useful cols
#'
#' @description Sometimes a little bit of previous day's data is included and
#' hence we have to make sure that we
#' remove any duplicates of Identifier
#' (the first column, which is the serial number of recording
#' that the data logger keeps tracks of )

#' @export
#'

get_clean_data<-function(merged_dataset){
  cleaned_merged_dataset<-merged_dataset%>%
    dplyr::distinct(Identifier, .keep_all = TRUE)

  cleaned_merged_dataset
}
