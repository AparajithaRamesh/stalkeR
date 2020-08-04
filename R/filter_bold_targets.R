#' Filter all the boldness targets
#'
#' In these pond experiments, boldness targets (4 targets at the top) were kept the same throughout the experiments
#' The antenna numbers (or `Unit number` in the output file) that corresponds to boldness targets are: 11,12,13,14,31,32,33,35
#' This function filters only the boldness targets from the given dataset
#'
#' @param mydataset Dataframe with the PIT tag data. It should already be filtered according to the time of exploration/boldness experiments
#'
#' @return The filtered dataset
#'
#' @export

#FUNCTION: filter only exploration targets
filter_bold_targets<-function(mydataset)
{
  bold_targets<-c(11,12,13,14,31,32,33,35)

  mydataset_new<-mydataset%>%
    filter(`Unit number` %in% bold_targets)
  mydataset_new
}
