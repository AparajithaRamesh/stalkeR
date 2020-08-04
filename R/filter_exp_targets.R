#' Filter all the exploration targets
#'
#' In these pond experiments, exploration targets (5 targets at the bottom) were kept the same throughout the experiments
#' The antenna numbers (or `Unit number` in the output file) that corresponds to exploration targets are: 21,22,23,24,25,41,42,43,44,45
#' This function filters only the exploration targets from the given dataset
#'
#' @param mydataset Dataframe with the PIT tag data. It should already be filtered according to the time of exploration/boldness experiments
#'
#' @return The filtered dataset
#'
#' @export

#FUNCTION: filter only exploration targets
filter_exp_targets<-function(mydataset)
{
  exp_targets<-c(21,22,23,24,25,41,42,43,44,45)

  mydataset_new<-mydataset%>%
    filter(`Unit number` %in% exp_targets)
  mydataset_new
}
