#' Get latency : time until an antenna was 'crossed' by an individual
#'
#' @param pit_data A dataframe: data that you get from the PIT antenna
#' @param test_details Dataframe containing the details of the test including the columns "test_ID","Start_time_exp","End_time_exp", "Start_time_pond","End_time_pond"
#'
#' @return a dataframe with antenna number and the time of first detection at the antenna
#'
#' @description pit_data is the data you get directly from the PIT tag readers. It can be for one or multiple tests.
#'   This function is particularly for handling exploration_migration tests in the ponds
#' @export

get_latency_antenna<-function(pit_data, test_details){
  #filter useful cols and add the date-time column here already
  pit_data<-get_useful_cols(pit_data)

  #

}
