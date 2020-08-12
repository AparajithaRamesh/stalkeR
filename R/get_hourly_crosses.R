#' Get hourly crosses
#'
#' @param pit_data A dataframe: data that you get from the PIT antenna
#' @param test_details Dataframe containing the details of the test including the columns "test_ID","Start_time_exp","End_time_exp", "Start_time_pond","End_time_pond"
#'
#' @return a datafrme with crosses between antenna per hour
#'
#' @description pit_data is the data you get directly from the PIT tag readers. It can be for one or multiple tests.
#'   This function is particularly for handling exploration_migration tests in the ponds
#' @export

get_hourly_crosses<-function(pit_data,
                             test_details,
                             method=c("exp", "pond"),
                             total_pond_hours = 16,
                             total_exp_hours = 5){
#filter useful cols and add the date-time column here already
pit_data<-get_useful_cols(pit_data)

  if (method == "exp"){
  }

  if (method == "pond"){
    #starting empty dataset
    tag_id<-as.character()
    test_id<-as.character()
    test_start_time<-as.character()
    hour_num<-as.integer()
    pond_crosses<-as.integer()
    empty_data<-data.frame(tag_id, test_id, test_start_time,hour_num,pond_crosses )

    #loop by tests
    for(j in 1:nrow(test_details)){
      #filter according to test time
      my_data<-pit_data%>%
        dplyr::filter(Actual_time > test_details$Start_time_pond[j] & Actual_time < test_details$End_time_pond[j])

      #number of hours of test
      my_data$crosstime<-as.numeric(my_data$Actual_time-test_details$Start_time_pond[j], units="hours")

  #select particular individual
  num_indiv<-pure1_new%>%
    distinct(Transponder.code)
  total_rows<-nrow(num_indiv)*total_pond_hours #do we need this?? -> yes, to say 0 crosses?

  #initialize a vector
  num_indiv$Transponder.code
  tag_ID<-rep(num_indiv$Transponder.code,round_number_hours) ##dont understand waht this is :/
  pond_crosses<-c(rep(NA,total_rows))
  hour<-rep(1:round_number_hours, each = nrow(num_indiv))
  final_data<-data.frame(tag_ID,hour,pond_crosses)


  #loop for time should be here before looping through individuals since we
  for(j in 1:round_number_hours){
    filter_time<-pure1_new%>%
      filter(crossroundtime>j-1)%>%
      filter(crossroundtime<j)%>%
      arrange(Identifier)

    #loop through individuals in the data
    for(i in 1:nrow(num_indiv)){
      indiv<-filter_time%>%
        filter(Transponder.code==num_indiv[[1]][i])

      #calculate all the pond changes
      pond_crosses<-which(indiv$pond_id != dplyr::lag(indiv$pond_id))
      print (c(num_indiv[[1]][i],length(pond_crosses)))

      final_data$pond_crosses[((j-1)*nrow(num_indiv)+i)]<-length(pond_crosses)
    }
  }
    }
  }
}




