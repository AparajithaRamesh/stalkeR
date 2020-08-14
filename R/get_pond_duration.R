#' Get time spent in ponds
#'
#'
#'
#' @param pit_data A dataframe: data that you get from the PIT antenna
#' @param test_details Dataset with details of the test. It should contain the columns "test_id", "start_time", "end_time"
#' @return The filtered dataset
#'
#' @description pit_data is the cleaned up dataset
#' @export
#Loop through different pairs of start and end times for each test
#needs data set, start and end times of test ----run code from above



get_pond_duration<-function(pit_data, test_details) {
  #check input data
  assertthat::assert_that("data.frame" %in% class(pit_data),
                          msg = "pit_data: input not a dataframe object!")

  assertthat::assert_that("data.frame" %in% class(test_details),
                          msg = "test_details: input not a dataframe object!")

  #filter useful cols and add the date-time column here already
  pit_data<-get_useful_cols(pit_data)

  #starting empty dataset
  tag_id<-as.character()
  test_id<-as.character()
  pond_id<-as.integer()
  entry_time<-as.character()
  exit_time<-as.character()
  empty_data<-data.frame(tag_id, test_id, pond_id, entry_time, exit_time)

  #loop all tests
  for(j in 1:nrow(test_details)){
    #filter according to test times
    my_data<-pit_data%>%
      dplyr::filter(Actual_time > test_details$Start_time_pond[j] & Actual_time < test_details$End_time_pond[j])

    #add pond IDs
    my_new<-my_data%>%
      dplyr::mutate("pond_id"=dplyr::recode(Unit.number, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,"33"=4,"32"=4,"31"=5,
                                            "24" = 1,"23"=2,"22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))%>%
      dplyr::arrange(my_data$Transponder.code)
    #individuals in test
    indiv<-my_new%>%
      dplyr::distinct(Transponder.code)

    #loop through individuals
    for(i in 1:nrow(indiv)){
      tag_id<-indiv[[1]][i]
      my_indiv<-my_new%>%
        dplyr::filter(Transponder.code==tag_id)

      #get the vector of instances of pond crosses
      pond_crosses<-which(my_indiv$pond_id != dplyr::lag(my_indiv$pond_id))

      if(length(pond_crosses)==0){
        entry_time<-test_details$Start_time_pond[j]
        exit_time<-test_details$End_time_pond[j]
        my_indiv_data<-data.frame(tag_id, test_id, pond_id, entry_time, exit_time)
      }

      else {
        entry_time<-c(test_details$Start_time_pond[j], my_indiv$Actual_time[pond_crosses])
        exit_time<-c(my_indiv$Actual_time[pond_crosses-1], test_details$End_time_pond[j])

        start_pond_id<-my_indiv$pond_id[1]
        pond_id<-c(start_pond_id, my_indiv$pond_id[pond_crosses])
        test_id<-rep(test_details$test_ID[j], length(pond_id))

        my_indiv_data<-data.frame(tag_id, test_id, pond_id, entry_time, exit_time)

      }
      empty_data<-rbind(empty_data, my_indiv_data)
    }
  }

  #loop gives entry and exit times for each pond per individual
  #add the time difference between entry and exit to get time_spent per visit
  empty_data<-empty_data%>%
    dplyr::mutate(time_spent_hrs=as.numeric(exit_time-entry_time,units="hours"))

  #need to make sure that pond_id is factor
  empty_data$pond_id<-as.factor(empty_data$pond_id)


  #summarise per individual, per pond the total staying times
  #remove the first row, which is just a placeholder for variable type when the dataframe is empty
  visits<-empty_data%>%
    dplyr::group_by(tag_id,test_id, pond_id)%>%
    dplyr::count()

  time_spent_inponds<-empty_data%>%
    dplyr::group_by(tag_id,test_id, pond_id)%>%
    dplyr::summarise(stay_time=sum(time_spent_hrs))

  final_data<-data.frame(time_spent_inponds, visits$n)

  final_data
}

#to test
#When there are no crosses,is it handling properly
#When a fish missed detection in one pond, how does it handle it
