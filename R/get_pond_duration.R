#' Get time spent in ponds
#'
#'
#'
#' @param pit_pond Given dataset with start and endtimes for filtering, filter tests
#' @param test_details Dataset with details of the test. It should contain the columns "test_id", "start_time", "end_time"
#' @return The filtered dataset
#'
#'@description pit_dataset is the cleaned up dataset
#'@export
#Loop through different pairs of start and end times for each test
#needs data set, start and end times of test ----run code from above



get_pond_duration<-function(pit_pond, test_details) {

  #check input data
  assertthat::assert_that("data.frame" %in% class(pit_pond),
                          msg = "pit_pond: input not a dataframe object!")

  assertthat::assert_that("data.frame" %in% class(test_details),
                          msg = "test_details: input not a dataframe object!")

  #add pond IDs
  my_data<-pit_pond%>%
    mutate("pond_id"=recode(Unit.number, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,"33"=4,"32"=4,"31"=5,
                            "24" = 1,"23"=2,"22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))%>%
    arrange(my_data$Transponder.code)

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
    my_new<-my_data%>%
      filter(Actual_time > test_details$Start_time_pond[j] & Actual_time < test_details$End_time_pond[j])

    #individuals in test
    indiv<-my_new%>%
      distinct(Transponder.code)

    #loop through individuals
    for(i in 1:nrow(indiv)){
      tag_id<-indiv[[1]][i]
      my_indiv<-my_new%>%
        filter(Transponder.code==tag_id)

      #get the vector of instances of pond crosses
      pond_crosses<-which(my_indiv$pond_id != lag(my_indiv$pond_id))
      entry_time<-c(test_details$Start_time_pond[j], my_indiv$Actual_time[pond_crosses])
      exit_time<-c(my_indiv$Actual_time[pond_crosses-1], test_details$End_time_pond[j])

      start_pond_id<-my_indiv$pond_id[1]
      pond_id<-c(start_pond_id, my_indiv$pond_id[pond_crosses])
      test_id<-rep(test_details$test_ID[j], length(pond_id))

      my_indiv_data<-data.frame(tag_id, test_id, pond_id, entry_time, exit_time)

      empty_data<-rbind(empty_data, my_indiv_data)
    }
  }

  #loop gives entry and exit times for each pond per individual
  #add the time difference between entry and exit to get time_spent per visit
  empty_data<-empty_data%>%
    mutate(time_spent_hrs=as.numeric(exit_time-entry_time,units="hours"))

  #need to make sure that pond_id is factor
  empty_data$pond_id<-as.factor(empty_data$pond_id)

  #summarise per individual, per pond the total staying times
  #remove the first row, which is just a placeholder for variable type when the dataframe is empty
  count<-empty_data[-1,]%>%group_by(tag_id,test_id, pond_id)%>%count()

  time_spent_inponds<-empty_data[-1,]%>%group_by(tag_id,test_id, pond_id)%>%summarise(stay_time=sum(time_spent_hrs), n=n())

  final_data<-data.frame(time_spent_inponds, count$n)

  final_data
}
