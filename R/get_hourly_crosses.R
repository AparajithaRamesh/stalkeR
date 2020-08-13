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

#get the test_details time in posixct format
test_details$Start_time_pond<-as.POSIXct(test_details$Start_time_pond, tz="UTC")
test_details$End_time_pond<-as.POSIXct(test_details$End_time_pond, tz="UTC")
test_details$Start_time_exp<-as.POSIXct(test_details$Start_time_exp, tz="UTC")
test_details$End_time_exp<-as.POSIXct(test_details$End_time_exp, tz="UTC")

#handle the method
  if (method == "exp"){
    #starting empty dataset
    tag_id<-as.character()
    test_id<-as.character()
    test_start_time<-as.character()
    hour_number<-as.integer()
    exp_crosses<-as.integer()
    bold_crosses<-as.integer()
    total_crosses<-as.integer()
    empty_data<-data.frame(tag_id, test_id, test_start_time, hour_number,
                           exp_crosses, bold_crosses, total_crosses)

    #loop by tests
    for(j in 1:nrow(test_details)){

      #filter according to test time for exploration and boldness
      my_data<-pit_data%>%
        dplyr::filter(Actual_time > test_details$Start_time_exp[j]
                      & Actual_time < test_details$End_time_exp[j])

      #total number of individuals detected in test
      num_indiv<-my_data%>%
        dplyr::distinct(Transponder.code)

      #get the time of crossing wrt to start time
      my_data$crosstime<-as.numeric(my_data$Actual_time-test_details$Start_time_exp[j], units="hours")

      #loop for time in hours of test
      for(i in 1:total_exp_hours){
        filter_time<-my_data%>%
          dplyr::filter(crosstime>i-1)%>%
          dplyr::filter(crosstime<i)%>%
          dplyr::arrange(Identifier)

        #loop through individuals in the data
        for(ind in 1:nrow(num_indiv)){
          #select that particular individual
          focal_ind<-num_indiv[[1]][ind]

          indiv<-filter_time%>%
            dplyr::filter(Transponder.code==focal_ind)
          #see if that individual is present in this hour or not
          if(nrow(indiv)==0){
            exp_crosses<-0
            bold_crosses<-0
            total_crosses<-0
            new_row<-c(focal_ind, test_details$test_ID[j],as.character(test_details$Start_time_exp[j]),i,exp_crosses, bold_crosses, total_crosses)
          }

          else{
            #calculate all the exp_crosses
            exp_indiv<-pondr::filter_exp_targets(indiv)
            exp_crosses<-length(which(exp_indiv$Unit.number != dplyr::lag(exp_indiv$Unit.number)))

            #calculate all the bold_crosses
            bold_indiv<-pondr::filter_bold_targets(indiv)
            bold_crosses<-length(which(bold_indiv$Unit.number != dplyr::lag(bold_indiv$Unit.number)))

            #calculate total_crosses
            total_crosses<-length(which(indiv$Unit.number != dplyr::lag(indiv$Unit.number)))

            new_row<-c(focal_ind, test_details$test_ID[j],as.character(test_details$Start_time_exp[j]),i,exp_crosses, bold_crosses, total_crosses)
          }
          #add new row to empty dataset
          empty_data<-rbind(empty_data, new_row)
        }
      }
    }
    colnames(empty_data)<-c("tag_id", "test_id","test_start_time","hour_number","exp_crosses","bold_crosses","total_crosses" )
    empty_data
  }

 else if (method == "pond"){
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
        dplyr::filter(Actual_time > test_details$Start_time_pond[j]
                      & Actual_time < test_details$End_time_pond[j])
      #total number of individuals detected in test
      num_indiv<-my_data%>%
        dplyr::distinct(Transponder.code)

      #give pond_id to Unit.number i.e. the antenna number
      my_data<-my_data%>%
        dplyr::mutate("pond_id"=dplyr::recode(Unit.number, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,"33"=4,"32"=4,"31"=5,
                                              "24" = 1,"23"=2,"22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))

      #get the time of crossing wrt to start time
      my_data$crosstime<-as.numeric(my_data$Actual_time-test_details$Start_time_pond[j], units="hours")

  #loop for time in hours of test
  for(i in 1:total_pond_hours){
    filter_time<-my_data%>%
      dplyr::filter(crosstime>i-1)%>%
      dplyr::filter(crosstime<i)%>%
      dplyr::arrange(Identifier)

    #loop through individuals in the data
    for(ind in 1:nrow(num_indiv)){
      #select that particular individual
      focal_ind<-num_indiv[[1]][ind]

      indiv<-filter_time%>%
        dplyr::filter(Transponder.code==focal_ind)
      #see if that individual is present in this hour or not
      if(nrow(indiv)==0){
        pond_crosses<-0
        new_row<-c(focal_ind, test_details$test_ID[j],as.character(test_details$Start_time_pond[j]),i,pond_crosses)
      }
      else{
        #calculate all the pond changes
        pond_crosses<-length(which(indiv$pond_id != dplyr::lag(indiv$pond_id)))
        new_row<-c(focal_ind, test_details$test_ID[j],as.character(test_details$Start_time_pond[j]),i,pond_crosses)
      }
      #add new row to empty dataset
      empty_data<-rbind(empty_data, new_row)
    }
  }
    }
    colnames(empty_data)<-c("tag_id", "test_id","test_start_time", "hour_number","pond_crosses")
    empty_data
  }
}



