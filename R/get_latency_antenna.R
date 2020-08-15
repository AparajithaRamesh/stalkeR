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

get_latency_antenna<-function(pit_data, test_details,
                              method= c("exp", "pond")){
  #filter useful cols and add the date-time column here already
  pit_data<-get_useful_cols(pit_data)

  #handling method - "exp"
  if(method == "exp"){

    #prepare empty dataset
    tag_id<-as.character()
    test_id<-as.character()
    unique_exp_antenna<-as.integer()
    unique_bold_antenna<-as.integer()
    latency_exp1<-as.double()
    latency_exp2<-as.double()
    latency_exp3<-as.double()
    latency_exp4<-as.double()
    latency_exp5<-as.double()
    latency_bold1<-as.double()
    latency_bold2<-as.double()
    latency_bold3<-as.double()
    latency_bold4<-as.double()
    empty_data<-data.frame(tag_id, test_id, unique_exp_antenna, unique_bold_antenna,
                           latency_exp1, latency_exp2, latency_exp3, latency_exp4,
                           latency_exp5, latency_bold1, latency_bold2, latency_bold3,
                           latency_bold4)

  #loop by tests
  for(j in 1:nrow(test_details)){

      #filter according to test time for exploration and boldness
      exp_bold_data<-pit_data%>%
        dplyr::filter(Actual_time > test_details$Start_time_exp[j]
                    & Actual_time < test_details$End_time_exp[j])

      #get all the individuals detected
      indiv<-exp_bold_data%>%
        dplyr::distinct(Transponder.code)

      #loop through individuals
      for (i in 1:nrow(indiv)){
        unique_exp_antenna<- unique_bold_antenna <- NA
        latency_exp_1<- latency_exp_2<-  latency_exp_3<- latency_exp_4<- NA
        latency_exp_5<- latency_bold_1<- latency_bold_2<- latency_bold_3<- NA
        latency_bold_4 <- NA

        focal_indiv<-indiv[[1]][i]

        my_indiv<-exp_bold_data%>%
          dplyr::filter(Transponder.code==focal_indiv)

        #filter exp data
        exp_data<-filter_exp_targets(my_indiv)
        bold_data<-filter_bold_targets(my_indiv)

        #get unique antennas visited: maybe it breaks if the indiv visited bold antenna but not exp antenna and vice versa
        unique_exp_antenna<-nrow(exp_data%>%
                                 dplyr::distinct(Unit.number))
        unique_bold_antenna<-nrow(bold_data%>%
                                  dplyr::distinct(Unit.number))

        #get the latency times to visit each exp antenna , if they visited any
        if(unique_exp_antenna>0){
          #sort the dataframe according to time
          exp_data<-exp_data%>%
            dplyr::arrange(Actual_time)

         first_visit_exp<-exp_data[!duplicated(exp_data$Unit.number), ]

         for(lat in 1:nrow(first_visit_exp)){
           exp_latency_in_hrs<-as.numeric(first_visit_exp$Actual_time[lat]-test_details$Start_time_exp[j], units="hours")
           assign(glue::glue('latency_exp_',lat), exp_latency_in_hrs)
         }
        }

         #get the latency times to visit each bold antenna , if they visited any
         if(unique_bold_antenna>0){
          #sort the dataframe according to time
          bold_data<-bold_data%>%
           dplyr::arrange(Actual_time)

          first_visit_bold<-bold_data[!duplicated(bold_data$Unit.number), ]

          for(lat in 1:nrow(first_visit_bold)){
            bold_latency_in_hrs<-as.numeric(first_visit_bold$Actual_time[lat]-test_details$Start_time_exp[j], units="hours")
           assign(glue::glue('latency_bold_',lat),bold_latency_in_hrs )
          }
         }
        new_row<-c(focal_indiv,test_details$test_ID[j],unique_exp_antenna, unique_bold_antenna,
                   latency_exp_1, latency_exp_2, latency_exp_3, latency_exp_4,
                   latency_exp_5, latency_bold_1, latency_bold_2, latency_bold_3,
                   latency_bold_4 )
        empty_data<-rbind(empty_data,new_row)
      }
  }
    colnames(empty_data)<-c("tag_id","test_id", "unique_exp_antenna", "unique_bold_antenna",
                            "latency_exp1", "latency_exp2", "latency_exp3", "latency_exp4",
                            "latency_exp5", "latency_bold1", "latency_bold2", "latency_bold3",
                            "latency_bold4")
    empty_data
}
    else if (method == "pond"){
      tag_id<-as.character()
      test_id<-as.character()
      unique_pond_antenna<-as.integer()
      latency_pond1<-as.double()
      latency_pond2<-as.double()
      latency_pond3<-as.double()
      latency_pond4<-as.double()
      latency_pond5<-as.double()
      empty_data<-data.frame(tag_id, test_id, unique_pond_antenna,latency_pond1,latency_pond2,
                             latency_pond3, latency_pond4, latency_pond5)

       #loop by tests
      for(j in 1:nrow(test_details)){
        #filter according to test time for exploration and boldness
        pond_data<-pit_data%>%
          dplyr::filter(Actual_time > test_details$Start_time_pond[j]
                        & Actual_time < test_details$End_time_pond[j])
        #get pond numbers
        pond_data<-pond_data%>%
          dplyr::mutate("pond_id"=dplyr::recode(Unit.number, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,"33"=4,"32"=4,"31"=5,
                                                "24" = 1,"23"=2,"22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))

        #get all the individuals detected
        indiv<-pond_data%>%
          dplyr::distinct(Transponder.code)

        #loop through individuals
        for (i in 1:nrow(indiv)){
          #empty placeholders
          unique_pond_antenna<- NA
          latency_pond_1<-latency_pond_2<-latency_pond_3<-latency_pond_4<-latency_pond_5<- NA

          #get focal individuals
          focal_indiv<-indiv[[1]][i]
          my_indiv<-pond_data%>%
            dplyr::filter(Transponder.code==focal_indiv)

          #get unique antennas visited:
          unique_pond_antenna<-nrow(my_indiv%>%
                                     dplyr::distinct(pond_id))
          #get latency for each pond
          if(unique_pond_antenna>0){
            #sort the dataframe according to time
            my_indiv<-my_indiv%>%
              dplyr::arrange(Actual_time)

            first_visit_pond<-my_indiv[!duplicated(my_indiv$pond_id), ]

            for(lat in 1:nrow(first_visit_pond)){
              pond_latency_in_hrs<-as.numeric(first_visit_pond$Actual_time[lat]-test_details$Start_time_pond[j], units="hours")
              assign(glue::glue('latency_pond_',lat),pond_latency_in_hrs )
            }
          }
          new_row<-c(focal_indiv, test_details$test_ID[j], unique_pond_antenna,latency_pond_1,latency_pond_2,
                     latency_pond_3, latency_pond_4, latency_pond_5)
          empty_data<-rbind(empty_data, new_row)

      }
      }
      colnames(empty_data)<-c("tag_id", "test_id", "unique_pond_antenna","latency_pond1","latency_pond2",
                              "latency_pond3", "latency_pond4", "latency_pond5")

      empty_data
    }
}
