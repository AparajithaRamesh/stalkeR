#' Get all crosses
#'
#'
#'
#' @param pit_dataset Given dataset with start
#' and endtimes for filtering, filter tests
#'
#' @return a datafrme with 5 columns containing tag_ID, test_id, exp_crosses, bold_crosses, pond_crosses
#'
#' @description pit_dataset is the data you get directly from the PIT tag readers. It can be for one or multiple tests.
#'   This function is particularly for handling exploration_migration tests in the ponds
#' @export
#Loop through different pairs of start and end times for each test
#needs data set, start and end times of test ----run code from above


get_all_crosses<-function(pit_dataset,
                          test_details){

#filter useful cols and add the date-time column here already
pit_dataset<-get_useful_cols(pit_dataset)

#check data
#pondr::check_data()
#create an empty dataset as a placeholder for data from loops
tag_ID<-"tagID"
exp_crosses<-0
bold_crosses<-0
pond_crosses<-0
test_id<-"test_id"
empty_data<-data.frame(tag_ID, test_id, exp_crosses, bold_crosses, pond_crosses)

numeric()

data.frame(
  'var1' = numeric(),
  'var2' = character()
)
tibble::tibble(
  'var1' = numeric(),
  'var2' = character()
)

###first loop through separate tests
for (i in 1:as.numeric(nrow(test_details)))
  {
  #Filter with start and end time for exploration/boldness
  temp_expbold<- pit_dataset %>%
    dplyr::filter(Actual_time > test_details$Start_time_exp[i] & Actual_time < test_details$End_time_exp[i])

  #filter pond migration data also for the same test
  temp_pond_full<- pit_dataset %>%
    dplyr::filter(Actual_time > test_details$Start_time_pond [i] & Actual_time < test_details$End_time_pond [i])

  #get number of individuals for this test as whole: including exp, bold, mig
  temp_data<-rbind(temp_expbold, temp_pond_full)

  indiv_test<-temp_data%>%
    dplyr::distinct(Transponder.code)

  num_indiv_test<- as.numeric(nrow(indiv_test))

  ###second loop through individuals within a test
  for(j in 1:num_indiv_test)
  {
    #filter focal individual
    focal_indiv<-indiv_test[[1]][j]

    indiv<-temp_data%>%
      dplyr::filter(Transponder.code==focal_indiv)

    #get temp_bold and temp_exp and temp_pond
    temp_explore<-pondr::filter_exp_targets(temp_expbold)%>%
      dplyr::filter(Transponder.code==focal_indiv)
    temp_bold<-pondr::filter_bold_targets(temp_expbold)%>%
      dplyr::filter(Transponder.code==focal_indiv)
    temp_pond<-temp_pond_full%>%
      dplyr::filter(Transponder.code==focal_indiv)

    #calculate all the exp_crosses
    if(nrow(temp_explore) !=0)
    {
      exp_crosses<-which(temp_explore$Unit.number != dplyr::lag(temp_explore$Unit.number))
      n_exp_crosses<-length(exp_crosses)
    }
    else
      n_exp_crosses<-0

    #calculate all the bold_crosses
    if(nrow(temp_bold) !=0)
    {
      bold_crosses<-which(temp_bold$Unit.number != dplyr::lag(temp_bold$Unit.number))
      n_bold_crosses<-length(bold_crosses)
    }
    else
      n_bold_crosses<-0

    #calculate all pond_crosses
    #first rename antenna to give pond number
    temp_pond<-temp_pond%>%
      dplyr::mutate("pond_id"=dplyr::recode(Unit.number, "44"=1,"43"=2,"42"=2,"41"=3,"35"=3,"33"=4,"32"=4,"31"=5,"24" = 1,"23"=2,"22"=2,"21"=3,"14"=3,"13"=4,"12"=4,"11"=5))%>%
      dplyr::arrange(Transponder.code)
    #get the crosses
    if(nrow(temp_pond) !=0)
    {
      pond_crosses<-which(temp_pond$pond_id != dplyr::lag(temp_pond$pond_id))
      n_pond_crosses<-length(pond_crosses)
    }
    else
      n_pond_crosses<-0

    #Now we have the exp, bold, pond crosses for one individual, we need to rbind it to the dataframe
    empty_data<-rbind(empty_data,c(focal_indiv,test_details$test_ID[i],n_exp_crosses,n_bold_crosses,n_pond_crosses))
  }
}
empty_data[-1,]
}
##write the output
#write.csv(empty_data[-1,], "mixed_crosses_new.csv")






