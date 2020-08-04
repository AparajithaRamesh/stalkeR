#' Get all crosses
#'
#'
#'
#' @param mydataset Given dataset with start and endtimes for filtering, filter tests
#' @return The filtered dataset
#'
#'
get_crosses<-function(input_dataset){
  num_indiv<-input_dataset%>%
    distinct(`Transponder code`)
  tag_id<-"tag_id"
  num_crosses<-0
  empty_dataset<-data.frame(tag_id, num_crosses)

  for(i in 1:nrow(num_indiv)){
    focal_indiv<-input_dataset%>%
      filter(`Transponder code`==num_indiv[[1]][i])

    #calculate all the crosses in `Unit number`
    crosses<-which(focal_indiv$`Unit number` != dplyr::lag(focal_indiv$`Unit number`))

    empty_dataset<-rbind(empty_dataset,c(focal_indiv$`Transponder code`[1],crosses))
  }
  empty_dataset[-1,] #delete the first row of placeholder values
}



