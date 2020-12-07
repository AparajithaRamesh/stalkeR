#' @title Distance travelled by an individual
#'
#' @description Distance travelled: eucleadian distance
#' travelled by fish: block_df, block_ref_id_df,
#' block_ref_antenna_df, output gives indiv, distance
#' travelled, 0 for indiv that got read, didnt move and
#'  NA for indiv that did not get read
#'
#' @param block_df A data frame containing the reads from an experimental block
#' @param block_ref_df A data frame containing a reference list with all individuals present in the experimental block.
#' @param ant_coordinates A data frame containing  "antenna" and coordinates, "x","y","z" in other columns. If there is no z axis, enter all zeros but do not omit the column
#' @return A data frame containing the "id" and corresponding "distance" travelled by fish
#'
#' @export
#'
#'

pr_dist<-function (block_df, block_ref_df, ant_coordinates){
  #create an empty data frame which will be filled by individuals
  focal_id <- "tagid"
  distance <- 0
  empty_data<-data.frame(focal_id, distance)

  #get all individuals that are read in block_df
  indiv<-block_df%>%
    dplyr::distinct(id)

  #subset block_df by individuals
  for(i in 1:nrow(indiv)){
    #get the first individual identity
    focal_id <- indiv[[1]][i]

    #temporary dataset for that individual
    temp_dataset<- block_df %>%
      dplyr::filter(id ==focal_id)

    #get the crosses using dplyr lag
    antenna<-temp_dataset$antenna [which(temp_dataset$antenna != dplyr::lag(temp_dataset$antenna))]
    antenna<-data.frame(antenna)

    #starting distance is zero
    distance <- as.numeric(0)

    #compute distance only for fish that don't move
    if(nrow(antenna)!= 0){

      #get the coordinates vector of the antenna changes
      coordinates<- dplyr::inner_join(antenna, ant_coordinates, by="antenna" )

      #loop to get pairwise distances between antenna
      if(nrow(coordinates)>1){
        for (j in 1:(nrow(coordinates)-1) ){
          distance <- distance + sqrt((coordinates$x[j]-coordinates$x[j+1])^2 +
                                        (coordinates$y[j]-coordinates$y[j+1])^2 +
                                        (coordinates$z[j]-coordinates$z[j+1])^2)
        }
      }
    }
    empty_data<-rbind(empty_data, c(focal_id,distance))
  }
  colnames(empty_data)<-c("id", "distance")
  empty_data[-1,]
}
