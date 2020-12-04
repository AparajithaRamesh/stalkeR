#' @title Computes the latency to reach one or several antennas
#'
#' @description This function computes, for every individual, the latency to reach (\emph{i.e.} be read at) certain antennas.
#' This can for example be used in the context of an exploration test, in which individuals enter an exploration box,
#' in which they are read by certain antennas.
#'
#' The function computes the difference between the initial time (time at which the antennas were introduced/started to record)
#' and the first read at one or several \emph{antennas of interest}. Individuals that were not read at these antennas (notably obtained
#' from \code{block_ref_df}) receive a score corresponding to the maximal possible duration by default.
#'
#' @param block_df A data frame containing the reads from an experimental block
#'
#' @param block_ref_df A data frame containing a reference list with all individuals present in the experimental block.
#'
#' @param antenna_nb If there is one antenna of interest, then antenna_nb corresponds to its number/name (\code{antenna_nb_1} in examples).
#' However, if there are several antennas of interest, the argument should be av vector of antenna names/numbers (\code{antenna_nb_2} in examples).
#'
#' @param start_time The time from which the experimental block starts (\emph{i.e.POSIXct} format, see examples).
#'
#' @param end_time The time until which the experimental block lasts (\emph{i.e.POSIXct} format, see examples).
#'
#' @param keep_NA Logical argument. If the individuals are never read by the antenna(s), the latency might be set as the maximal possible value,
#' \emph{i.e.} \code{end_time} - \code{start_time} (default option), or be kept as NA.
#'
#' @param unit Unit of the latency duration: m as default.
#'
#' @return A data frame containing the latency, for every individual, to enter the antenna(s) of interest.
#' @export
#'
#' @examples
#'
#' data(block_df)
#' data(block_ref_df)
#' antenna_nb_1 <- 44
#' antenna_nb_2 <- c(41, 42, 43, 44)
#' start_time <-
#'   as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
#' end_time <-
#'   as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
#'
#'
#'
#' pr_latency_reach(
#' block_df,
#' block_ref_df,
#' antenna_nb_2,
#' start_time,
#' end_time,
#' keep_NA = T,
#' unit = 's')
#'
#' pr_latency_reach(
#' block_df,
#' block_ref_df,
#' antenna_nb_1,
#' start_time,
#' end_time,
#' keep_NA = T,
#' unit = 'm')
pr_latency_reach <-

  function(block_df,
           block_ref_df,
           antenna_nb,
           start_time,
           end_time,
           keep_NA = FALSE,
           unit = 'm') {
    # If there is only one antenna of interest
    if (length(antenna_nb) == 1) {
      # Subset reads at this antenna
      block_df <- subset(block_df, antenna == antenna_nb) %>%
        # Reorder by time
        dplyr::arrange(time) %>%
        # Group by individual
        dplyr::group_by(id) %>%
        # Keep the first read per individual
        dplyr::filter(row_number() == 1) %>%
        # Create a column computing the latency between first read and start time
        dplyr::mutate(latency = as.numeric(difftime(time, start_time, units = unit))) %>%
        # Only keep the latency and individual column
        dplyr::select(-c(time, antenna))
    }

    # If more than one antennas of interest
    if (length(antenna_nb) > 1) {
      # Keep reads at antennas of interest
      block_df <- block_df[block_df$antenna %in% antenna_nb, ] %>%
        # reorder by time
        dplyr::arrange(time) %>%
        # Group by antennas and individual id
        dplyr::group_by(antenna, id) %>%
        # Keep first reads of each individual at the different antennas
        dplyr::filter(row_number() == 1) %>%
        # Create a column computing the latency between first read and start time
        dplyr::mutate(latency = as.numeric(difftime(time, start_time, units = unit))) %>%
        # Remove the time column
        dplyr::select(-time) %>%
        # Spread the table horizontally (one column per antenna)
        tidyr::pivot_wider(names_from = antenna , values_from = latency)
    }

    # If keep_NA is FALSE, or if nothing is indicated
    if (keep_NA == FALSE) {
      # Obtain the difference between the start and the end time
      max_latency <-
        as.numeric(difftime(end_time, start_time, units = unit))


      block_df <- block_df %>%
        # Merge by individuals in block_ref_df (all individuals are kept)
        dplyr::right_join(block_ref_df["id"]) %>%
        # In numeric columns, I replace NAs per max_latency
        dplyr::mutate_if(is.numeric, ~ replace(., is.na(.), max_latency))
    }
    return(block_df)
  }






