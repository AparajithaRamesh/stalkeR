#' @title Computes the latency to cross specific sequences of antennas consecutively
#'
#'
#' @param block_df A data frame containing the reads from an experimental block
#'
#' @param block_ref_df A data frame containing a reference list with all individuals present in the experimental block.
#'
#' @param sequence Either a vector of antenna numbers/names, or a data frame containing several sequences \emph{of
#' the same length} (one vector per row), see examples.
#'
#' @param start_time The time from which the experimental block starts (\emph{i.e.POSIXct} format, see examples).
#'
#' @param end_time The time until which the experimental block lasts (\emph{i.e.POSIXct} format, see examples).
#'
#' @param seq_position Which antenna, from the sequence of antennas, should be crossed by an animal to calculate the latency?
#' By default, the first read at the \emph{second antenna} is used to calculate the latency (\code{seq_position = 2}).
#' When \code{seq_position = 1}, the value will correspond to the first read at the \code{first} antenna of the sequence, which might
#' have occurred long before the animal actually crossed the sequence. For example, if the sequence is \code{c(1, 2)}, the
#' individual can have been read at antenna 1 at 12:00:00, and only cross the sequence later (cross antenna 1 at 12:45:03 and
#' cross antenna 2 at 12:45:05). In the later case, if \code{seq_position = 1}, the latency will be computed with 12:00:00, and
#' if \code{seq_position = 2}, the latency will be computed with 12:45:05.
#'
#' @param unit Unit of the latency duration: 'm' as default.
#'
#' @param keep_NA Logical argument. If the individuals are never read by the antenna(s), the latency might be set as the maximal possible value,
#' \emph{i.e.} \code{end_time} - \code{start_time} (default option), or be kept as NA.
#'
#'
#' @return A data frame containing, for each sequence of interest, the latency of each individual to cross it entirely.
#' @export
#'
#' @examples
#'sequence_1 <- as.data.frame(rbind(
#'  c(41, 42),
#'  c(41, 43),
#'  c(41, 44),
#'  c(42, 41),
#'  c(42, 43),
#'  c(42, 44),
#'  c(43, 41),
#'  c(43, 42),
#'  c(43, 44),
#'  c(44, 41),
#'  c(44, 42),
#'  c(42, 43)
#'))
#'
#'
#'sequence_2 <- c(43, 42)
#'
#'
#'start_time <-
#'  as.POSIXct(strptime(c("2020-11-05 12:30:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
#'end_time <-
#'  as.POSIXct(strptime(c("2020-11-05 15:00:00"), "%Y-%m-%d %H:%M:%OS"), "UTC")
#'
#' pr_latency_cross(block_df, block_ref_df, sequence_1, start_time, end_time)
#' pr_latency_cross(block_df, block_ref_df, sequence_2, start_time, end_time, keep_NA = T, unit = 's')
#'
#'
pr_latency_cross <- function(block_df,
                         block_ref_df,
                         sequence,
                         start_time,
                         end_time,
                         seq_position = 2,
                         unit = 'm',
                         keep_NA = FALSE) {
  # Function to obtain the latency of each individual to cross one sequence
  lat_cross <- function(block_df, sequence) {
    # Define objects
    list_df <- list()

    # Split block_df per individual
    block_df_id <- split(block_df, f = block_df$id)
    # For each individual


    for (i in 1:length(block_df_id)) {
      # First reorder by time
      block_df_id[[i]][order(as.POSIXct(block_df_id[[i]]$time, "UTC")),]

      # I obtain the row nb of the antenna changes
      changes          <-
        which(block_df_id[[i]]$antenna != dplyr::lag(block_df_id[[i]]$antenna))

      # Corresponding antenna numbers
      antenna          <-
        c(block_df_id[[i]]$antenna[1], block_df_id[[i]]$antenna[changes])

      # Corresponding time
      time             <-
        c(block_df_id[[i]]$time[1], block_df_id[[i]]$time[changes])
      attr(time, "tzone") <- "UTC"

      # Name of individual
      id               <-
        c(block_df_id[[i]]$id[1], block_df_id[[i]]$id[changes])

      # Obtain a data frame with each antenna change per individual
      block_df_id[[i]] <- data.frame(antenna, time, id)

      # If there is at least one antenna change
      if (nrow(block_df_id[[i]]) > 1) {
        # reversed sequence
        sequence.rev <- rev(sequence)

        # Embed time series (?)
        w <- embed(block_df_id[[i]]$antenna, length(sequence))

        # I subset one row from the crossing sequence of interest
        # By default, the read at the second antenna of the sequence is selected (seq_position = 2)
        list_df[[i]] <-
          block_df_id[[i]][which(rowSums(w == rep(sequence.rev, each = nrow(w))) == ncol(w)) + (seq_position - 1), ]


        # I make a data frame containing the name of the individual, and its latency to cross the sequence of interest
        list_df[[i]] <- data.frame(id = list_df[[i]]$id[1],
                                   lat = as.numeric(difftime(
                                     list_df[[i]]$time[1], start_time, unit = unit
                                   )))

      } # end of if
    } # end of for loop

    # I obtain a data frame containing all the individuals that crossed the sequence of interest, and
    # the corresponding latency to reach one of the antenna from the sequence
    list_df <- dplyr::bind_rows(list_df)

    # Remove rows with NAs
    list_df <- na.omit(list_df)

    # Reset row names
    rownames(list_df) <- NULL

    # I name the latency column based on the sequence of interest
    names(list_df)[2] <-
      paste0("lat_", paste0(sequence, collapse = "_"))

    return(list_df)

  } # end of lat_cross function

  # Define objects
  latency_per_seq <- list()

  # If the input vector is a vector, convert it to a one row data frame
  if (is.vector(sequence) == TRUE) {
    sequence <- as.data.frame(rbind(sequence))

  }

  # For every sequence of interest
  for (i in 1:nrow(sequence)) {
    # I run the lat_cross function and obtain the individuals latency to cross it
    latency_per_seq[[i]] <- lat_cross(block_df, sequence[i, ])
  }

  # I merge all data frames contained in latency_per_seq
  output <- Reduce(function(x, y)
    merge(x, y, all = TRUE), latency_per_seq)

  # If keep_NA is FALSE, I give the maximal value for the latency scores
  if (keep_NA == FALSE) {
    output[is.na(output)] <- difftime(end_time, start_time, unit = 'm')
  }


  return(output)
}
