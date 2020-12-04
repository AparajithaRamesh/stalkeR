#' @title Computes the proportion of time spent with at least one conspecific
#'
#'
#' @description The function computes, for every individual, the proportion of time it has been read in presence of at least
#' one conspecific (read at the same second at the same antenna).
#'
#'
#' @param block_df A data frame containing the reads from an experimental block
#'
#' @param block_ref_df A data frame containing a reference list with all individuals present in the experimental block.
#'
#' @param cutoff A value corresponding to the minimum number of \emph{total reads} (both accompanied and non-accompanied) from which a
#' sociability index (\emph{i.e.} proportion of accompanied reads) should be computed. For example, if an individual has been
#' read only once, by chance, in presence of a conspecific, a score of 100% for this individual is likely non-representative of the
#' real proportion of time it spends in presence of conspecicifs. By default, the value if set at 10 total reads. For no cutoff, one can
#' use a value of 0.
#'
#' @return A data table containing (i) the number of accompanied reads (\emph{i.e.} reads for which at least
#' another individual was present at the same antenna and at the same second); (ii) total number of reads;
#' (iii) proportion of accompanied reads (\code{proportion} = \code{acc_reads} / \code{tot_reads}).
#'
#' @export
#'
#' @examples
#'
#' data(block_df)
#' data(block_ref_df)
#'
#'
#' pr_sociality(block_df,
#'             block_ref_df)
#'
#' pr_sociality(block_df,
#'             block_ref_df,
#'             cutoff = 0)
#'
#'
pr_sociality <- function(block_df,
                        block_ref_df,
                        cutoff = 10) {

  block_df <- block_df %>%
    # Group by antenna and time (for each antenna, within a second)
    group_by(antenna, time) %>%
    # I only keep the reads of individuals that co-occurred with another indiviudal
    filter(n() > 1) %>%
    # Group by individual
    group_by(id) %>%
    # Obtain the number of accompanied reads per individual
    add_tally(name = "acc_reads") %>%
    # Remove time and antenna columns, useless here
    select(-c(time, antenna)) %>%


    # I go back to my iniaial table (one row per read),
    # but with each individual assigned to a nb of accompanied reads
    right_join(block_df["id"]) %>%
    # For every individual (I also keep their number of acc. reads)
    group_by(id, acc_reads) %>%
    # I count the total number of reads (accompanied and non-accompanied)
    summarise(tot_reads = n()) %>%
    # And create a column calculating the proportion between acc reads and total reads
    mutate(proportion = acc_reads / tot_reads) %>%
    # Finally, I add the individuals that haven't been read at all
    right_join(block_ref_df["id"])


  if (missing(cutoff)) {
    block_df[4][block_df[3] < cutoff] <- NA
  }

  return(block_df)
}
