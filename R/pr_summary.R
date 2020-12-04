#' @title Produces a summary table and a heatmap
#'
#'
#' @description This function takes the reads from an experimental block (\code{block_df}) and a
#' reference list of fish present in this block (\code{block_ref_df}).
#' It produces (i) a summary table containing the reads of each individual per antenna, and the total reads;
#' (ii) a heatmap corresponding to a visual representation of the table.
#'
#' Why do we need both \code{block_df} and \code{block_ref_df}? If all the individuals have been read in \code{block_df},
#' then the individuals present in \code{block_ref_df} and \code{block_df} are the same.
#' However, if some individuals haven't been read in \code{block_df} (e.g. animals haven't moved, lost the tag, are dead),
#' then there will be more individuals in the \code{block_ref_df}.
#'
#'
#'
#'
#' @param block_df A data frame containing the reads from an experimental block
#' @param block_ref_df A data frame containing a reference list with all individuals present in the experimental block.
#'
#' @return A summary table containing the number of reads of each individual (per antenna, and total reads),
#' and its visual representation as a heatmap.
#'
#' @export
#'
#' @examples
#' # Inputs
#' block_7_df <- read.csv("~/block_7_df.txt", sep="")
#' ind_block_7 <- data.frame(id = c("0007E50321", "0007A34978", "0007DF1B76"))
#'
#' # Run the function
#' pr_summary(block_df = block_7_df, ind_block_7)
#'

pr_summary <- function(block_df, block_ref_df){


# I order my individuals based on the total number of reads
  block_ref_df <- block_df %>%
  # Group by individual
    dplyr::group_by(id) %>%
  # Count number of reads
    dplyr::tally() %>%
  # Merge with the reference data frame
    dplyr::right_join(block_ref_df[c("id")], by = "id") %>%
  # Reorder the rows
    dplyr::arrange(desc(n)) %>%
  # Add a column corresponding to this order
    dplyr::mutate(vec = 1:dplyr::n()) %>%
  # Remove the columns with number of reads
    dplyr::select(-`n`)

output <- list()

# Output 1 - number of reads per antenna
output$df <- block_df %>%
  # Group by individuals and antenna
    dplyr::group_by(id, antenna) %>%
  # Obtain number of reads
    dplyr::tally() %>%
  # Merge with the tibble obtained above
    dplyr::right_join(block_ref_df, by = "id") %>%
  # Tibble format
    dplyr::as_tibble() %>%
  # Spread the table horizontally
    tidyr::spread(antenna, n) %>%
  # Reorder the rows based on this total count
    dplyr::arrange(vec) %>%
  # Remove the 'NA' and 'vec' columns
    dplyr::select(-c(`<NA>`, vec)) %>%
  # Add a column containing the total read count
    dplyr::mutate(total = rowSums(.[setdiff(names(.), "id")], na.rm = T))


# Create the tibble for plotting
block_df <- output$df %>%
  # Remove the total column
  dplyr::select(-total) %>%
  # Put the table back at a horizontal format
  tidyr::gather(antenna, n, - id) %>%
  # Merge with the tibble obtained above
  dplyr::right_join(block_ref_df, by = "id") %>%
  # Antenna name as character
  dplyr::mutate(antenna = as.character(antenna))

# Output 2 - heatmap with reads per antenna
output$heatmap <- ggplot2::ggplot(data = block_df,
       # Aesthetics
       ggplot2::aes(x=antenna, y = reorder(id, - vec), fill = n)) +
  ggplot2::geom_tile() +
  # Theme
  ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(family="Source Sans Pro Light"),
        axis.text = ggplot2::element_text(family="Source Sans Pro Light"),
        legend.text = ggplot2::element_text(family="Source Sans Pro Light"),
        legend.title = ggplot2::element_text(family="Source Sans Pro Light"),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),
        panel.background = ggplot2::element_rect(fill = "transparent"))  +
  # Remove labs
  ggplot2::xlab("Antennas") + ggplot2::ylab("Individuals") +
  # Colors
  ggplot2::scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "white")



return(output)
}


# How to output a message indicating that some individuals are missing?
# How to add the function documentation

#block_df <- read.csv("~/Cours/M2 - Sticklebacks/pondr2/Dummy data/block_df.txt", sep="")
#block_ref_df <- read.csv("~/Cours/M2 - Sticklebacks/pondr2/Dummy data/block_ref_df.txt", sep="")

#pr_summary(block_df, block_ref_df)


