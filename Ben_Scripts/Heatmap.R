#'
#' Obtain a summary of your read counts.
#'
#' @param block_df A data frame containing all reads for one experimental block
#' @param block_ref_df A reference data frame with all individuals in the experimental block
#'
#'
#'
#'
#'
#'
#' @export


pr_summary <- function(block_df, block_ref_df){
  usethis::use_pipe()
# I order my individuals based on the total number of reads
  dplyr::block_ref_df <- block_df %>%
  # Group by individual
  dplyr::group_by(id) %>%
  # Count number of reads
  dplyr::tally() %>%
  # Merge with the reference data frame
  dplyr::right_join(l_fish[c("id")], by = "id") %>%
  # Reorder the rows
  dplyr::arrange(desc(n)) %>%
  # Add a column corresponding to this order
  dplyr::mutate(vec = 1:n()) %>%
  # Remove the columns with number of reads
  dplyr::select(-`n`)


# Count reads per antenna and order based on total reads
block_df <- block_df %>%
  # Group by individuals and antenna
  dplyr::group_by(id, antenna) %>%
  # Obtain number of reads
  dplyr::tally() %>%
  # Merge with the tibble obtained above
  dplyr::right_join(block_ref_df, by = "id") %>%
  # Replace NAs per zero
  dplyr::mutate(n = replace_na(n, 0))

output <- list()
# Output 1 - heatmap with reads per antenna
output$heatmap <- ggplot2::ggplot(data = block_df,
       # Aesthetics
       aes(x=antenna, y=reorder(id, -vec), fill = n)) +
  # Heatmap
  geom_tile() +
  # Theme
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_rect(fill = "transparent"))  +
  # Remove labs
  xlab("Antennas") + ylab("Individuals") +
  # Colors
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  # Discrete breaks between antenna identifier
  scale_x_continuous(breaks=df$antenna)


# Output 2 - table with antenna and total read counts
output$table <- block_df %>%
  # Tibble format
  dplyr::as_tibble()%>%
  # Spread the table horizontally
  dplyr::spread(antenna, n) %>%
  # Remove the 'NA' and 'vec' columns
  dplyr::select(-c(`<NA>`, vec)) %>%
  # Add a column containing the total read count
  dplyr::mutate(total = rowSums(.[setdiff(names(.), "id")])) %>%
  # Reorder the rows based on this total count
  dplyr::arrange(desc(total)) %>%
  # Replace NAs per zero
  dplyr::mutate_all(funs(replace_na(.,0)))

return(output)
}




