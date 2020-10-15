#########################


co_occurrences_per_ind <- list()
focal <- list()
nb.occ <- numeric()
Shoaling.dfs <- list()
for (x in 1:2){

for (a in 1:nb.individuals){

focal[[a]] <- subset(df_list_ant[[x]], id == individuals[a], drop = F)

for (i in 1:nrow(focal[[a]])){

  # A list. Each df corresponds to the co-occurring reads for a single read of the focal individuals
  list_co_occurrences[[i]] <- subset(df_list_ant[[x]],
                                     abs(difftime(df_list_ant[[x]]$time, focal[[a]]$time[i], units = "s")) <= 3, drop = F)

} # end of time window loop

# I obtain a dataframe with all the reads co-occurring with the focal individuals at a given antenna
# NB: The reads from the focal ind are included too
  co_occurrences_per_ind[[a]] <- bind_rows(list_co_occurrences)

# I remove the reads from the focal individual
  co_occurrences_per_ind[[a]] <- subset(co_occurrences_per_ind[[a]], id != individuals[a], drop = F)
  nb.occ[a] <- c(nrow(co_occurrences_per_ind[[a]]))
} # end of individuals loop

# Number of co-occurrent reads per individual for each antenna
  Shoaling.dfs[[x]] <- data.frame(nb.occ, individuals, df_list_ant[[x]]$antenna[1])

} # end of antenna loop

Shoaling.df <- bind_rows(Shoaling.dfs)
names(Shoaling.df)[3] <- "antenna"
Shoaling.df <- spread(Shoaling.df, antenna, nb.occ)


