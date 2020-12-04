


data(block_df, block_ref_df)
df <- data.frame(id = as.character(NA),
                    time = as.POSIXct(NA))

for (i in 1:(nrow(block_df) - 1)) {
  if (block_df[i, ]$antenna == 41 & block_df[i + 1, ]$antenna == 42) {
    df[i, ] <- block_df[i, c("id", "time")]
  }
  df <- na.omit(df)
  rownames(df) <- NULL

}
