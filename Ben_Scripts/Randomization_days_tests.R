# I randomize what test is conducted which day, and when, each day, the tests are conducted


# Packages
  library(dplyr)

# I define my objects
  days <- c("Mon", "Tue", "Wed", "Thur")
  tests <- c("Shelter", "Exploration_box", "Top", "Bottom")
  nb_of_weeks <- 7
  nb_of_treatment_days <- 28
  

## 1. Loop to randomly decide which test is performed which day of the week
  df <- list()
  for (i in 1:nb_of_weeks){
    df[[i]] <- sample(tests)
  }

# I bind my list of vectors, and rename my columns
  df <- as.data.frame(do.call(rbind, df))
  names(df) <- sample(days)


## 2. Loop to randomly decide which ponds are tested in the morning and afternoon every day
# The randomization is partial, as two ponds per treatments are picked for sure
# every half-day (i.e. morning/afternoon).
  morning <- list()
  afternoon <- list()
  for (i in 1:nb_of_treatment_days){
    morning[[i]] <- c(sample(c(1,2,3,4), 2), sample(c(5,6,7,8), 2))
    afternoon[[i]] <- setdiff(c(1,2,3,4,5,6,7,8), morning[[i]])
  }

# I bind my list of vectors, and rename my columns
  morning <- as.data.frame(do.call(rbind, morning))
  names(morning) <- c("M1", "M2", "M3", "M4")
  afternoon <- as.data.frame(do.call(rbind, afternoon))
  names(afternoon) <- c("A1", "A2", "A3", "A4")

# My final data table, containing the ponds that are being tested in the morning
# and in the afternoon for every of the 28 testing days.
  time <- cbind(morning, afternoon)
