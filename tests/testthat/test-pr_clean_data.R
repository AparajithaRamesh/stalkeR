context("check function pr_clean_data \n")

testthat::test_that("data does not have the expected columns after running function pr_clean_data", {

  # make some test data
  raw_df <- data.frame(useless_col1 = as.integer(1:20),
                       useless_col2 = as.double(1:20),
                       time = as.character(c(rep("00:12:00", 5), rep("00:12:01", 5),rep("00:12:00", 5), rep("00:12:01", 5))),
                       antenna = as.integer(rep(c(1,2),10)),
                       id = as.character(c(rep("00075A6A7", 10), rep("0007APU", 10))),
                       useless_col3 = as.character(rep("00:12:00", 20)),
                       date=rep(as.character("21-07-2020"), 20)
  )

  id<-c("00075A6A7","0007APU")
  id_ref_df <- data.frame(id)
  # expect no error
  testthat::expect_silent(
    dummy<-pondr::pr_clean_data(raw_df, id_ref_df)
  )
  testthat::expect_equal(ncol(dummy),3)

  # expect an error
  # testthat::expect_error(expr = {
  #  atl_check_data(data = testdata,
  #                 names_expected = c("X", "Y", "TIME"))
  #  })
})
