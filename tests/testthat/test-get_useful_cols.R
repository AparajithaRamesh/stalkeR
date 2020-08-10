context("check data for columns\n")

testthat::test_that("data has the expected columns after running function", {

  # make some test data
  testdata <- data.frame(Identifier = as.integer(1:1e3),
                                     useless_col1 = as.double(1:1e3),
                                     Time = as.character(rep("00:12:00", 1e3)),
                                     Unit.number=as.double(1:1e3),
                                     Transponder.code= as.character(rep("00075A6A7", 1e3)),
                                     useless_col2=as.character(rep("00:12:00", 1e3)),
                                     Date=as.character(rep("21/07/2020"), 1e3)
                         )

  # expect no error
  testthat::expect_silent(
    dummy<-pondr::get_useful_cols(testdata)
    )
  testthat::expect_length (dummy,4)

  # expect an error
 # testthat::expect_error(expr = {
  #  atl_check_data(data = testdata,
  #                 names_expected = c("X", "Y", "TIME"))
#  })
})
