library(spiroview)

test_that("tests formatData invalid inputs for formatData", {

  # check if file exits
  expect_error(formatData("abcdef"))


  # unsupported file format
  txtPath <- tempfile(fileext = ".txt")
  expect_error(formatData(txtPath))

})
