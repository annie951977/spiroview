library(spiroview)

test_that("tests formatData invalid inputs for formatData", {

  # check if file exits
  expect_error(formatData("abcdef"))


  # unsupported file format
  txtPath <- tempfile(fileext = ".txt")
  expect_error(formatData(txtPath))

})

test_that("tests formatData with different column names", {

  headers <- c("id", "gender", "age", "height", "ethnicity", "smoking", "FEV1", "FVC")
  # capitalized headers
  testPath2 <- system.file("extdata",
                          "example_dataset_2.csv",
                          package="spiroview")
  resultDB2 <- formatData(path=testPath2)
  expect_true(setequal(headers, colnames(resultDB2)))


  # reformat gender column
  testPath3 <- system.file("extdata",
                           "example_dataset_3.csv",
                           package="spiroview")
  resultDB3 <- formatData(path=testPath3)
  expect_true(setequal(headers, colnames(resultDB3)))
  expect_true(1 %in% resultDB3$gender)
  expect_true(2 %in% resultDB3$gender)

  # column rename with label input
  testPath4 <- system.file("extdata",
                              "example_dataset_4.csv",
                             package="spiroview")
  resultDB4 <- formatData(path=testPath4,
                         ageLabel= "A")
  expect_true("age" %in% colnames(resultDB4))
})
