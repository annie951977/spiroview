library(spiroview)

# viewNumerical
test_that("tests invalid inputs for viewNumerical", {

  emptyDF <- data.frame()

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       ethnicity=c(1),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # no dataframe, no anything else
  expect_error(viewNumerical(df=emptyDF))

  # only DF
  expect_error(viewNumerical(df=testDF))

  # invalid parameters
  expect_error(viewNumerical(df=testDF,
                           demParam=123,
                           spiroParam=123))

})

# compareNumerical
test_that("tests invalid inputs for compareNumerical", {

  emptyDF <- data.frame()
  testDFThreeValue <- data.frame(id=c(0000, 1111, 2222),
                       gender=c(1, 2, 1),
                       height=c(1.52, 1.83, 1.96),
                       age=c(22, 75, 74),
                       ethnicity=c(1, 2, 1),
                       FEV1=c(2.581, 2.2768045, 0.4895280),
                       FVC=c(2.924, 3.0208665, 0.6688253))


  # no dataframe, no anything else
  expect_error(compareNumerical(df=emptyDF))

  # only DF
  expect_error(compareNumerical(df=testDF))

  # invalid parameters
  expect_error(compareNumerical(df=testDF,
                             demParam=123,
                             spiroParam=123))

  # delim is not numeric
  expect_error(compareNumerical(df=testDF,
                                demParam="height",
                                spiroParam="FEV1",
                                delim="1"))


  # secondDelim is categorical but secondParamIsNumeric is TRUE
  expect_error(compareNumerical(df=testDF,
                                demParam="height",
                                spiroParam="FEV1",
                                delim=">1.8",
                                secondParam="gender",
                                secondDelim="1",
                                secondParamIsNumeric=TRUE))

  # secondDelim is numeric but secondParamIsNumeric is FALSE
  expect_error(compareNumerical(df=testDF,
                                demParam="height",
                                spiroParam="FEV1",
                                delim=">1.8",
                                secondParam="age",
                                secondDelim=">70",
                                secondParamIsNumeric=FALSE))
})
