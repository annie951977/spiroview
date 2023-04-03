library(spiroview)

test_that("tests invalid inputs", {
  emptyDF <- data.frame()

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       ethnicity=c(1),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # no dataframe, no anything else
  expect_error(summarize(df=emptyDF))

  # only DF
  expect_error(segregateBy(df=testDF))

  # delim doesn't match delimIsNumeric
  expect_error(summarize(df=testDF,
                           demParam = "smoking",
                           delim="1",
                           delimIsNumeric=TRUE,
                           spiroParam="FEV1"))

  expect_error(summarize(df=testDF,
                           demParam = "age",
                           delim=">30",
                           delimIsNumeric=FALSE,
                           spiroParam="FEV1"))


})


test_that("tests categorical delimiter", {
  testDF <- data.frame(id=c(0000, 1111, 2222),
                       gender=c(1, 2, 1),
                       age=c(22, 75, 74),
                       height=c(1.52, 1.83, 1.96),
                       ethnicity=c(1, 2, 1),
                       FEV1=c(2.581, 2.2768045, 0.4895280 ),
                       FVC=c(2.924, 3.0208665, 0.6688253))

  expect_equal(is.data.frame(summarizeAllByCategory(df=testDF,
                                                  demParam="gender",
                                                  delim="1",
                                                  delimIsNumeric = FALSE,
                                                  spiroParam="FEV1")), TRUE)

  # check to see if the output has 2 rows
  expect_equal(nrow(summarizeAllByCategory(df=testDF,
                                                   demParam="gender",
                                                   delim="1",
                                                   delimIsNumeric = FALSE,
                                                   spiroParam="FEV1")), 1)
})


test_that("tests numerical delimiter", {

  testDF <- data.frame(id=c(0000, 1111, 2222),
                       gender=c(1, 2, 1),
                       age=c(22, 75, 74),
                       height=c(1.52, 1.83, 1.96),
                       ethnicity=c(1, 2, 1),
                       FEV1=c(2.581, 2.2768045, 0.4895280 ),
                       FVC=c(2.924, 3.0208665, 0.6688253))

  expect_equal(is.data.frame(summarizeAllByCategory(df=testDF,
                                                   demParam="age",
                                                   delim=">70",
                                                   delimIsNumeric = TRUE,
                                                   spiroParam="FEV1")), TRUE)

  # check to see if the output has 2 rows
  expect_equal(nrow(summarizeAllByCategory(df=testDF,
                                                   demParam="age",
                                                   delim=">70",
                                                   delimIsNumeric = TRUE,
                                                   spiroParam="FEV1")), 1)

})
