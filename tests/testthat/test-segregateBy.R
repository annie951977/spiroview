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
  expect_error(segregateBy(df=emptyDF))

  # only DF
  expect_error(segregateBy(df=testDF))

  # invalid parameters
  expect_error(segregateBy(df=testDF,
                           demParam=123,
                           segBy=123))



})

test_that("tests numeric deliminter", {
  testDF <- data.frame(id=c(0000, 1111, 2222),
                       gender=c(1, 2, 1),
                       height=c(1.52, 1.83, 1.96),
                       ethnicity=c(1, 2, 1),
                       FEV1=c(2.581, 2.2768045, 0.4895280 ),
                       FVC=c(2.924, 3.0208665, 0.6688253))

# expect a list output
 expect_true(is.list(segregateBy(df=testDF,
                         demParam = "height",
                         segBy = ">1.80",
                         segIsNumeric = TRUE)), TRUE)

 # expect a list output that's length 2
 expect_equal(length(segregateBy(df=testDF,
                                 demParam = "height",
                                 segBy = ">1.80",
                                 segIsNumeric = TRUE)), 2)

 # expect a dataframe value in the list
 expect_true(is.data.frame(segregateBy(df=testDF,
                                 demParam = "height",
                                 segBy = ">1.80",
                                 segIsNumeric = TRUE)[[1]]), TRUE)


})


test_that("tests categorical deliminter", {
  testDF <- data.frame(id=c(0000, 1111, 2222),
                       gender=c(1, 2, 1),
                       height=c(1.52, 1.83, 1.96),
                       ethnicity=c(1, 2, 1),
                       FEV1=c(2.581, 2.2768045, 0.4895280 ),
                       FVC=c(2.924, 3.0208665, 0.6688253))

  # expect a list output
  expect_true(is.list(segregateBy(df=testDF,
                                  demParam = "gender",
                                  segBy = "1",
                                  segIsNumeric = FALSE)), TRUE)

  # expect a list output that's length 2
  expect_equal(length(segregateBy(df=testDF,
                                  demParam = "gender",
                                  segBy = "1",
                                  segIsNumeric = FALSE)), 2)

  # expect a dataframe value in the list
  expect_true(is.data.frame(segregateBy(df=testDF,
                                        demParam = "gender",
                                        segBy = "1",
                                        segIsNumeric = FALSE)[[1]]), TRUE)


})

