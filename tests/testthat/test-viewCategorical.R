library(spiroview)

# viewCategorical

test_that("tests invalid inputs viewCategorical"){

  emptyDF <- data.frame()

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       age=c(22),
                       height=c(1.52),
                       ethnicity=c(1),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # no dataframe, no anything else
  expect_error(viewCategorical(df=emptyDF))

  # only DF
  expect_error(viewCategorical(df=testDF))

  # invalid parameters
  expect_error(viewCategorical(df=testDF,
                             demParam=123,
                             segBy=123))

}


# viewCategoricalCounts
test_that("tests invalid inputs for viewCategoricalCounts"){
  emptyDF <- data.frame()

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       age=c(22),
                       height=c(1.52),
                       ethnicity=c(1),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # no dataframe, no anything else
  expect_error(viewCategoricalCounts(df=emptyDF))

  # dataframe no demParam
  expect_error(viewCategoricalCounts(df=testDF))

  # dataframe, demParam, invalid graph type
  expect_error(viewCategoricalCounts(df=testDF,
                                     demParam="gender",
                                     type="potato"))

}
