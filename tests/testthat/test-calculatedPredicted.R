library(spiroview)

# calculateLLNPret

test_that("tests calculateLLNPret invalid inputs for calculateLLNPret", {
  emptyList <- list()
  emptyDF <- data.frame()
  noDemDF <- data.frame(id=c(0000),
                        ethnicity=c(1),
                        FEV1=c(2.581),
                        FVC=c(2.924))

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(1),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # not a dataframe
  expect_error(calculateLLNPret(df=emptyList))

  # no height or age information
  expect_error(calculateLLNPret(df=noDemDF))

  # invalid spiro param option
  expect_error(calculateLLNPret(df=testDF,
                                param="ABC"))

  # invalid reference equation
  expect_error(calculateLLNPret(df=testDF,
                                ref="ABC"))


})


test_that("tests calculateLLNPret single GLI", {
  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(5),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # expect a result
  expect_false(is.na(calculateLLNPret(df=testDF,
                                      param="FEV1",
                                      ref="GLI")))

  # expect a vector of 1
  expect_equal(length(calculateLLNPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")), 1)

})


test_that("tests calculateLLNPret multiple GLI",{
  testDF <- GLIData

  # expect a result
  expect_false(is.null(calculateLLNPret(df=testDF,
                                      param="FEV1",
                                      ref="GLI")))

  # expect a vector of length 50 as a result
  expect_equal(length(calculateLLNPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")), 50)


})

test_that("tests calculateLLNPret single NHANES3", {

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(3),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # expect a result
  expect_false(is.null(calculateLLNPret(df=testDF,
                                      param="FEV1",
                                      ref="NHANES3")))

  # expect a vector of 1
  expect_equal(length(calculateLLNPret(df=testDF,
                                       param="FEV1",
                                       ref="NHANES3")), 1)
})


# calculatePctPret

test_that("tests calculatePctPret invalid inputs for calculatePctPret", {

  emptyList <- list()
  emptyDF <- data.frame()

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(5),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  noDemDF <- data.frame(id=c(0000),
                        ethnicity=c(1),
                        FEV1=c(2.581),
                        FVC=c(2.924))

  testDFOnlyDemo <-data.frame(id=c(0000),
                              gender=c(1),
                              height=c(1.52),
                              ethnicity=c(1))

  # not a dataframe
  expect_error(calculatePctPret(df=emptyList))

  # no spiro param
  expect_error(calculatePctPret(df=testDFOnlyDemo))

  # no height or age information
  expect_error(calculatePctPret(df=noDemDF))

  # invalid spiro param option
  expect_error(calculatePctPret(df=testDF,
                                param="ABC"))

  # invalid reference equation
  expect_error(calculatePctPret(df=testDF,
                                ref="ABC"))

})

test_that("tests calculatePctPret single GLI", {
  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(5),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # expect a result
  expect_false(is.na(calculateLLNPret(df=testDF,
                                      param="FEV1",
                                      ref="GLI")))

  # expect a vector of 1 with param FEV1
  expect_equal(length(calculateLLNPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")), 1)
  # expect a vector of 1, with param FVC
  expect_equal(length(calculateLLNPret(df=testDF,
                                       param="FVC",
                                       ref="GLI")), 1)

})


test_that("tests calculatePctPret multiple GLI", {
  testDF <- GLIData

  # expect a result
  expect_false(is.null(calculatePctPret(df=testDF,
                                      param="FEV1",
                                      ref="GLI")))

  # expect a vector of length 50 as a result
  expect_equal(length(calculatePctPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")), 50)


})

test_that("tests calculatePctPret single NHANES3",{

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(3),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # expect a result
  expect_false(is.null(calculatePctPret(df=testDF,
                                       param="FEV1",
                                       ref="NHANES3")))

  # expect a vector of 1
  expect_equal(length(calculatePctPret(df=testDF,
                                        param="FEV1",
                                        ref="NHANES3")), 1)
})


# calculateMeanPret

test_that("tests invalid inputs for calculateMeanPret", {
  emptyList <- list()
  emptyDF <- data.frame()
  noDemDF <- data.frame(id=c(0000),
                        ethnicity=c(1),
                        FEV1=c(2.581),
                        FVC=c(2.924))

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(5),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # not a dataframe
  expect_error(calculateMeanPret(df=emptyList))

  # no height or age information
  expect_error(calculateMeanPret(df=noDemDF))

  # invalid spiro param option
  expect_error(calculateMeanPret(df=testDF,
                                param="ABC"))

  # invalid reference equation
  expect_error(calculateMeanPret(df=testDF,
                                ref="ABC"))

})

test_that("tests calculateMeanPret single GLI", {
  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(5),
                       FEV1=c(2.581),
                       FVC=c(2.924))

  # expect a result
  expect_false(is.null(calculateMeanPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")))

  # expect a vector of length 1 as a result
  expect_equal(length(calculateMeanPret(df=testDF,
                                        param="FEV1",
                                        ref="GLI")), 1)



})


test_that("tests calculateMeanPret multiple GLI", {
  testDF <- GLIData

  # expect a result
  expect_false(is.null(calculateMeanPret(df=testDF,
                                      param="FEV1",
                                      ref="GLI")))

  # expect a vector of length 50 as a result
  expect_equal(length(calculateMeanPret(df=testDF,
                                       param="FEV1",
                                       ref="GLI")), 50)


})

test_that("tests calculateMeanPret single NHANES3", {

  testDF <- data.frame(id=c(0000),
                       gender=c(1),
                       height=c(1.52),
                       age=c(22),
                       ethnicity=c(3),
                       FEV1=c(2.581),
                       FVC=c(2.924))
  # expect a result
  expect_false(is.null(calculateMeanPret(df=testDF,
                                      param="FEV1",
                                      ref="NHANES3")))

  # expect a vector of 1
  expect_equal(length(calculateMeanPret(df=testDF,
                                       param="FEV1",
                                       ref="NHANES3")), 1)


})

