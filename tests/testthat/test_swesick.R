context("swesick")

test_that("swesick throws error on incorrect inputs", {
  expect_error(swesick(year="2009"))
  expect_error(swesick(year=2004))
  expect_error(swesick(year=(as.numeric(substr(date(), (nchar(date())-3), nchar(date()))))))
})

test_that("output is a dataframe", {
  testframe <- swesick()
  
  expect_true(is.data.frame(testframe))
})

test_that("Output is correct", {
  testvalue <- swesick(2005)
  
  expect_equal(testvalue[c(1,2,3),6] , c("239929", "1210", "9156"))
})