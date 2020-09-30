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
  
  expect_equal(nrow(testvalue), 96)
  expect_equal(ncol(testvalue), 10)
  
  expect_equal(names(testvalue), c("kvartal", 
                                   "ar",
                                   "diagnoskapitel_text", 
                                   "andel", 
                                   "diagnoskapitel_kod",
                                   "antal","antal_man",
                                   "andel_kvinnor",
                                   "antal_kvinnor",
                                   "andel_man"))
  
  expect_equal(testvalue[c(1,2,3),6] , c("239929", 
                                         "1210", 
                                         "9156"))
  
  expect_equal(testvalue[c(1,2,3),3] , c("Samtliga", 
                                         "Vissa infektionssjukdomar och parasitsjukdomar", 
                                         "Tumörer"))
})

test_that("Encoding is UTF-8", {
  testvalue <- swesick(2005)
  
  expect_equal(Encoding(testvalue$diagnoskapitel_text[3]), "UTF-8")
})


