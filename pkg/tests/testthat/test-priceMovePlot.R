context("Test generation of plots")

dt <- data.frame(symbol = c('BCH-USD', 'BTC-USD', 'BCH-USD', 'ETH-USD', 'ETH-USD'),
                  date = c('2017-07-23', '2017-07-24', '2017-07-25', '2017-07-26', '2017-07-27'),
                  open = c(10, 12, 14, 16, 17),
                  close = c(12, 13, 15, 10, 19))

test_that("can build plot: only type ", {
  p<-priceMovePlot(dt,"ETH-USD", type = "close")
  expect_equal("ggplot" %in% class(p), TRUE)
})

test_that("can build plot:  type + start date + end_date", {
  p<-priceMovePlot(dt,"BTC-USD", start_date = "2017-01-01",end_date = "2018-01-01" , type = "open")
  expect_equal("ggplot" %in% class(p), TRUE)
})

test_that("can build plot: type + start date + end_date + brk", {
  p<-priceMovePlot(dt,"BCH-USD", start_date = "2017-07-24",end_date = "2018-07-26" , type = "open", brk = "1 months")
  expect_equal("ggplot" %in% class(p), TRUE)
})

test_that("can manage error in call: wrong parameters", {
  p<-
  error_msg_wrong_type <- "'arg' should be one of \"open\", \"high\", \"low\", \"close\""
  error_msg_not_string <- "*is not a string*"
  error_msg_no_type <- "No type provided or more than one value"
  expect_error(priceMovePlot(dt,"BCH-USD", start_date = "2017-07-24",end_date = "2018-07-26" , type = "opn", brk = "1 months"), error_msg_wrong_type, fixed=TRUE)
  expect_error(priceMovePlot(dt,"BTC-USD", start_date = "2017-07-24",end_date = 7 , type = "open", brk = "1 months"), error_msg_not_string)
  expect_error(priceMovePlot(dt,"ETH-USD"), error_msg_no_type, fixed=TRUE)
  
})

