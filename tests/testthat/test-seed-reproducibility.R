test_that("safe_sample handles length-1 vectors correctly", {
  set.seed(123)
  # safe_sample should always return the single element, not sample from 1:x
  result1 <- fect:::safe_sample(17, 1)
  expect_equal(result1, 17)
  
  result2 <- fect:::safe_sample(17, 5, replace = TRUE)
  expect_equal(result2, rep(17, 5))
  
  # For length > 1, should behave like regular sample
  set.seed(123)
  result3 <- fect:::safe_sample(1:10, 3)
  set.seed(123)
  result4 <- sample(1:10, 3)
  expect_equal(result3, result4)
})

test_that("fect results are reproducible with same seed (sequential)", {
  skip_on_cran()
  
  data(fect)
  
  set.seed(12345)
  out1 <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
               force = "two-way", method = "fe", se = TRUE, nboots = 50,
               parallel = FALSE)
  
  set.seed(12345)
  out2 <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
               force = "two-way", method = "fe", se = TRUE, nboots = 50,
               parallel = FALSE)
  
  expect_equal(out1$est.att, out2$est.att)
  expect_equal(out1$att.avg, out2$att.avg)
})

test_that("fect results are reproducible with same seed (parallel)", {
  skip_on_cran()
  skip_if_not(parallel::detectCores() > 1, "Multiple cores not available")
  
  data(fect)
  
  set.seed(12345)
  out1 <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
               force = "two-way", method = "fe", se = TRUE, nboots = 50,
               parallel = TRUE, cores = 2)
  
  set.seed(12345)
  out2 <- fect(Y ~ D + X1 + X2, data = simdata, index = c("id","time"), 
               force = "two-way", method = "fe", se = TRUE, nboots = 50,
               parallel = TRUE, cores = 2)
  
  expect_equal(out1$est.att, out2$est.att)
  expect_equal(out1$att.avg, out2$att.avg)
})
