test_that("bootstrap is reproducible with parallel processing", {
  skip_on_cran()
  skip_if_not_installed("doParallel")
  skip_if_not_installed("doRNG")
  
  # Setup parallel backend
  cores <- 2
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit({
    parallel::stopCluster(cl)
    doParallel::registerDoSEQ()
  })
  
  # Create simple test data
  set.seed(123)
  N <- 30
  TT <- 10
  
  # Create panel data
  data <- data.frame(
    id = rep(1:N, each = TT),
    time = rep(1:TT, N),
    y = rnorm(N * TT, mean = 5, sd = 2),
    x = rnorm(N * TT),
    treat = 0
  )
  
  # Add treatment effect for half the units after time 5
  treated_units <- 1:(N/2)
  data$treat[data$id %in% treated_units & data$time > 5] <- 1
  data$y[data$id %in% treated_units & data$time > 5] <- 
    data$y[data$id %in% treated_units & data$time > 5] + 2
  
  # Run fect twice with same seed using L'Ecuyer-CMRG
  set.seed(1234, kind = "L'Ecuyer-CMRG")
  result1 <- fect(
    y ~ x, 
    data = data, 
    index = c("id", "time"),
    method = "mc", 
    se = TRUE, 
    nboots = 50,  # Small number for fast testing
    parallel = TRUE, 
    cores = cores
  )
  
  set.seed(1234, kind = "L'Ecuyer-CMRG")
  result2 <- fect(
    y ~ x, 
    data = data, 
    index = c("id", "time"),
    method = "mc", 
    se = TRUE, 
    nboots = 50,
    parallel = TRUE, 
    cores = cores
  )
  
  # Check that results are identical
  expect_identical(
    result1$est.avg[1, "S.E."], 
    result2$est.avg[1, "S.E."],
    info = "Bootstrap standard errors should be identical with same seed"
  )
  
  expect_identical(
    result1$est.avg[1, "CI.lower"], 
    result2$est.avg[1, "CI.lower"],
    info = "Confidence intervals should be identical with same seed"
  )
  
  expect_identical(
    result1$est.avg[1, "CI.upper"], 
    result2$est.avg[1, "CI.upper"],
    info = "Confidence intervals should be identical with same seed"
  )
})

test_that("bootstrap is reproducible with IFE method", {
  skip_on_cran()
  skip_if_not_installed("doParallel")
  skip_if_not_installed("doRNG")
  
  # Setup parallel backend
  cores <- 2
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit({
    parallel::stopCluster(cl)
    doParallel::registerDoSEQ()
  })
  
  # Create simple test data
  set.seed(456)
  N <- 30
  TT <- 10
  
  # Create panel data
  data <- data.frame(
    id = rep(1:N, each = TT),
    time = rep(1:TT, N),
    y = rnorm(N * TT, mean = 5, sd = 2),
    treat = 0
  )
  
  # Add treatment effect
  treated_units <- 1:(N/2)
  data$treat[data$id %in% treated_units & data$time > 5] <- 1
  data$y[data$id %in% treated_units & data$time > 5] <- 
    data$y[data$id %in% treated_units & data$time > 5] + 2
  
  # Run fect twice with same seed
  set.seed(5678, kind = "L'Ecuyer-CMRG")
  result1 <- fect(
    y ~ 1, 
    data = data, 
    index = c("id", "time"),
    method = "ife", 
    r = 0,
    se = TRUE, 
    nboots = 50,
    parallel = TRUE, 
    cores = cores
  )
  
  set.seed(5678, kind = "L'Ecuyer-CMRG")
  result2 <- fect(
    y ~ 1, 
    data = data, 
    index = c("id", "time"),
    method = "ife", 
    r = 0,
    se = TRUE, 
    nboots = 50,
    parallel = TRUE, 
    cores = cores
  )
  
  # Check that results are identical
  expect_identical(
    result1$est.avg[1, "S.E."], 
    result2$est.avg[1, "S.E."],
    info = "IFE bootstrap standard errors should be identical with same seed"
  )
})

test_that("results differ with different seeds", {
  skip_on_cran()
  skip_if_not_installed("doParallel")
  skip_if_not_installed("doRNG")
  
  # Setup parallel backend
  cores <- 2
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  on.exit({
    parallel::stopCluster(cl)
    doParallel::registerDoSEQ()
  })
  
  # Create simple test data
  set.seed(789)
  N <- 30
  TT <- 10
  
  data <- data.frame(
    id = rep(1:N, each = TT),
    time = rep(1:TT, N),
    y = rnorm(N * TT, mean = 5, sd = 2),
    treat = 0
  )
  
  treated_units <- 1:(N/2)
  data$treat[data$id %in% treated_units & data$time > 5] <- 1
  data$y[data$id %in% treated_units & data$time > 5] <- 
    data$y[data$id %in% treated_units & data$time > 5] + 2
  
  # Run with different seeds
  set.seed(1111, kind = "L'Ecuyer-CMRG")
  result1 <- fect(
    y ~ 1, 
    data = data, 
    index = c("id", "time"),
    method = "mc", 
    se = TRUE, 
    nboots = 50,
    parallel = TRUE, 
    cores = cores
  )
  
  set.seed(9999, kind = "L'Ecuyer-CMRG")
  result2 <- fect(
    y ~ 1, 
    data = data, 
    index = c("id", "time"),
    method = "mc", 
    se = TRUE, 
    nboots = 50,
    parallel = TRUE, 
    cores = cores
  )
  
  # Results should differ with different seeds
  expect_false(
    isTRUE(all.equal(result1$est.avg[1, "S.E."], result2$est.avg[1, "S.E."])),
    info = "Bootstrap standard errors should differ with different seeds"
  )
})
