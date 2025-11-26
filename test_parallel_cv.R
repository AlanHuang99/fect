#!/usr/bin/env Rscript

## Test Script for Parallel Cross-Validation
## This script tests:
## 1. Whether parallel computing is actually being used
## 2. Whether there are significant speed gains
## 3. Whether results are identical between parallel and sequential

library(fect)
library(parallel)

cat("\n========================================\n")
cat("Testing Parallel Cross-Validation\n")
cat("========================================\n\n")

# Load test data
data("simdata")

# Display system information
cat("System Information:\n")
cat("  Available cores:", detectCores(), "\n")
cat("  Will use 4 cores for testing\n\n")

## ========================================
## Test 1: IFE Method - Correctness Test
## ========================================
cat("TEST 1: IFE Method - Correctness Test\n")
cat("--------------------------------------\n")
cat("Testing that parallel and sequential produce identical results...\n\n")

set.seed(12345)
cat("Running IFE with parallel=FALSE (sequential)...\n")
time_seq_ife <- system.time({
  out_seq_ife <- fect(
    Y ~ D + X1 + X2,
    data = simdata,
    index = c("id", "time"),
    method = "ife",
    CV = TRUE,
    r = c(0, 3),  # Test range of factors
    parallel = FALSE,
    se = FALSE
  )
})

cat("Sequential IFE completed in", time_seq_ife["elapsed"], "seconds\n\n")

set.seed(12345)
cat("Running IFE with parallel=TRUE (4 cores)...\n")
time_par_ife <- system.time({
  out_par_ife <- fect(
    Y ~ D + X1 + X2,
    data = simdata,
    index = c("id", "time"),
    method = "ife",
    CV = TRUE,
    r = c(0, 3),
    parallel = TRUE,
    cores = 4,
    se = FALSE
  )
})

cat("Parallel IFE completed in", time_par_ife["elapsed"], "seconds\n\n")

# Check if results are identical
cat("Checking result consistency:\n")
ife_att_match <- all.equal(out_seq_ife$att.avg, out_par_ife$att.avg)
ife_r_match <- all.equal(out_seq_ife$r.cv, out_par_ife$r.cv)
ife_time_match <- all.equal(out_seq_ife$time, out_par_ife$time)

cat("  att.avg identical:", isTRUE(ife_att_match), "\n")
cat("  r.cv identical:", isTRUE(ife_r_match), "\n")
cat("  time identical:", isTRUE(ife_time_match), "\n\n")

# Calculate speedup
ife_speedup <- time_seq_ife["elapsed"] / time_par_ife["elapsed"]
cat("IFE Speedup:", round(ife_speedup, 2), "x\n")

if (ife_speedup > 1.2) {
  cat("✓ PASS: Significant speedup observed (>1.2x)\n")
} else {
  cat("⚠ WARNING: Limited speedup observed (<1.2x)\n")
  cat("  Note: Speedup depends on dataset size and number of folds\n")
}

if (isTRUE(ife_att_match) && isTRUE(ife_r_match)) {
  cat("✓ PASS: Results are identical\n\n")
} else {
  cat("✗ FAIL: Results differ between parallel and sequential!\n\n")
}

## ========================================
## Test 2: MC Method - Correctness Test
## ========================================
cat("\n========================================\n")
cat("TEST 2: MC Method - Correctness Test\n")
cat("--------------------------------------\n")
cat("Testing that parallel and sequential produce identical results...\n\n")

set.seed(12345)
cat("Running MC with parallel=FALSE (sequential)...\n")
time_seq_mc <- system.time({
  out_seq_mc <- fect(
    Y ~ D + X1 + X2,
    data = simdata,
    index = c("id", "time"),
    method = "mc",
    CV = TRUE,
    nlambda = 8,  # Test multiple lambda values
    parallel = FALSE,
    se = FALSE
  )
})

cat("Sequential MC completed in", time_seq_mc["elapsed"], "seconds\n\n")

set.seed(12345)
cat("Running MC with parallel=TRUE (4 cores)...\n")
time_par_mc <- system.time({
  out_par_mc <- fect(
    Y ~ D + X1 + X2,
    data = simdata,
    index = c("id", "time"),
    method = "mc",
    CV = TRUE,
    nlambda = 8,
    parallel = TRUE,
    cores = 4,
    se = FALSE
  )
})

cat("Parallel MC completed in", time_par_mc["elapsed"], "seconds\n\n")

# Check if results are identical
cat("Checking result consistency:\n")
mc_att_match <- all.equal(out_seq_mc$att.avg, out_par_mc$att.avg)
mc_lambda_match <- all.equal(out_seq_mc$lambda.cv, out_par_mc$lambda.cv)
mc_time_match <- all.equal(out_seq_mc$time, out_par_mc$time)

cat("  att.avg identical:", isTRUE(mc_att_match), "\n")
cat("  lambda.cv identical:", isTRUE(mc_lambda_match), "\n")
cat("  time identical:", isTRUE(mc_time_match), "\n\n")

# Calculate speedup
mc_speedup <- time_seq_mc["elapsed"] / time_par_mc["elapsed"]
cat("MC Speedup:", round(mc_speedup, 2), "x\n")

if (mc_speedup > 1.2) {
  cat("✓ PASS: Significant speedup observed (>1.2x)\n")
} else {
  cat("⚠ WARNING: Limited speedup observed (<1.2x)\n")
  cat("  Note: Speedup depends on dataset size and number of folds\n")
}

if (isTRUE(mc_att_match) && isTRUE(mc_lambda_match)) {
  cat("✓ PASS: Results are identical\n\n")
} else {
  cat("✗ FAIL: Results differ between parallel and sequential!\n\n")
}

## ========================================
## Test 3: Performance Summary
## ========================================
cat("\n========================================\n")
cat("PERFORMANCE SUMMARY\n")
cat("========================================\n\n")

cat("IFE Method:\n")
cat("  Sequential time:", round(time_seq_ife["elapsed"], 2), "seconds\n")
cat("  Parallel time:  ", round(time_par_ife["elapsed"], 2), "seconds\n")
cat("  Speedup:        ", round(ife_speedup, 2), "x\n")
cat("  Time saved:     ", round(time_seq_ife["elapsed"] - time_par_ife["elapsed"], 2), "seconds\n\n")

cat("MC Method:\n")
cat("  Sequential time:", round(time_seq_mc["elapsed"], 2), "seconds\n")
cat("  Parallel time:  ", round(time_par_mc["elapsed"], 2), "seconds\n")
cat("  Speedup:        ", round(mc_speedup, 2), "x\n")
cat("  Time saved:     ", round(time_seq_mc["elapsed"] - time_par_mc["elapsed"], 2), "seconds\n\n")

avg_speedup <- mean(c(ife_speedup, mc_speedup))
cat("Average Speedup: ", round(avg_speedup, 2), "x\n\n")

## ========================================
## Test 4: Verify Parallel Backend Usage
## ========================================
cat("\n========================================\n")
cat("TEST 4: Verify Parallel Backend Usage\n")
cat("========================================\n\n")

cat("Testing with different core counts to verify parallel usage...\n\n")

# Test with 2 cores
set.seed(12345)
cat("Testing with 2 cores...\n")
time_2cores <- system.time({
  fect(Y ~ D + X1 + X2, data = simdata, index = c("id", "time"),
       method = "ife", CV = TRUE, r = c(0, 2), 
       parallel = TRUE, cores = 2, se = FALSE)
})

# Test with 4 cores
set.seed(12345)
cat("Testing with 4 cores...\n")
time_4cores <- system.time({
  fect(Y ~ D + X1 + X2, data = simdata, index = c("id", "time"),
       method = "ife", CV = TRUE, r = c(0, 2),
       parallel = TRUE, cores = 4, se = FALSE)
})

cat("\n")
cat("Time with 2 cores:", round(time_2cores["elapsed"], 2), "seconds\n")
cat("Time with 4 cores:", round(time_4cores["elapsed"], 2), "seconds\n")

if (time_4cores["elapsed"] < time_2cores["elapsed"]) {
  cat("✓ PASS: More cores = faster execution (parallel backend is working)\n\n")
} else {
  cat("⚠ NOTE: Time with 4 cores not faster than 2 cores\n")
  cat("  This may be normal for small datasets or limited parallelizable work\n\n")
}

## ========================================
## Final Results
## ========================================
cat("\n========================================\n")
cat("FINAL RESULTS\n")
cat("========================================\n\n")

all_correct <- isTRUE(ife_att_match) && isTRUE(ife_r_match) && 
               isTRUE(mc_att_match) && isTRUE(mc_lambda_match)

if (all_correct && avg_speedup > 1.2) {
  cat("✓✓✓ ALL TESTS PASSED ✓✓✓\n\n")
  cat("Summary:\n")
  cat("  ✓ Parallel and sequential produce identical results\n")
  cat("  ✓ Parallel computing provides significant speedup (", round(avg_speedup, 2), "x)\n")
  cat("  ✓ Implementation is working correctly\n\n")
} else if (all_correct) {
  cat("✓ CORRECTNESS PASSED, but limited speedup\n\n")
  cat("Summary:\n")
  cat("  ✓ Parallel and sequential produce identical results\n")
  cat("  ⚠ Speedup is limited (", round(avg_speedup, 2), "x)\n")
  cat("  Note: This may be normal for small datasets like simdata\n")
  cat("        Larger datasets should show more significant speedup\n\n")
} else {
  cat("✗✗✗ TESTS FAILED ✗✗✗\n\n")
  cat("Issues detected:\n")
  if (!isTRUE(ife_att_match) || !isTRUE(ife_r_match)) {
    cat("  ✗ IFE results differ between parallel and sequential\n")
  }
  if (!isTRUE(mc_att_match) || !isTRUE(mc_lambda_match)) {
    cat("  ✗ MC results differ between parallel and sequential\n")
  }
  cat("\n")
}

cat("========================================\n\n")
