## Parallel Cross-Validation Implementation - Visual Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     PARALLEL CV IMPLEMENTATION                          │
└─────────────────────────────────────────────────────────────────────────┘

User Code:
  fect(Y ~ D + X, data, CV=TRUE, parallel=TRUE, cores=4)
         │
         ▼
  ┌─────────────────┐
  │   fect()        │  [R/default.R]
  │   parallel=TRUE │
  │   cores=4       │
  └────────┬────────┘
           │ passes parameters
           ▼
  ┌─────────────────┐
  │   fect_cv()     │  [R/cv.R]
  │   parallel=TRUE │
  │   cores=4       │
  └────────┬────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  Parallel Backend Setup                      │
  │  • detectCores() → 8 available              │
  │  • cores = min(8-2, 8) = 6                  │
  │  • parallel::makeCluster(6)                  │
  │  • doParallel::registerDoParallel()         │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  Cross-Validation Loop                       │
  │  • IFE: for (r in 0:5) { ... }              │
  │  • MC: for (lambda in λ_1:λ_10) { ... }     │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  K-Fold Loop (PARALLELIZED)                  │
  │                                              │
  │  foreach(ii = 1:k) %dopar% {                │
  │    ┌────────────┬────────────┬──────────┐   │
  │    │  Fold 1    │  Fold 2    │  Fold 5  │   │
  │    │  Worker 1  │  Worker 2  │  Worker 5│   │
  │    ├────────────┼────────────┼──────────┤   │
  │    │ Train on   │ Train on   │ Train on │   │
  │    │ 4 folds    │ 4 folds    │ 4 folds  │   │
  │    │ Test on 1  │ Test on 1  │ Test on 1│   │
  │    ├────────────┼────────────┼──────────┤   │
  │    │ Compute    │ Compute    │ Compute  │   │
  │    │ SSE, WSSE  │ SSE, WSSE  │ SSE, WSSE│   │
  │    │ GSSE, etc  │ GSSE, etc  │ GSSE, etc│   │
  │    └────────────┴────────────┴──────────┘   │
  │  }                                           │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  Result Aggregation (.combine function)      │
  │  • SSE = SSE₁ + SSE₂ + ... + SSE₅          │
  │  • WSSE = WSSE₁ + WSSE₂ + ... + WSSE₅      │
  │  • moment.list = c(m₁, m₂, ..., m₅)        │
  │  • MAD.list = c(mad₁, mad₂, ..., mad₅)     │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  Compute CV Metrics                          │
  │  • MSPE = SSE / n                           │
  │  • Select best r (IFE) or λ (MC)            │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  ┌──────────────────────────────────────────────┐
  │  Cleanup                                     │
  │  • parallel::stopCluster(para.clusters)     │
  └────────┬─────────────────────────────────────┘
           │
           ▼
  Return results to user


═══════════════════════════════════════════════════════════════════════

PERFORMANCE COMPARISON:

Sequential (parallel=FALSE):
  ┌─────┐
  │Fold1│─┐
  └─────┘ │
  ┌─────┐ │
  │Fold2│─┤
  └─────┘ │
  ┌─────┐ │
  │Fold3│─┤  Total Time: 100 seconds
  └─────┘ │
  ┌─────┐ │
  │Fold4│─┤
  └─────┘ │
  ┌─────┐ │
  │Fold5│─┘
  └─────┘

Parallel (parallel=TRUE, cores=5):
  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐
  │Fold1│ │Fold2│ │Fold3│ │Fold4│ │Fold5│
  └─────┘ └─────┘ └─────┘ └─────┘ └─────┘
  
  Total Time: ~25 seconds (4x speedup)

═══════════════════════════════════════════════════════════════════════

KEY IMPLEMENTATION DETAILS:

1. Parallelization Target:
   • k-fold inner loop (most time-consuming)
   • NOT the outer loop (r or lambda)
   • Good balance: efficiency vs complexity

2. Variable Export:
   • inter_fe_ub / inter_fe_mc (estimation functions)
   • Data: II, YY, Y0CV, X, WW, beta0CV
   • Parameters: force, tol, max.iteration, r, i
   • CV splits: rmCV, estCV
   • Weights: count.T.cv, T.on

3. Result Combination:
   • Sums: SSE, WSSE, GSSE, WGSSE, ll.length
   • Concatenations: moment.list, index.moment.list, MAD.list, W.estCV

4. Correctness:
   • Same algorithm as sequential
   • Same random seed → identical results
   • Proper aggregation logic

═══════════════════════════════════════════════════════════════════════

USAGE EXAMPLES:

# Basic: Just add parallel=TRUE
fect(Y ~ D + X1 + X2, data = simdata, index = c("id", "time"),
     method = "ife", CV = TRUE, r = c(0, 5), parallel = TRUE)

# Advanced: Specify cores
fect(Y ~ D + X1 + X2, data = simdata, index = c("id", "time"),
     method = "mc", CV = TRUE, nlambda = 10, 
     parallel = TRUE, cores = 8)

# Auto-detect cores (default behavior when parallel=TRUE)
fect(Y ~ D + X1 + X2, data = simdata, index = c("id", "time"),
     method = "ife", CV = TRUE, r = c(0, 5), parallel = TRUE)
     # Uses min(detectCores() - 2, 8) cores

═══════════════════════════════════════════════════════════════════════
```
