# fect 2.0.5 (Development)

* **Performance improvement**: Parallelized cross-validation for IFE and MC methods
  - Cross-validation now supports parallel computing via the `parallel` parameter
  - The k-fold cross-validation loops are now executed in parallel when `parallel=TRUE`
  - Significant speedup for cross-validation operations, especially with multiple folds
  - Uses existing `doParallel` infrastructure, defaults to `min(detectCores() - 2, 8)` cores
  - Maintains backward compatibility - parallel is FALSE by default

# fect 2.0.4

* Add new plot `type = "hte"`

# fect 2.0.0

* New syntax
* Merged in **gsynth**

# fect 1.0.0

* First CRAN version
* Fixed bugs

# fect 0.6.5

* Replace fastplm with fixest for fixed effects estimation
* Added plots for heterogeneous treatment effects
* Fixed bugs

# fect 0.4.1

* Added a `NEWS.md` file to track changes to the package.
