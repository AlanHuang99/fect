# fect (Development version)

## Bug Fixes

* Fixed seed reproducibility issue caused by R's `sample()` function behavior with length-1 vectors. Results are now fully reproducible when a seed is set, regardless of the number of units in treatment/control/reversal groups. This affects bootstrap standard errors, permutation tests, and cross-validation.

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
