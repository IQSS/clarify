# `clarify` (development version)

* Simultaneous confidence bands and p-values are now available by setting `simultaneous = TRUE` in calls to `summary()` or `plot()`.

* Fixed a bug when using `sim_ame()` after `misim()` with `by` specified. Thanks to Arvind Ilamaran for pointing out this bug.

* Added references to the published article in *The R Journal*, including in the package citation (`citation("clarify")`).

* Fixed an issue where sampling from a t-distribution used a slightly incorrect covariance matrix, leading to intervals that could be a tiny bit too wide in small samples.

* Now using a new random number generator that is more tolerant of nearly singular covariance matrices. The removes a dependence on `mvnfast`. Results from prior versions of `clarify` will not be reproducible using this and future versions, even when using the same seed.

# `clarify` 0.2.2

* `sim_setx()`, `sim_ame()`, and `sim_adrf()` now accept other arguments passed through `...`, which are passed to `marginaleffects::get_predict()` to compute predictions.

* Documentation updates.

* Error and warning messages are wrapped more nicely.

# `clarify` 0.2.1

* In `sim_ame()` and `sim_adrf()`, unit-level weights are no longer used to compute means, consistent with advice in [Gabriel et al. (2023)](https://doi.org/10.1002/sim.9969). For those using these functions after matching or weighting for the ATT or ATC, this will not change results. For matching or weighting for the ATE, this improves robustness against misspecified weights.

* In `sim_ame()`, more than one variable can be supplied to `var` to generate average adjusted predictions or compute average marginal effects with other variables set to supplied values. The help page for `sim_ame()` has been retooled to reflect this.

* In `transform()`, values can now be indicated by positional shortcuts of the form `.b{#}`, e.g., `.b1 - .b2`, to facilitate specifying transformations of the desired quantities without using the names of the quantities, which can be frustrating to use.

* When `reference = TRUE` with `plot()`, a blue line at the median of the simulated estimates is also included on the plot; when this value does not align with the estimate, quantile confidence intervals may be invalid.

# `clarify` 0.2.0

* `sim_ame()` and `sim_adrf()` now have a `by` argument, which can be used to estimate quantities of interest within subsets of one or more variables.

* `sim_setx()` can now receive a data frame for its `x` and `x1` arguments.

* `sim_ame()` can accept new options for `contrast`: `"sr"` for the survival ratio and `"srr"` for the switch relative risk.

* Slight speed improvements in `sim_ame()` with continuous `var` and `sim_adrf()` with `contrast = "amef"`.

* Typo fixes in vignettes.

# `clarify` 0.1.3

* Documentation updates incorporating the work of Rainey (2023). `clarify` already implemented the recommendations in Rainey (2023) so no functionality has changed.

# `clarify` 0.1.2

* Added the argument `reference` to `plot.clarify_est()`, which adds a reference normal distribution to the density of the estimates.

* Fixed error in `sim()` documentation about how degrees of freedom are computed. Thanks to @wviechtb. (#8)

* Fixed a warning that can occur about recovering model data, from `insight`.

# `clarify` 0.1.1

* In `summary.clarify_est()`, `null` can now be supplied as a named vector to specify the quantities for which p-values should be computed.

* Fixes in anticipation of breaking changes from `marginaleffects` to ensure compatibility (including with older versions).

* Updates to the README and vignettes.

# `clarify` 0.1.0

* First release!
