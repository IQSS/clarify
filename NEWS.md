# `clarify` 0.2.0

* `sim_ame()` and `sim_adrf()` now have a `by` argument, which can be used to estimate quantities of interest within subsets of one or more variables.

* `sim_setx()` can now receive a data frame for its `x` and `x1` arguments.

* `sim_ame()` can accept new options for `contrast`: `"sr"` for the survival ratio and `"srr"` for the switch relative risk.

* Slight speed improvements in `sim_ame()` with continuous `var` and `sim_adrf()` with `contrast = `"amef"`.

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
