# `clarify` 0.1.3

* Documentation updates incorporating the work of Rainey (2023). `clarify` already implemented the recommendations in `Rainey (2023) so no functionality has changed.

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
