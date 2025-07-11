# Version 5.1.20 [2025-07-03]
* Main project repo is now hosted on [Gitlab](https://gitlab.com/dashaub/forecastHybrid/). The [GitHub](https://github.com/ellisp/forecastHybrid) repo may not be the most up to date.
* Added second GPG signing key davidshaub@alumni.harvard.edu to package in addition to deprecated davidshaub@gmx.com key in `inst` directory.
* Add the `arfima` model as a possibility to the ensemble by using the model code `x`.
* Require `forecast` 8.13.
* Run `chkDots()` where possible but remove it from functions that will throw warnings when running tests (`tsCombine()`, `forecast.thetam()`, and `forecast.hybridModel()`).
* Add tests for `tsCombine()` and more tests for `tsSubsetWithIndices()`
* Documentation links not specify exact packages for function to avoid ambiguity.

# Version 5.0.19 [2020-08-27]
* Fix `residuals.hybridModel()` and added tests.
* Linting of package with the "lintr" package
* `Makefile` targets for building and package development.

# Version 5.0.18 [2020-04-01]
* Add the `rolling` argument to `hybridModel()` that can be used when `weights = "cv.errors"` to control the `rolling` argument in `cvts()`.
* Add `comb` as an argument to `thiefModel()`.
* Update `accuracy()` formals to support changes in the "forecast" package version 8.12.
* Disable longer-running tests on CRAN.
* Faster examples and vignette

# Version 4.2.17 [2019-02-11]
* Experimental ensembles with the `thief()` function can now be created with the new `thiefModel()` function. The API is similar to that of `hybridModel()`.
* Import "thief" package.

# Version 4.1.16 [2018-12-19]
* Use forking instead of a socket cluster for parallel execution on UNIX-like systems for `cvts()`. This results in significantly faster execution and less memory usage, particularly when the `FUN` and `FCFUN` functions are very quick (e.g. `snaive()`, `rwf()`, `stlm()`), the time series is short, few cores are used, or few CV folds run.
* Faster `cvts()` examples.
* More tests for `cvts()`.
* Fix spelling errors.

# Version 4.0.15 [2018-12-17]
* The `xreg` argument passed in should now be a matrix instead of a dataframe for consistency with "forecast" v8.5.
* Fix messy function call in #27. This results in `hybridModel` objects that use far less memory and that print more cleanly to the console. For example, previously `hm <- hybridModel(wineind); format(object.size(hm), units = "auto")` produced a 5.8 Mb object but now it is only 314.8 Kb.
* Adds "purrr" to imports.

# Version 3.0.14 [2018-07-22]
* Parallel support added to `hybridModel()`. This can be controlled by setting `parallel = TRUE` and setting `num.cores`. By default this is not enabled since the performance improvement typically only occurs when fitting `auto.arima` and `tbats` models on long series with large frequency (e.g. `taylor`).
* Added `z.args` for the `snaive()` model.
* The `tbats()` and `snaive()` models now respect and use `lambda` when passed in `t.args` and `z.args`.
* Refactored code to remove code duplication and cleaned up the hybridModel file by moving code into helper and generic files.
* Weights and cross validation for the `snaive` model are now handled correctly.
* Project is now hosted on [GitLab](https://gitlab.com/dashaub/forecastHybrid) as well as [GitHub](https://github.com/ellisp/forecastHybrid). Pull requests can be submitted to either platform, and branches should sync to Gitlab automatically every hour.
* Commits and tags are now signed with RSA key 09183768E3BC25497F9060C701B81BAF17A89621. The public key is located in the package archive root directory in `inst/davidshaub@gmx.com.key` and hosted on both GitHub and GitLab in `pkg/inst/davidshaub@gmx.com.key`.

# Version 2.2.12 [2018-05-04]
* Added `PI.combination` argument to `forecast.hybridModel()`. The default behavior is to follow the existing methodology of using the most extreme prediction intervals from the component models. When `"mean"` is passed instead, a simple (unweighted) average of the component prediction intervals is used instead.
* The theta model included in an ensemble can now handle seasonality with frequency >= 24.
* The ets model can now be included for hourly data.
* The "reshape2" package is no longer imported.

# Version 2.1.11 [2018-03-27]
* Added `snaive()` model to the ensemble. It is disabled by default, but can be added with "z".

# Version 2.0.10 [2018-01-03]
* API change in `cvts()` for the `FCFUN` argument: custom forecasting functions should now return a S3 "forecast" object with the point forecast in `$mean`, and the `ts` properties should be properly set.
* `cvts()` now defaults to 2 cores
* Moved usage examples for `cvts()` to the vignette.
* Add "GMDH" to suggested packages.
* Fixed a bug in `cvts()` introduced in version 1.0.8 when a custom `FUN` or `FCFUN` is used that requires packages other than "forecast" or "forecastHybrid".
* The `thetam()` function now checks for an input time series with less length than the seasonality. Similarly, `hybridModel()` detects this behavior. Thanks to Nicholas Fong for the bugfix.
* Updated `cvts()` usage example in documentation for "GMDH".
* Refactored many unit tests and the vignette for quicker examples.

# Version 1.1.9 [2017-08-23]
* Fixed a bug in `forecast.hybridModel()` when for models where `xreg` was not supplied to all of arima/nnetar models.
* Fixes in unit tests and better documentation of unit tests.
* `ts` objects created with the "timekt" package can now be used in `hybridModel()`.
* The `doParallel` and `forecast` packages are now imported instead of loading their entire namespaces.

# Version 1.0.8 [2017-07-10]
* `cvts()` now supports parallel fitting through the `num.cores` argument.
Note that if the model that you are fitting also utilizes parallelization,
the number of cores used by each model multiplied by `num.cores` passed to
`cvts()` should not exceed the number of cores on your machine.
* The package versioning now follows [semantic versioning](https://semver.org/) more closely; however, the convention used will be `MAJOR.MINOR.RELEASE_NUM`.
* Instead of loading the entire `ggplot2` namespace, only specific functions are now imported.

# Version 0.4.1 [2017-06-18]
* The "forecast" package v8.1 now declares the S3 method `accuracy()`, so this is imported and no longer declared in "forecastHybrid".

# Version 0.4.0 [2017-03-31]
* Import the "zoo" package 
* Fixed a bug in `cvts()` when using `rolling = TRUE` whereby the incorrect number of periods were calulated. Thanks to Ganesh Krishnan for the bugfix.
  * The `cvts()` function now allows additional arguments to be passed with `...`. Thanks to Ganesh Krishnan.
* Additional `...` arguments can be passed to the individual component models in `forecast.hybridModel()`.
* Documentation fixes and improvements, particularly for the `cvts()` function.
* Unit tests were optimized for speed, and the package tests in half the previous time.
* The behavior of the `forecast()` function from the "forecast" package when multiple or single prediction intervals are passed has changed. The prediction inervals are now consistently returned as matrices. This change fixes a bug in `forecast.hybridModel()` when multiple prediction intervals are used.
* Fixed a bug with `forecast.hybridModel()` for `ets`, `nnetar`, and `stlm` component models when the `level` argument was set to a single value instead of a vector of values.
* Fixed warning message for superfluous lists passed to base models in `hybridModel()`

# Version 0.3.0 [2016-12-18]
* Prediction intervals are now created for `nnetar` objects in the ensemble. This should address one aspect of incorrect prediction intervals (e.g. issue #37).
* theta models can be added (by including "`f`" in the `models =` argument for `hybridModel()`) and are indeed part of the default - so by default, hybridModel() will now fit six models
* `accuracy.cvts()` is now exported
* `plot.hybridModel()` now supports `ggplot2` graphics when the argument `ggplot = TRUE` is passed.
* Time series must be at least four observations long
* Fixed an error where e.args was passed to tbats instead of t.args

# Version 0.2.0 [2016-09-23]
* Add time series cross validation with `cvts()`
* Add support for `weights = "cv.errors"` in `hybridModel()`
* Fix model weights when `weights = "insample.errors"` and one or more component models perfectly fit the time series
* Fixed erroneous warning message when `xreg` is included in `n.args` but a `nnetar` model is not included in the model list
* Clean up titles in `plot.hybridModel()`
* Enable passing `...` arguments to `plot()` from `plot.hybridModel()`
* Round weights in `print.hybridModel()` to three digits for cleaner display
* Add `verbose` argument and enable by default in `hybridModel()` to display fitting/cross validation progress

# Version 0.1.7 [2016-06-04]
* Build vignette with `knitr rmarkdown` engine
* Build vignette with travis

# Version 0.1.6 [2016-05-31]
* Fix broken S3 generic `accuracy()` and `hybridModel.accuracy()`
* Add vignette
* Add NEWS
* Remove "fpp" from dependencies
* Fix warning for unimplemented parameter `weights = "cv.errors"`
* Fix error with `nnetar` and `stlm` models when `2 * frequency(y) >= length(y)`
* Documentation improvements MORE TODO
* Migrate unit tests away from deprecated `not()` function from "testthat" package
* Add additional unit tests for bugfixes (accuracy fix, nnetar/stlm `2 * frequency(y) >= length(y)`, `weights = "cv.errors"`)

# Version 0.1.5 [2016-04-16]
* First CRAN release
