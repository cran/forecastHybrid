#' Hybrid forecast
#'
#' Forecast method for hybrid models.
#'
#' @export
#' @import forecast
#' @param object a hybrid time series model fit with \link{hybridModel}.
#' @param h number of periods for forecasting. If \code{xreg} is used, \code{h} is ignored
#' and the number of forecast periods is set to the number of rows of \code{xreg}.
#' @param xreg future values of regression variables (for use if one of the ensemble methods used
#' in creating the hybrid forecast was \code{auto.arima}, \code{nnetar}, or \code{stlm}
#' and the model(s) used \code{xreg} in the fit).
#' It should be supplied as a matrix.
#' @param level confidence level for prediction intervals. This can be expressed as a decimal
#' between 0.0 and 1.0 or numeric between 0 and 100.
#' @param fan if \code{TRUE}, level is set to \code{seq(51, 99, by = 3)}. This is suitable
#' for fan plots.
#' @param PI should prediction intervals be produced? If a \code{nnetar} model is in the ensemble,
#' this can be quite slow, so disabling prediction intervals will speed up the forecast generation.
#' If \code{FALSE}, the arguments \code{level} and \code{fan} are ignored.
#' @param PI.combination Method for combining the prediction intervals from each of the
#' forecasts. Supplying \code{"mean"}
#' will simply average each of the lower/upper intervals from each model without using the model
#' weights used for the point forecasts. The default value \code{"extreme"} will take
#' the most pessimistic intervals (i.e. the highest upper interval from all the component models
#' and the lowest prediction interval from all of the component models').
#' @param ... other arguments passed to the individual \code{forecast} generic methods.
#' @seealso \code{\link{hybridModel}}
#' @details if \code{xreg} was used in constructing the \code{hybridModel},
#' it must also be passed into \code{forecast.hybridModel}.
#' \cr
#' \cr
#' While prediction intervals are produced for the
#' final ensemble forecast model, these should be viewed conservatively as insights
#' to the forecast's uncertainty. Currently these are constructed using the most extreme interval
#' from each component model for each horizon, so the composite prediction intervals do not
#' have statistical guarantees of asymptotic efficiency. More sophisticated
#' and rigorous techniques are planned, however, particularly when cross validation
#' approaches are used.
#' @return An object of class \link[forecast]{forecast}.
#' @examples
#' \dontrun{
#' mod <- hybridModel(AirPassengers)
#' fc <- forecast(mod)
#'
#' # View the point forecasts
#' fc$mean
#'
#' # View the upper prediction interval
#' fc$upper
#'
#' # View the lower prediction interval
#' fc$lower
#'
#' # Plot the forecast
#' plot(fc)
#' }
#'@author David Shaub
#'
forecast.hybridModel <- function(object, # nolint
                                 h = ifelse(object$frequency > 1, 2 * object$frequency, 10),
                                 xreg = NULL,
                                 level = c(80, 95),
                                 PI = TRUE, # nolint
                                 fan = FALSE,
                                 PI.combination = c("extreme", "mean"), # nolint
                                 ...) {
  # Check inputs
  # Although chkDots() has an `allowed` argument, in R 4.3.3 it is still not implemented.
  # If it is implemented in a later version, we can probably enable
  # the check. chkDots(..., allowed = "npaths")
  if (!is.hybridModel(object)) {
    stop("The object must be constructed from hybridModel().", call. = FALSE)
  }

  # apply nrow(xreg) to h if h isn't provided and xreg is
  if (missing(h) && !missing(xreg)) {
    h <- nrow(xreg)
  }

  # xreg should be a matrix and have same number of observations as the horizon
  if (!is.null(xreg)) {
    if (!is.matrix(xreg) && !is.numeric(xreg) && !is.data.frame(xreg)) {
      stop("The supplied xreg must be numeric, matrix, or data.frame.", call. = FALSE)
    }
    xreg <- as.matrix(xreg)
    if (!is.numeric(xreg)) {
      stop("The supplied xreg must be numeric.", call. = FALSE)
    }
    if (is.null(h) || nrow(xreg) != h) {
      warning("The number of rows in xreg should match h. Setting h to nrow(xreg).", call. = FALSE)
      h <- nrow(xreg)
    }
  }

  # Check the forecast horizon
  if (!is.numeric(h)) {
    stop("The forecast horizon h must be a positive integer.", call. = FALSE)
  }
  if (!as.logical((h %% 1L == 0L)) || h <= 0L) {
    stop("The forecast horizon h must be a positive integer.", call. = FALSE)
  }

  # Allow for fan prediction intervals
  if (fan) {
    level <- seq(51, 99, by = 3)
  } else {
    if (min(level) > 0 && max(level) < 1) {
      level <- 100 * level
    } else if (min(level) < 0 || max(level) > 99.99) {
      stop("Prediction interval out of range", call. = FALSE)
    }
  }


  # This code is pretty ugly, There is probably a better way of doing this.
  forecastWeights <- object$weights
  weightsMatrix <- matrix(rep(forecastWeights, times = h), nrow = h, byrow = TRUE)
  includedModels <- object$models
  forecasts <- list()
  forecasts$pointForecasts <- matrix(numeric(), nrow = h, ncol = length(includedModels))
  colnames(forecasts$pointForecasts) <- includedModels
  if ("auto.arima" %in% includedModels) {
    # Only apply the xreg if it was used in the original model
    xregA <- xreg
    if (!object$xreg$auto.arima) {
      xregA <- NULL
    }
    forecasts$auto.arima <- forecast(object$auto.arima, h = h, xreg = xregA, level = level, ...) # nolint
    forecasts$pointForecasts[, "auto.arima"] <- forecasts$auto.arima$mean
  }
  if ("ets" %in% includedModels) {
    forecasts$ets <- forecast(object$ets, h = h, level = level, ...)
    forecasts$pointForecasts[, "ets"] <- forecasts$ets$mean
  }
  if ("thetam" %in% includedModels) {
    forecasts$thetam <- forecast(object$thetam, h = h, level = level, ...)
    forecasts$pointForecasts[, "thetam"] <- forecasts$thetam$mean
  }
  if ("nnetar" %in% includedModels) {
    # Only apply the xreg if it was used in the original model
    xregN <- xreg
    if (!object$xreg$nnetar) {
      xregN <- NULL
    }
    forecasts$nnetar <- forecast(object$nnetar, h = h, xreg = xregN, PI = PI, level = level, ...)
    forecasts$pointForecasts[, "nnetar"] <- forecasts$nnetar$mean
  }
  if ("stlm" %in% includedModels) {
    # Only apply the xreg if it was used in the original model
    xregS <- xreg
    # xreg is only used in stlm if method = "arima"
    if (!object$xreg$stlm) {
      xregS <- NULL
    }
    forecasts$stlm <- forecast(object$stlm, h = h, xreg = xregS, level = level, ...)
    forecasts$pointForecasts[, "stlm"] <- forecasts$stlm$mean
  }
  if ("tbats" %in% includedModels) {
    forecasts$tbats <- forecast(object$tbats, h = h, level = level, ...)
    forecasts$pointForecasts[, "tbats"] <- forecasts$tbats$mean
  }
  if ("snaive" %in% includedModels) {
    forecasts$snaive <- snaive(object$x, h = h, level = level, ...)
    forecasts$pointForecasts[, "snaive"] <- forecasts$snaive$mean
  }
  if ("arfima" %in% includedModels) {
    forecasts$arfima <- forecast(object$arfima, h = h, level = level, ...)
    forecasts$pointForecasts[, "arfima"] <- forecasts$arfima$mean
  }

  # Apply the weights to the individual forecasts and create the final point forecast
  finalForecast <- rowSums(forecasts$pointForecast * weightsMatrix)
  # Conver the final forecast into a ts object
  finalForecast <- ts(finalForecast,
                      start = start(forecasts[[object$models[1]]]$mean),
                      end = end(forecasts[[object$models[1]]]$mean),
                      frequency = object$frequency)

  # Apply the weights to construct the fitted values
  fits <- sapply(includedModels, FUN = function(x) fitted(object[[x]]))
  fitsWeightsMatrix <- matrix(rep(forecastWeights, times = nrow(fits)),
                              nrow = nrow(fits), byrow = TRUE)
  fits <- rowSums(fits * fitsWeightsMatrix)
  resids <- object$x - fits

  # Construct the prediction intervals
  if (PI) {
    # Set the functions for the uppper/lower prediction intervals
    piCombination <- match.arg(PI.combination)
    if (piCombination == "mean") {
      upperFunction <- lowerFunction <- mean
    } else if (piCombination == "extreme") {
      upperFunction <- max
      lowerFunction <- min
    }
    nint <- length(level)
    upper <- lower <- matrix(NA, ncol = nint, nrow = length(finalForecast))

    piModels <- object$models
    # Produce each upper/lower limit
    for (i in 1:nint) {
      # Produce the upper/lower limit for each model for a given level
      tmpUpper <- tmpLower <- matrix(NA, nrow = h, ncol = length(piModels))
      j2 <- 1
      for (mod in piModels) {
        tmpUpper[, j2] <- as.numeric(matrix(forecasts[[mod]]$upper, nrow = h)[, i])
        tmpLower[, j2] <- as.numeric(matrix(forecasts[[mod]]$lower, nrow = h)[, i])
        j2 <- j2 + 1
      }
      # Apply the function for reconciling the prediction intervals
      upper[, i] <- apply(tmpUpper, 1, FUN = upperFunction)
      lower[, i] <- apply(tmpLower, 1, FUN = lowerFunction)
    }
    if (!is.finite(max(upper)) || !is.finite(min(lower))) {
      warning("Prediction intervals are not finite.", call. = FALSE)
    }
    colnames(lower) <- colnames(upper) <- paste0(level, "%")
    forecasts$lower <- lower
    forecasts$upper <- upper
  }
  forecasts$mean <- finalForecast

  # Add the fitted and residuals values
  if (is.ts(object$x)) {
    fits <- ts(fits)
    resids <- ts(resids)
    tsp(fits) <- tsp(resids) <- tsp(object$x)
  }
  forecasts$fitted <- fits
  forecasts$residuals <- resids

  # Build a forecast object
  forecasts$x <- forecasts[[object$models[1]]]$x
  forecasts$method <- paste0(object$models, " with weight ", round(object$weights, 3))
  forecasts$level <- level
  class(forecasts) <- "forecast"
  forecasts
}
