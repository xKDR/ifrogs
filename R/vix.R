library(boot)
library(fOptions)

###########################################################################
#                           Implied Volatility                            #
###########################################################################

try_iv <- function (type, value, underlying, strike, maturity, riskfree, 
                    carry) {

  sapply(X=1:length(type),
         FUN=function (i) {
           retval <- try(GBSVolatility(price=value[i],
                                       TypeFlag=type[i],
                                       S=underlying[i],
                                       X=strike[i],
                                       Time=maturity[i],
                                       r=riskfree[i],
                                       b=carry[i]),
                         silent=FALSE)
           if (inherits(retval, "try-error")) retval <- NA
           retval
         })

}

###########################################################################
#                             Vega Calculation                            #
###########################################################################

compute_vega <- function (type, underlying, strike, maturity, riskfree, carry, 
                          iv) {

  vega <- sapply(X=1:length(type),
                 FUN=function (i) {
                   GBSGreeks(Selection="Vega",
                             TypeFlag=type[i],
                             S=underlying[i],
                             X=strike[i],
                             Time=maturity[i],
                             r=riskfree[i],
                             b=carry[i],
                             sigma=iv[i])
                 })

  ## Vega is a weight. Wherever we do not have a sensible value, we weight that
  ## observation by zero.
  vega[!is.finite(as.numeric(vega))] <- 0

  return(vega)

}

###########################################################################
#                           Weighting Schemes                             #
###########################################################################

get_weights <- function (scheme, bid, ask, traded_vol, vega, iv, value) {

  weight <- function (x) x / sum(x)

  switch(scheme,
         "spread"={
           spread <- 100 * (ask - bid) / ((bid + ask) / 2)
           ## Yes, this is supposed to be this way.
           sum(spread) / spread
         },
         "volume"={
           traded_vol[is.na(traded_vol)] <- 0
           weight(traded_vol)
         },
         "vega"=weight(vega),
         "elasticity"=weight(vega * iv / value))

}

###########################################################################
#                        Black Scholes Violations                         #
###########################################################################

remove_bs_call_violations <- function (x) {

  min_value <- ((x[["underlying"]] * exp((x[["carry"]] - x[["riskfree"]]) *
                                        x[["maturity"]])) -
                (x[["strike"]] * exp(-x[["riskfree"]] * x[["maturity"]])))
  min_value <- ifelse(min_value < 0, 0, min_value)
  
  max_value <- (x[["underlying"]] * exp((x[["carry"]] - x[["riskfree"]]) *
                                        x[["maturity"]])) 
  keep <- (x[["value"]] > min_value & x[["value"]] < max_value)

  list(x=x[keep, ], dropped_any=any(!keep))

}

remove_bs_put_violations <- function (x) {

  min_value <- (x[["strike"]] * exp(-x[["riskfree"]] * x[["maturity"]]) -
                (x[["underlying"]] * exp((x[["carry"]] - x[["riskfree"]]) *
                                        x[["maturity"]])))
  min_value <- ifelse(min_value < 0, 0, min_value)

  max_value <- (x[["strike"]] * exp(-x[["riskfree"]] * x[["maturity"]]))

  keep <- (x[["value"]] > min_value & x[["value"]] < max_value)

  list(x=x[keep, ], dropped_any=any(!keep))

}

###########################################################################
#                              VIX Functions                              #
###########################################################################

prep_maturity <- function (maturity, riskfree, carry, type, strike, underlying,
                           schemes, bid=NULL, ask=NULL, value=NULL,
                           traded_vol=NULL, tv_filter=FALSE, verbose=TRUE) {

  ## Initializing or a properly sized data.frame at the beginning or
  ## consolidating the data.frame at the end is difficult because not
  ## everything is always available. Growing the list dynamically is required
  ## to keep track of what is available and what is required.

  out <- list()

  stopifnot(length(maturity) == 1)
  if (maturity < 8 / 365)
    stop("Number of days to maturity should be at least eight.")
  out[["maturity"]] <- maturity

  stopifnot(length(riskfree) == 1)
  stopifnot(riskfree >= 0)
  out[["riskfree"]] <- riskfree

  stopifnot(length(carry) == 1)
  out[["carry"]] <- carry

  type <- as.factor(type)
  stopifnot(all(levels(type) %in% c("c", "p")))
  out[["type"]] <- type

  stopifnot(all(na.omit(strike) > 0))
  out[["strike"]] <- strike

  stopifnot(all(na.omit(underlying) > 0))
  out[["underlying"]] <- underlying

  if (!is.null(value)) {
    stopifnot(all(na.omit(value) > 0))
    out[["value"]] <- value
  }

  stopifnot(all(schemes %in% c("spread", "elasticity", "vega", "volume")))

  if (is.null(bid) || is.null(ask)) {

    if ("spread" %in% schemes)
      stop("Need bid-ask pairs to compute spread adjusted VIX.")

    if (is.null(value))
      stop("Need either `value' or bid-ask pairs to compute VIX.")

  } else {

    stopifnot(all(na.omit(bid) >= 0))
    out[["zbids"]] <- bid == 0
    out[["bid"]] <- bid

    stopifnot(all(na.omit(ask) >= 0))
    out[["non_pos_spreads"]] <- ask - bid <= 0
    out[["ask"]] <- ask
    if (is.null(value)) out[["value"]] <- (bid + ask) / 2

  }

  if (("volume" %in% schemes || tv_filter)) {
    if (is.null(traded_vol)) {
      stop("`traded_vol' needed but not supplied.")
    } else {
      stopifnot(all(na.omit(traded_vol) >= 0))
      out[["ztrades"]] <- traded_vol == 0
      out[["traded_vol"]] <- traded_vol
    }
  }

  out <- data.frame(out)
  drops <- which(colnames(out) %in% c("zbids", "ztrades", "non_pos_spreads"))
  ## Drop these whenever the need arises.

  out <- out[complete.cases(out), ]

  if (nrow(out) == 0)
    return(list("maturity"=maturity,
                "schemes"=schemes,
                "out"=out[-drops]))

  if (any(out[["zbids"]]) && verbose)
    message("Found zero bids. Dropping corresponding rows.\n")

  if (any(out[["non_pos_spreads"]]) && verbose)
    message("Found non-positive spreads. Dropping corresponding rows.\n")

  out <- out[which(!out[["zbids"]] |
                   !out[["non_pos_spreads"]]), ]
  if (nrow(out) == 0)
    return(list("maturity"=maturity,
                "schemes"=schemes,
                "out"=out[-drops]))

  if (any(out[["ztrades"]]) && tv_filter)
    out <- out[which(!out[["ztrades"]]), ]
  if (nrow(out) == 0)
    return(list("maturity"=maturity,
                "schemes"=schemes,
                "out"=out[-drops]))

  out <- out[-drops]

  # Dropping option pricing model violations.
  types <- split(out, out[["type"]])

  if (!is.null(types[["c"]])) {

    violations <- remove_bs_call_violations(types[["c"]])
    if (violations[["dropped_any"]] && verbose)
      message("Dropping call options that violate the option pricing model
              limits.\n")
    types[["c"]] <- violations[["x"]]

  }

  if (!is.null(types[["p"]])) {

    violations <- remove_bs_put_violations(types[["p"]])
    if (violations[["dropped_any"]] && verbose)
      message("Dropping put options that violate option pricing model
              limits.\n")
    types[["p"]] <- violations[["x"]]

  }

  out <- do.call(rbind, types)

  if (nrow(out) == 0)
    return(list("maturity"=maturity,
                "schemes"=schemes,
                "out"=out))

  out[["iv"]] <- try_iv(out[["type"]],
                        out[["value"]],
                        out[["underlying"]],
                        out[["strike"]],
                        out[["maturity"]],
                        out[["riskfree"]],
                        out[["carry"]])

  if (any(is.na(out[["iv"]])) && verbose) {
    message("Dropping rows with errors returned by GBSVolatility.\n")
    out <- out[which(complete.cases(out)), ]
  }

  if (nrow(out) == 0)
    return(list("maturity"=maturity,
                "schemes"=schemes,
                "out"=out))

  if (any(c("vega", "elasticity") %in% schemes))
    out[["vega"]] <- compute_vega(out[["type"]],
                                  out[["underlying"]],
                                  out[["strike"]],
                                  out[["maturity"]],
                                  out[["riskfree"]],
                                  out[["carry"]],
                                  out[["iv"]])

  return(list("maturity"=maturity,
              "schemes"=schemes,
              "out"=out))

}

weighted_iv <- function (prepped) {

  if (!is.list(prepped) ||
      !names(prepped) == c("maturity", "schemes", "out") ||
      !is.numeric(prepped[["maturity"]]) ||
      !is.character(prepped[["schemes"]]) ||
      !is.data.frame(prepped[["out"]]))
    stop("prepped not in expected format. Use `prep_maturity'.")

  maturity <- prepped[["maturity"]]
  schemes <- prepped[["schemes"]]
  out <- prepped[["out"]]

  if (nrow(out) == 0) {
    message("No valid options in the prepared data. Returning iv's as NA.")
    weighted_ivs <- rep(NA, length(schemes))
    names(weighted_ivs) <- schemes
    return(list("maturity"=maturity, "schemes"=schemes, "iv"=weighted_ivs))
  }

  if (nrow(out) == 1) {
    message("Only one option in the prepared data. Returning it's implied
            volatility as the weighted iv.")
            weighted_ivs <- rep(out[["iv"]], length(schemes))
            names(weighted_ivs) <- schemes
            return(list("maturity"=maturity,
                        "schemes"=schemes,
                        "iv"=weighted_ivs))
  }

  wts <- with(out, sapply(X=schemes,
                          FUN=get_weights,
                          bid=bid,
                          ask=ask,
                          traded_vol=traded_vol,
                          vega=vega,
                          iv=iv,
                          value=value))

  weighted_ivs <- apply(X=wts, MARGIN=2, FUN=function (k)
                        sum(out[["iv"]] * k) / sum(k))

  return(list("maturity"=maturity,
              "schemes"=schemes,
              "iv"=weighted_ivs))

}

vix_pt <- function (iv_near, iv_next) {

  if(all(is.na(iv_near[["iv"]])))
    stop("IV near is NA. Can't proceed.")

  if(all(is.na(iv_next[["iv"]])))
    stop("IV next is NA. Can't proceed.")

  if (!setequal(iv_next[["schemes"]], iv_near[["schemes"]]))
    stop("Schemes do not match for near and next maturities.")

  if (iv_near[["maturity"]] >= iv_next[["maturity"]])
    stop("Next maturity not greater than near.")

  tmp1 <- (((iv_next[["maturity"]] * 365 - 30) /
            ((iv_next[["maturity"]] - iv_near[["maturity"]]) * 365)) *
           iv_near[["iv"]])

  tmp2 <- (((30 - iv_near[["maturity"]] * 365) /
            ((iv_next[["maturity"]] - iv_near[["maturity"]]) * 365)) *
           iv_next[["iv"]])

  vix <- (tmp1 + tmp2) * 100

  return(vix)

}

iv_statistic <- function (out, indices, maturity, schemes, verbose) {

  prepped <- list("maturity"=maturity,
                  "schemes"=schemes,
                  "out"=out[indices, ])

  weighted_iv(prepped)[["iv"]] * 100

}

vix_statistic <- function (out, indices, schemes, maturities, verbose) {

  out <- out[indices, ]
  out <- split(out, out[["maturity"]])

  prep_near <- list("maturity"=maturities[["near"]], "schemes"=schemes,
                    "out"=out[[1]])
  prep_next <- list("maturity"=maturities[["next"]], "schemes"=schemes,
                    "out"=out[[2]])

  iv_near <- weighted_iv(prep_near)
  iv_next <- weighted_iv(prep_next)

  vix_pt(iv_near, iv_next)

}

construct_ci <- function (booted, conf) {

  ci <- sapply(X=seq_along(booted[["t0"]]),
               FUN=function (k) {
                 retval <- boot.ci(booted, index=k, type="bca", conf=conf)
                 retval[["bca"]][1, 4:5]
               })

  dimnames(ci) <- list(c("lower", "upper"), names(booted[["t0"]]))
  colnames(booted[["t"]]) <- names(booted[["t0"]])

  return(list("point"=booted[["t0"]], "ci"=ci, "samples"=booted[["t"]]))

}

vix_ci <- function (prep_near, prep_next=NULL, n_samples=1e3, conf=0.95,
                    verbose=TRUE, ...) {

  stopifnot(is.numeric(n_samples) && length(n_samples) == 1)

  stopifnot(is.numeric(conf) && length(conf) == 1)

  stopifnot(is.logical(verbose) && length(verbose) == 1)

  if (!(is.list(prep_near) &&
        names(prep_near) == c("maturity", "schemes", "out")))
    stop("prep_near not in expected format. Use prep_maturity().")

  if (nrow(prep_near[["out"]]) == 0)
    return(list("point"=rep(NA, length(prep_near[["schemes"]])),
                "ci"=NULL, "samples"=NULL))

  if (is.null(prep_next)) {

    if (nrow(prep_near[["out"]]) == 1)
      stop("Not enough variation in the data to compute CI. Try weighted_iv.")

    booted <- boot(data=prep_near[["out"]],
                   statistic=iv_statistic, R=n_samples,
                   schemes=prep_near[["schemes"]],
                   maturity=prep_near[["maturity"]],
                   verbose=verbose, ...)

    return(construct_ci(booted, conf))

  } else {

    if (!(is.list(prep_next) &&
          names(prep_next) == c("maturity", "schemes", "out")))
      stop("prep_next not in expected format. Use prep_maturity().")

    if (nrow(prep_next[["out"]]) == 0)
      return(list("point"=rep(NA, length(prep_near[["schemes"]])),
                  "ci"=NULL, "samples"=NULL))

    if (nrow(prep_near[["out"]]) == 1 && nrow(prep_next[["out"]]) == 1)
      stop("Not enough variation in the data to compute CI. Try vix_pt.")

    schemes <- prep_near[["schemes"]]

    out <- rbind(prep_near[["out"]], prep_next[["out"]])

    booted <- boot(data=out, statistic=vix_statistic, R=n_samples,
                   strata=out[["maturity"]],
                   schemes=prep_near[["schemes"]],
                   maturities=c("near"=prep_near[["maturity"]],
                                "next"=prep_next[["maturity"]]),
                   verbose=verbose, ...)

    return(construct_ci(booted, conf))

  }

}

###########################################################################
#                              VXO Functions                              #
###########################################################################

check_vxo_data_for_a_maturity <- function (x) {

  if (any(x[-c(2, 3, 4)] <= 0)) return(1)

  x <- x[order(x[["strike"]], x[["type"]]), ]

  if (!identical(as.character(x[["type"]]),
                 c("c", "p", "c", "p"))) return(2)

  if (!identical(x[["strike"]],
                 rep(c(x[["strike"]][[1]], x[["strike"]][[3]]),
                     each=2)))
    return(3)

  return(0)

}

avg_iv <- function (prepped) {

  prepped[["iv"]] <- try_iv(prepped[["type"]],
                            prepped[["value"]],
                            prepped[["underlying"]],
                            prepped[["strike"]],
                            prepped[["maturity"]],
                            prepped[["riskfree"]],
                            prepped[["carry"]])

  prepped <- prepped[order(prepped[["strike"]], prepped[["maturity"]]), ]

  avg_iv <- sapply(X=1:4, FUN=function(k)
                   (prepped[["iv"]][2 * k - 1] + prepped[["iv"]][2 * k]) / 2)

  strike <- unique(prepped[["strike"]])

  underlying <- unique(prepped[["underlying"]])

  if(length(underlying)==1) {
  underlying <- rep(underlying, 2)
  }

  sigma1 <- ((avg_iv[1] * (strike[2] - underlying[1]) /
              (strike[2] - strike[1])) +
             (avg_iv[3] * (underlying[1] - strike[1]) /
              (strike[2] - strike[1])))

  sigma2 <- ((avg_iv[2] * (strike[2] - underlying[2]) /
              (strike[2] - strike[1])) +
             (avg_iv[4] * (underlying[2] - strike[1]) /
              (strike[2] - strike[1])))

  maturity <- unique(prepped[["maturity"]])

  caldays <- maturity * 365 - 2 * as.integer(maturity * 365 / 7)

  sigma <- c(sigma1, sigma2)
  sigma <- (sqrt(maturity * 365) / sqrt(caldays)) * sigma

  return(list(sigma=sigma, caldays=caldays))

}

vxo <- function (maturity, riskfree, carry, type, strike, underlying,
                 bid=NULL, ask=NULL, value=NULL) {

  if (is.null(value) && any(is.null(bid), is.null(ask)))
    stop("Need either `value' or bid-ask pairs to compute VXO.")

  if (!is.null(bid)) {
    if (any(bid == 0)) stop("Found zero bid price. Can not proceed.")
    if (any(ask == 0)) stop("Found zero ask price. Can not proceed.")
    value <- (bid + ask) / 2
  }

  umat <- unique(maturity)
  if (length(umat) != 2) stop("Need exactly two unique maturities.")
  if (any(umat < 8 / 365))
    stop("Number of days to maturity should be at least eight")
  
  uund <- unique(underlying)[1]
  
  ust <- sort(unique(strike))
  if (length(ust) != 2) stop("Need exactly two unique strikes.")

  if (!(ust[1] < uund | uund <= ust[2]))
    stop("Need one strike price just above and one just below the underlying.")

  urf <- sort(unique(riskfree))
  if (length(urf) != 2) stop("Need exactly two unique riskfree rates.")
  if (any(urf < 0)) stop("A riskfree value is negative.")
 
  ucr <- sort(unique(carry))
  if (length(ucr) != 2) stop("Need exactly two unique cost-of-carry rates.")
 
  prepped <- data.frame(maturity, riskfree, carry, type, strike, underlying, 
                        value)

  if (nrow(prepped) != 8) stop("Need 8 values of each variable.")

  if (sum(complete.cases(prepped)) != 8) stop("NAs in the dataset.")

  prepped <- split(prepped, prepped[["maturity"]])

  check_near <- check_vxo_data_for_a_maturity(prepped[[1]])
  switch(check_near,
         stop("Non-positive values in the dataset."),
         stop("Need exactly two pairs of call and put for the near maturity."),
         stop("Need exactly two pairs of call and put for the near maturity."))

  check_next <- check_vxo_data_for_a_maturity(prepped[[2]])
  switch(check_next,
         stop("Non-positive values in the dataset."),
         stop("Need exactly two pairs of call and put for the next maturity."),
         stop("Need exactly two pairs of call and put for the near maturity."))

  prepped <- do.call(rbind, prepped)

  # Dropping Black-Scholes violations.
  types <- split(prepped, prepped[["type"]])

  if (!is.null(types[["c"]])) {

    violations <- remove_bs_call_violations(types[["c"]])
    types[["c"]] <- violations[["x"]]

    if (violations[["dropped_any"]])
      stop("Dropping call options that violate Black Scholes limits.\n")

  }

  if (!is.null(types[["p"]])) {

    violations <- remove_bs_put_violations(types[["p"]])
    types[["p"]] <- violations[["x"]]

    if (violations[["dropped_any"]])
      stop("Dropping put options that violate Black Scholes limits.\n")

  }

  prepped <- do.call(rbind, types)

  iv <- avg_iv(prepped)

  ((iv[[2]][2] - 22) / (diff(iv[[2]])) * iv[[1]][1] +
   (22 - iv[[2]][1]) / (diff(iv[[2]])) * iv[[1]][2]) * 100

}
