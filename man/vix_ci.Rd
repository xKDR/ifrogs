\name{vix_ci}

\alias{vix_ci}

\title{Computes confidence interval for model-based volatility indexes}

\description{
  This function implements the methodology for constructing confidence
  intervals for model based volatility indexes as described in Grover and Shah
  (2013). The four model based indexes provided are described in Grover and
  Thomas (2012).
}

\usage{
vix_ci(prep_near, prep_next, n_samples=1e3, conf=0.95, verbose=TRUE, ...)
}

\arguments{
  \item{prep_near}{
    A list of objects returned by \sQuote{prep_maturity} for the near month
    maturity.
  }
  \item{prep_next}{
    A list of objects returned by \sQuote{prep_maturity} for the next month
    maturity. If not available it is set as \sQuote{NULL}.
  }
  \item{n_samples}{
    A positive integer of the number of bootstrap replicates.
  }
  \item{conf}{
    A scalar containing the confidence level of the required interval.
  }
  \item{verbose}{
    A logical value indicating whether warning messages from \sQuote{boot} be
    printed.
  }
  \item{...}{
    Further arguments to be passed to \sQuote{boot}.
  }
}

\details{
  The function consumes as \sQuote{prep_near} and \sQuote{prep_next} the output
  returned by \sQuote{prep_maturity} for each maturity. 

  If options are not available for the next maturity, \code{prep_next=NULL} may
  be used to compute the confidence interval for weighted average IV for only
  the near maturity. In this case the weighted average implied volatility is
  converted to a percentage.

  The adjusted bootstrap percentile method of Efron (1987) is used to compute
  the confidence interval.
}

\value{
  Returns a list of following three elements.
  \item{point}{
    A numeric value/vector of the point estimates of VIXes.
  }
  \item{ci}{
    The lower and upper confidence limits corresponding to each volatility
    index computed.
  }
  \item{samples}{
    The bootstrap replicates of the volatility index/indexes computed.
  }
}

\references{
  Grover, R. and Thomas, S. (2012). ``Liquidity Considerations in Estimating
  Implied Volatility'', Journal of Futures Market, 32,  714 - 741.

  Grover, R. and Shah, A. (2013). ``The imprecision of volatility indexes''.
}

\author{
  Akhil S. Behl \email{akhilsbehl@gmail.com}; \cr
  Rohini Grover \email{grover.rohini@gmail.com}
}

\seealso{boot.ci, prep_maturity, weighted_iv}

\examples{
  # Example 1: Construct confidence interval for Vega VIX using SPX options

  set.seed(101)
  data(vix_spx)

  # Preparing data for near and next month spx options without filtering
  # options with zero traded volume.
  spx_near <- prep_maturity(maturity=vix_spx$opt_near$maturity[[1]],
                            riskfree=vix_spx$opt_near$riskfree[[1]],
                            carry=vix_spx$opt_near$riskfree[[1]],
                            type=vix_spx$opt_near$type,
                            strike=vix_spx$opt_near$strike,
                            underlying=vix_spx$opt_near$underlying,
                            schemes="vega",
                            bid=vix_spx$opt_near$bid,
                            ask=vix_spx$opt_near$ask,
                            tv_filter=FALSE)

  spx_next <- prep_maturity(maturity=vix_spx$opt_next$maturity[[1]],
                            riskfree=vix_spx$opt_next$riskfree[[1]],
                            carry=vix_spx$opt_next$riskfree[[1]],
                            type=vix_spx$opt_next$type,
                            strike=vix_spx$opt_next$strike,
                            underlying=vix_spx$opt_next$underlying,
                            schemes="vega",
                            bid=vix_spx$opt_next$bid,
                            ask=vix_spx$opt_next$ask,
                            tv_filter=FALSE)

  # Confidence interval for Vega VIX
  spx_ci <- vix_ci(prep_near=spx_near,
                   prep_next=spx_next,
                   n_samples=1e3, conf=0.95,
                   verbose=TRUE)

  str(spx_ci)

  # Point estimate
  spx_ci$point

  # Confidence interval
  spx_ci$ci

  # Bootstrap replicates
  head(spx_ci$samples)

  # Example 2: Compute Vega VIX using NIFTY options
  set.seed(101)
  data(vix_nifty)

  # Preparing data for near and next month nifty options and filtering options
  # with zero traded volume.
  nifty_near <- prep_maturity(maturity=vix_nifty$opt_near$maturity[[1]],
                              riskfree=vix_nifty$opt_near$riskfree[[1]],
                              carry=vix_nifty$opt_near$riskfree[[1]],
                              type=vix_nifty$opt_near$type,
                              strike=vix_nifty$opt_near$strike,
                              underlying=vix_nifty$opt_near$underlying,
                              schemes="vega",
                              bid=vix_nifty$opt_near$bid,
                              ask=vix_nifty$opt_near$ask,
                              traded_vol=vix_nifty$opt_near$traded_vol,
                              tv_filter=TRUE)

  nifty_next <- prep_maturity(maturity=vix_nifty$opt_next$maturity[[1]],
                              riskfree=vix_nifty$opt_next$riskfree[[1]],
                              carry=vix_nifty$opt_next$riskfree[[1]],
                              type=vix_nifty$opt_next$type,
                              strike=vix_nifty$opt_next$strike,
                              underlying=vix_nifty$opt_next$underlying,
                              schemes="vega",
                              bid=vix_nifty$opt_next$bid,
                              ask=vix_nifty$opt_next$ask,
                              traded_vol=vix_nifty$opt_next$traded_vol,
                              tv_filter=TRUE)
  # Confidence interval for Vega VIX
  nifty_ci <- vix_ci(prep_near=nifty_near,
                    prep_next=nifty_next,
                    n_samples=1e3, conf=0.95,
                    verbose=TRUE)

  str(nifty_ci)

  # Point estimate
  nifty_ci$point

  # Confidence interval
  nifty_ci$ci

  # Bootstrap replicates
  head(nifty_ci$samples)
}
