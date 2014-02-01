\name{prep_maturity}

\alias{prep_maturity}

\title{Prepares a cross-section of data on options for a maturity to feed to
  \sQuote{weighted_iv} and \sQuote{vix_ci}.}

\description{
  This function consumes a cross-section of data on options for a maturity
  along with a set of weighting schemes. The function checks for sufficiency of
  data for calculation of IV and VIX for the schemes provided. Further, it
  filters out options with non-positive spreads and zero bid / ask prices.
  Options with prices that violate the no-arbitrage band of the option pricing
  model specified are also dropped. Filtering of options with zero traded
  volume is optional.
}

\usage{
  prep_maturity(maturity, riskfree, carry, type, strike, underlying, 
                schemes, bid=NULL, ask=NULL, value=NULL, traded_vol=NULL,
                tv_filter=FALSE, verbose=TRUE)
}

\arguments{
  \item{maturity}{
    A numeric value that specifies time to expiration in years.
  }
  \item{riskfree}{
    A numeric value of the annualized risk-free interest rate to expiration;
    e.g. 0.16 means 16\% pa.
  }
   \item{carry}{
    A numeric value of the annualized cost-of-carry rate; e.g. 0.10 means 10\%
    pa.
  }
  \item{type}{
    A character vector of strings that specifies \sQuote{\dQuote{c}} for a call
    option and \sQuote{\dQuote{p}} for a put option.
  }
  \item{strike}{
    A numeric vector of excercise prices corresponding to each element in
    \sQuote{type}.
  }
  \item{underlying}{
    A numeric vector of the asset prices corresponding to each element in
    \sQuote{type}.
  }
  \item{schemes}{
    A character string/vector of strings that specifies which scheme(s) to
    employ. The four schemes corresponding to each volatility index are:
    \sQuote{\dQuote{vega}}, \sQuote{\dQuote{spread}}, \sQuote{\dQuote{volume}},
    and \sQuote{\dQuote{elasticity}}.
  }
  \item{bid}{
    A numeric vector of best buy prices corresponding to each element in
    \sQuote{type}.
    It is a required argument when \dQuote{\sQuote{spread}} is one of the
    schemes.
  }
  \item{ask}{
    A numeric vector of best sell prices corresponding to each element in
    \sQuote{type}.
    It is a required argument when \dQuote{\sQuote{spread}} is one of the
    schemes.
  }
  \item{value}{
    An optional numeric vector of option prices corresponding to each element
    in \sQuote{type} when bid and ask prices are not available. If \sQuote{bid}
    and \sQuote{ask} prices are provided then \sQuote{value} is computed as
    \sQuote{(bid + ask) / 2} and any provided \sQuote{value} is ignored.
  }
  \item{traded_vol}{
    A numeric vector corresponding to each element in \sQuote{type}. It is
    required when \sQuote{\dQuote{volume}} is one of the \sQuote{schemes} or
    \sQuote{tv_filter = TRUE}.
  }
  \item{tv_filter}{
    A logical value indicating whether options with zero traded value be
    discarded from the cross-section.
  }
  \item{verbose}{
    A logical value indicating whether warning messages be printed when
    dropping invalid options.
  }
}

\value{
  Returns a list of the following three elements:
  \item{maturity}{Time to expiration in years as passed to prep_maturity.}
  \item{schemes}{The weighting scheme(s) as passed to prep_maturity.}
  \item{out}{A data frame of the filtered inputs, implied volatilities, and
    vega values.}
}

\details{
  The function checks for the validity of inputs for a cross-section of options
  for a maturity and weighting scheme. The maturity used must have at least
  eight days to expiration.

  A number of filters are applied to discard invalid/illiquid options: options
  with zero bid / ask prices, negative spreads, and option prices
  (\sQuote{value}) that violate the no-arbitrage bounds of the option prcing
  model. Optionally, if traded volumes are available, one may drop options with
  zero traded volume by using the toggle \sQuote{tv_filter}. 
  
  The function also computes the implied volatility of each option using an
  option pricing model. Note, that setting \sQuote{carry = riskfree} we get
  Black and Scholes' stock option model, \sQuote{carry = riskfree-div} we get
  Merton's stock option model with continuous dividend yield \sQuote{div},
  \sQuote{carry = 0} we get Black's futures option model, and \sQuote{carry =
  riskfree-riskfreef} we get Garman and Kohlhagen's currency option model with
  foreign interest rate \sQuote{riskfreef} (using GBSVolatility from package
  \sQuote{fOptions}). In addtion, for the Black's Model on futures the
  \sQuote{underlying} value specified should be the futures price. If
  \sQuote{schemes} specified include either \sQuote{\dQuote{vega}} or
  \sQuote{\dQuote{elasticity}}, the vega of each option is computed (using
  GBSGreeks from package \sQuote{fOptions}) and returned in the prepared data.

  Note: It is possible that the cleaning up of the dataset produces an
  \sQuote{out} in the returned value which is empty. This is by design and
  handled in \sQuote{vix_ci} and \sQuote{vix_pt}.
}

\references{
  Black,  F. and Scholes,  M. (1973).  ``The Pricing of Options and Corporate
  Liabilities'', Journal of Political Economy 81, 637-654.
}

\author{
  Akhil S. Behl \email{akhilsbehl@gmail.com}; \cr
  Rohini Grover \email{grover.rohini@gmail.com}
}

\seealso{GBSVolatility, GBSGreeks}

\examples{
  # Example 1: SPX near month options and the Vega scheme.

  data(vix_spx)

  # Preparing data for near month spx options without filtering options with
  # zero traded volume.
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
  str(spx_near)

  # Example 2: NIFTY near month options and the Spread, Elasticity, and Vega
  # schemes.

  data(vix_nifty)

  # Preparing data for near month nifty options and filtering options with
  # zero traded volume.
  nifty_near <- prep_maturity(maturity=vix_nifty$opt_near$maturity[[1]],
                              riskfree=vix_nifty$opt_near$riskfree[[1]],
                              carry=vix_nifty$opt_near$riskfree[[1]],
                              type=vix_nifty$opt_near$type,
                              strike=vix_nifty$opt_near$strike,
                              underlying=vix_nifty$opt_near$underlying,
                              schemes=c("spread", "elasticity", "vega"),
                              bid=vix_nifty$opt_near$bid,
                              ask=vix_nifty$opt_near$ask,
                              traded_vol=vix_nifty$opt_near$traded_vol,
                              tv_filter=TRUE)
  str(nifty_near)
}
