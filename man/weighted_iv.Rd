\name{weighted_iv}

\alias{weighted_iv}

\title{Computes weighted average implied volatility for a maturity} 

\description{
  This function computes the weighted average implied volatility corresponding
  to four weighting schemes \sQuote{\dQuote{vega}}, \sQuote{\dQuote{spread}},
  \sQuote{\dQuote{volume}}, and \sQuote{\dQuote{elasticity}} discussed in
  Grover and Thomas (2012).
 }

\usage{
 weighted_iv(prepped)
}

\arguments{
  \item{prepped}{A list of objects returned by prep_maturity for a given
    maturity.}
}

\details{
  This function consumes as \sQuote{prepped} the output of the function
  \sQuote{prep_maturity}.
}

\value{
  Returns a list of the following three elements:
  \item{maturity}{Time to expiration in years as passed to
    weighted_iv via the object returned by prep_maturity.} 
  \item{schemes}{The weighting scheme(s) as passed to weighted_iv via the
    object returned by prep_maturity.}
  \item{iv}{The weighted average implied volatility corresponding to each
    weighting schemes.}	
}

\references{
  Grover, R. and Thomas, S. (2012). ``Liquidity Considerations in Estimating
  Implied Volatility'',  Journal of Futures Market, 32,  714 - 741.
}

\author{
  Akhil S. Behl \email{akhilsbehl@gmail.com}; \cr
  Rohini Grover \email{grover.rohini@gmail.com}
}

\seealso{prep_maturity}

\examples{
  # Example 1: Weighted average implied volatility for SPX near month options
  # using the Vega scheme.

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

  # Weighted average implied volatility                              
  spx_near_iv <- weighted_iv(prepped=spx_near)
  spx_near_iv

  # Example 2: Weighted average implied volatility for NIFTY near month options
  # for the Spread, Elasticity and Vega weighting schemes.
  
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

  # Weighted average implied volatility
  nifty_near_iv <- weighted_iv(prepped=nifty_near)
  nifty_near_iv
}
