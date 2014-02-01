\name{vix_pt}

\alias{vix_pt}

\title{Computes point estimates of model-based volatility indexes} 

\description{
  This function implements the point estimation of the four model-based
  volatility indexes described in  Grover and Thomas (2012). They are: Vega
  weighted VIX (VVIX), Spread Weighted VIX (SVIX), Traded Volume Weighted VIX
  (TVVIX), and Elasticity weighted  VIX (EVVIX).
}

\usage{
 vix_pt(iv_near, iv_next)
}

\arguments{
  \item{iv_near}{
    A list of objects returned by \sQuote{weighted_iv} for the near month
    maturity.
  }
  \item{iv_next}{
    A list of objects returned by \sQuote{weighted_iv} for the next month
    maturity.
  }
}

\value{
  \item{vix}{Returns a numeric value/vector of the volatility index/indexes
    computed.} 
}

\references{
  Grover, R. and Thomas, S. (2012). ``Liquidity Considerations in Estimating
  Implied Volatility'',  Journal of Futures Market, 32,  714 - 741.
}

\author{
  Akhil S. Behl \email{akhilsbehl@gmail.com}; \cr
  Rohini Grover \email{grover.rohini@gmail.com}
}

\seealso{prep_maturity, weighted_iv, vix_ci, prep_vxo, vxo}

\examples{
  # Example 1: Compute Vega VIX using SPX options

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

  # Weighted average implied volatility for near and next maturity                           
  spx_near_iv <- weighted_iv(prepped=spx_near)
  spx_next_iv <- weighted_iv(prepped=spx_next)
  
  # Point estimate of Vega VIX
  spx_vvix <- vix_pt(iv_near=spx_near_iv, 
                      iv_next=spx_next_iv)
  spx_vvix

  # Example 2: Compute Spread, Elasticity, and Vega VIX using NIFTY options 
  
  data(vix_nifty)

  # Preparing data for near and next month nifty options and filtering options
  # with zero traded volume.
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

  nifty_next <- prep_maturity(maturity=vix_nifty$opt_next$maturity[[1]],
                              riskfree=vix_nifty$opt_next$riskfree[[1]],
                              carry=vix_nifty$opt_next$riskfree[[1]],
                              type=vix_nifty$opt_next$type,
                              strike=vix_nifty$opt_next$strike,
                              underlying=vix_nifty$opt_next$underlying,
                              schemes=c("spread", "elasticity", "vega"),
                              bid=vix_nifty$opt_next$bid,
                              ask=vix_nifty$opt_next$ask,
                              traded_vol=vix_nifty$opt_next$traded_vol,
                              tv_filter=TRUE)

  # Weighted average implied volatilities
  nifty_near_iv <- weighted_iv(prepped=nifty_near)
  nifty_next_iv <- weighted_iv(prepped=nifty_next)
 
  # Point estimates of VIXs
  nifty_vixes <- vix_pt(iv_near=nifty_near_iv, 
                        iv_next=nifty_next_iv)
  nifty_vixes
}
