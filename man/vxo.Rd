\name{vxo}

\alias{vxo}

\title{Calculates the old CBOE VIX also known as VXO}

\description{
  This function calculates the volatility index proposed by Whaley (1993) and
  introduced by the Chicago Board of Exchange known as the old CBOE VIX.
}

\usage{
 vxo(maturity, riskfree, carry, type, strike, underlying, bid=NULL, ask=NULL,
    value=NULL)
}

\arguments{
  \item{maturity}{
    A numeric vector specifying time to expiration in years corresponding to
    each element in \sQuote{type}.
  }
 \item{riskfree}{
    A numeric value of the annualized risk-free interest rate to expiration;
    e.g. 0.16 means 16\% pa corresponding to each element in \sQuote{type}.
  }
   \item{carry}{
    A numeric value of the annualized cost-of-carry rate; e.g. 0.10 means 10\%
    pa corresponding to each element in \sQuote{type}.
  }
  \item{type}{
    A character vector specifying the option type \sQuote{\dQuote{c}} and
    \sQuote{\dQuote{p}} for call and put options respectively.
  }
  \item{strike}{
    A numeric vector with excercise prices corresponding to each element in
    \sQuote{type}.
  }
  \item{underlying}{
    A numeric vector of the asset prices corresponding to each element in
    \sQuote{type}.
  }
  \item{bid}{
    A numeric vector of best buy prices corresponding to each element in
    \sQuote{type}.
  }
  \item{ask}{
    A numeric vector of best sell prices corresponding to each element in
    \sQuote{type}.
  }
  \item{value}{
    An optional numeric vector of option prices corresponding to each element
    in \sQuote{type} when bid and ask prices are not available. If \sQuote{bid}
    and \sQuote{ask} prices are provided then \sQuote{value} is computed as
    \sQuote{(bid + ask) / 2} and any provided \sQuote{value} is ignored.
  }
}

\details{
  The function calculates the old CBOE volatility index. The computation
  requires eight at-the-money options for two nearest maturities. Therefore
  each input to the function is a vector of length eigth with elements at each
  index value corresponding to that option.

  The eight options are selected in the following fashion: there should be a
  pair of call and put options for two unique strikes, one directly above and
  the other directly below the underlying value. The near month maturity must
  have at least eight days to maturity.

  The implied volatilities are computed for each option by using an option
  pricing model. Note, that setting \sQuote{carry = riskfree} we get Black and
  Scholes' stock option model, \sQuote{carry = riskfree-div} we get Merton's
  stock option model with continuous dividend yield \sQuote{div}, \sQuote{carry
  = 0} we get Black's futures option model, and \sQuote{carry =
  riskfree-riskfreef} we get Garman and Kohlhagen's currency option model with
  foreign interest rate \sQuote{riskfreef} (using GBSVolatility from package
  \sQuote{fOptions}). In addtion, for the Black's Model on futures the
  \sQuote{underlying} value specified should be the futures price.

  For further details on the computation procedure please see Whaley (1993).
}

\value{
 Returns VXO value: a numeric scalar.
}

\references{
  Whaley,  R.E. (1993). ``Derivatives on market volatility: hedging tools long
  over due'', Journal of Derivatives 1, 71-84.
}

\author{
  Akhil S. Behl \email{akhilsbehl@gmail.com}; \cr
  Rohini Grover \email{grover.rohini@gmail.com}
}

\examples{
  # Example 1: SPX options to compute VXO

  data(vxo_spx)

  vxo(maturity=vxo_spx$maturity, riskfree=vxo_spx$riskfree,
     carry=vxo_spx$riskfree, type=vxo_spx$type, strike=vxo_spx$strike,
     underlying=vxo_spx$underlying, bid=vxo_spx$bid, ask=vxo_spx$ask)

  # Example 2: NIFTY options to compute VXO
  data(vxo_nifty)

   vxo(maturity=vxo_nifty$maturity, riskfree=vxo_nifty$riskfree,
      carry=vxo_nifty$riskfree, type=vxo_nifty$type, strike=vxo_nifty$strike,
      underlying=vxo_nifty$underlying, bid=vxo_nifty$bid, ask=vxo_nifty$ask)
}
