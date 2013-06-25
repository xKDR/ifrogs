\name{dtd}

\alias{dtd}

\title{Computes distance to default}

\description{
  This function implements the Merton Model (1974) to compute a measure
  of credit risk of a firm: Distance to default (DtD). DtD indicates how
  many standard deviations is a firm away from the default point. 
} 

\usage{
 dtd(mcap, debt, vol, r)
}

\arguments{
  \item{mcap}{
    Is a scalar specifying the market capital of the firm. 
  }
  \item{debt}{
    Is a scalar specifying the threshold level of debt for the
  firm below which the firm will default. Should be a non-zero number.
  }
  \item{vol}{
    Is a scalar specifying the equity volatility of the firm.
  }
  \item{r}{
    Is a scalar specifying the annualized risk free interest
    rate. 
  }
}

\details{
  DtD is calculated as the difference between the market value of the
  assets of the firm and the face value of its debt, scaled by the
  standard deviation of the firm's asset value. While the face value of
  the debt of the firm is known, the market value of the assets is not.
  
  Exploiting the option nature of equity as a European call option on
  the underlying assets of a firm, the Merton Model (1974) derives the
  implied market value of the firm's assets and its volatility by 
  solving the Black-Scholes (BS) equation backwards. 
}



\value{
  Returns a vector of three elements:

  \item{dtd.v}{Distance to default value of the firm}
  \item{asset.v}{Estimated asset value of the firm}
  \item{sigma.v}{Estimated volatility of the asset value of the firm}
}

\references{
  Merton, R.C. (1974). ``On the pricing of corporate debt: The risk structure of
  interest rate", The Journal of Finance, 29(2), 449-470
}

\author{
  Ajay Shah \email{ajayshah@mayin.org};\cr
  Manish K. Singh \email{mks344@gmail.com};\cr
  Nidhi Aggarwal \email{nidhi@igidr.ac.in}
}


\examples{
# Example 1: 
	mcap <- 10000 			## Market capital of firm X 
	debt <- 5000 			## Default point for firm X
	vol <- 0.4 			## Equity volatility of firm X
	r <- 0.1 			## Annual risk free interest rate	
	dtd(mcap, debt, vol, r) 	## Distance to default value


# Example 2: Generate a time-series of DtD

       data(dtd_reliance)       ## Load the data
       ans <- apply(X=dtd_reliance[ , -1], MARGIN=1,   
                   FUN=function (i) dtd(mcap=i[["E"]],
                   vol=i[["sE"]], debt=i[["F"]], r=0.05))

       ans <- data.frame(date=dtd_reliance[ , "date"], t(ans))
       head(ans)
}

\keyword{Credit risk}
\keyword{Distance to default}
\keyword{Merton Model}

