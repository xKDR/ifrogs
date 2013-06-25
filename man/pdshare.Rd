\name{pdshare}

\alias{pdshare}

\title{Computes information share & component share weights} 

\description{
  This function implements the two most commonly used techniques to
  measure price discovery in multiple markets. These are information
  share (Hasbrouck (1995)) and the component share approach (Booth et
  al. (1999), Chu et al. (1999), Harris et al. (2002)) which utilises
  the Gonzalo and Granger (1995) permanent-transitory decomposition.  
}

\usage{
 pdshare(x, override.lags = NULL, lag.max = 10)
}

\arguments{
  \item{x}{
    Is a numeric matrix / data frame which has two columns with log
    prices of two markets.
  }
  \item{override.lags}{
    Is an integer that specifies user-defined lags to be used for VECM
    estimation. Uses NULL as default. See Details.
    
  }
  \item{lag.max}{
    Is an integer that specifies the maximum lag order to be used for
  selecting the number of lags in VARselect. Is used when
  `override.lags' is NULL. Uses 10 as default. 
  }
}

\details{
  The function estimates the information share (IS) and
  component share weights for a two market case. This is done by first
  estimating the VECM model for two cointegrated price series using the
  functionality in package `urca'. The subsequent VAR and VMA
  representation are obtained using package `vars'. 

  The number of lags to be used for VECM estimation can be pre-specified
  by the user in the argument `override.lags'. The default is NULL. If
  the argument is kept as NULL, then the number of lags  are decided
  based on the AIC criterion using the function `VARselect' from package
  `vars'. The maximum number of lags to be used in `VARselect' can be
  specified using the argument `lag.max'.

  To achieve the lower and upper bound of IS using triangularization of
  covariance matrix, the function first computes IS for the supplied
  ordering. The resulting IS estimate maximises the share of Market 1
  and minimises the share of Market 2. The results are saved in
  `is.original.ordering'. Subsequently, the ordering is reversed. The
  corresponding IS estimate maximises the share of Market 2 and
  minimises it for Market 1. 

}

\value{
  A list of the following five elements:
  
  \item{is.original.ordering}{Information shares of Market 1 & 2 under
    the supplied ordering} 
  
  \item{is.reversed.ordering}{Information shares of Market 2 & 1 under
    the reversed ordering}
  
  \item{component.share}{Component share weights of Market 1 & 2}
  
  \item{var.covar.matrix}{Variance covariance matrix of the residuals}
  
  \item{lags.used}{Number of lags used in VECM estimation}
}

\references{
  Booth G, So R, Tse Y (1999). Price discovery in the German equity
  index derivatives. Journal of Futures Markets, 19(6), 619-643.

  Chu QC, Hsieh WG, Tse Y (1999). Price discovery on the S&P 500 index
  markets: An analysis of spot index, index futures and SPDRs.
  International Review of Financial Analysis, 8(1), 21-34

  Gonzalo J, Granger C (1995). Estimation of common long-memory
  components in cointegrated systems. Journal of Business and Economic
  Statistics, 13(1), 27-35. 

  Harris F, McInish TH, Wood R (2002). Security price adjustments across
  exchanges: An investigation of common factor components for Dow
  stocks. Journal of Financial Markets, 5(3), 277-308.

  Hasbrouck J (1995). One security, many markets: Determining the
  contributions to price discovery. Journal of Finance, 50(4),
  1175-1199. 
}

\author{
  Nidhi Aggarwal \email{nidhi@igidr.ac.in}
}

\seealso{
  VARselect
}

\examples{
  data(is_reliance)
  head(is_reliance)      ## Two columns with data on spot and futures prices
  ln_reliance <- log(is_reliance[,-1])  ## removes the first column of datetime
                               ##  and takes log of the prices
  pdshare(ln_reliance, lag.max=120)  
  pdshare(ln_reliance, override.lags=60)	
}

\keyword{Price discovery}
\keyword{Information share}
\keyword{Component share}
