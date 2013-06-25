# Function to calculate Distance-to-default

dtd <- function(mcap, debt, vol, r){
  if(debt==0)stop("Please provide a non-zero debt value")
  stopifnot(is.numeric(mcap),
            is.numeric(debt),
            is.numeric(vol),
            is.numeric(r))
  
  rho <- 1                 # forbearance
  Maturity <- 1

  ## Starting values of firm's market value and its volatility
  seed.V <- mcap + debt
  seed.sV <- mcap * vol / debt
  
  # Present value of debt
  debt <- debt * exp(-r)
    
  # Solving reverse Black-Scholes for market value of asset and
  # asset volatility  
  d1 <- function(V, debt, sV, Maturity) {
    num <- log(V/debt) + 0.5*sV*sV*Maturity
    den <- sV * sqrt(Maturity)
    num/den
  }
    
  d2 <- function(V, debt, sV, Maturity) {
    d1(V, debt, sV, Maturity) - sV*sqrt(Maturity)
  }

  
  ## Feed this function the parameter vector x.
  ## Error term computation:
  ## It returns the sum of squared errors for the two equations
  ## x[1] is V and x[2] is sV. 
  
  objective.function <- function(x, mcap, vol, debt, rho, Maturity){

    e1 <- -mcap + x[1]*pnorm(d1(x[1], debt*rho, x[2], Maturity)) -
      rho*debt*pnorm(d2(x[1], rho*debt, x[2], Maturity))
    
    e2 <- -vol*mcap + x[2]*x[1]*pnorm(d1(x[1], debt*rho, x[2], Maturity))
    
    (e1*e1) + (e2*e2)
  }
  
  # Solve it - Minimizing the error term
  res <- optim(c(seed.V, seed.sV),
               method="L-BFGS-B",
               fn=objective.function,
               lower=c(mcap, 0), upper=c(Inf, Inf),
               mcap=mcap, vol=vol, debt=debt, Maturity=Maturity,
               rho=rho)
  
  # Distance-to-default calculation
  dtd.v <- (res$par[1] - debt)/(res$par[1]*res$par[2])
  return(c("dtd.v"=dtd.v, "asset.v"=res$par[1], "sigma.v"=res$par[2]))
#  list(dtd.v=dtd.v, asset.v=res$par[1], sigma.v=res$par[2])  	
} 


