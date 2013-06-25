# Estimate information share and component share weights.
library(vars)
library(urca)

# The function `pdshare' implements the two techniques and returns the 
# estimates of information share and component share weights 

# Args:
#   x:              matrix / data frame which has two columns with log
#                   prices of two markets 

#   override.lags:  integer specifying user-defined lags for VECM
#                   estimation 
    
#   lag.max:        integer specifying the maximum number of lags for
#                   VARselect 

# Returns:
#   is.original.ordering  Information shares under the supplied ordering
#   is.reversed.ordering  Information shares under the reverse ordering
#   component.share       Component share weights
#   var.covar.matrix      Var-covar matrix of residuals
#   lags.used             Lags used in VECM estimation    


pdshare <- function(x, override.lags = NULL, lag.max = 10) {
  stopifnot(ncol(x)==2)
  stopifnot(is.numeric(x[,1]))
  stopifnot(is.numeric(x[,2]))
  if (is.null(override.lags)){
    if(lag.max<2) stop("Minimum lags should be 2")
  } else {
    if(override.lags<2) stop("Minimum lags should be 2")
  }
  cnames <- colnames(x)
  pdshare.computation <- function(x, nlag) {
    cointest <- ca.jo(x, K = nlag, type = "eigen", ecdet = "const",
    spec = "transitory")  
    k <- cointest@lag
    vecm <- cajorls(cointest)
    varm <- vec2var(cointest)
    vma <- Psi(varm)
    ## converts level VARS to VMA model and gives orthogonalised psi
    ## matrix. 

    ## Head towards IS
    ## We need Psi(1), the matrix that captures the long run impact 
    ## of a disturbance on each of the prices.
    ## Psi(1) = beta.orthogonal*
    ##        [inverse(transpose(alpha.orthogonal) *gamma*
    ## (beta.orthogonal))] * transpose(alpha.orthogonal)

    ## the beta_orthogonal and alpha_orthogonal vectors :
   beta.ort <- as.vector(c(-cointest@V[2,1], cointest@V[1,1]))
   alpha.ort <- as.vector(c(-cointest@W[2,1], cointest@W[1,1]))

    ## initializing the parameters of gamma matrix
    aa <- bb <- cc <- dd <- 0
    for (i in 1:(k-1)) {
      aa <- aa + vecm$rlm$coefficients[2*i,1]
      bb <- bb + vecm$rlm$coefficients[2*i+1,1]
      cc <- cc + vecm$rlm$coefficients[2*i,2]
      dd <- dd + vecm$rlm$coefficients[2*i+1,2]
    }
    gamma.1 <- matrix(c(1-aa, -bb, -cc, 1-dd), nrow = 2, ncol = 2, byrow
    = TRUE) 

    b <- as.numeric(t(alpha.ort) %*% gamma.1 %*% beta.ort)
    psi <- (beta.ort %*% t(alpha.ort))/b

    ## Information share is: (psi[1,]* f)_j^2) /
    ##                       (psi[1,]*omega*transpose(psi[1,])) 
    ## where f is the cholesky factorization of the omega matrix. 
   
    f <- vma[,,1]
    omega <- f %*% t(f)
    psi <- t(psi[1,])
    n <- psi %*% f
    d <- psi %*% omega %*% t(psi)

    list(ishares = c((n[, 1]^2)/d, (n[, 2]^2)/d), alpha.ort = alpha.ort, 
         omega = omega, lags = varm$p)
  }

                                        # Choosing the number of lags
  if (is.null(override.lags)) {
    nlag <- MVARselect(x, lag.max=lag.max)$selection[1] 
  } else {
    nlag <- override.lags
  }

  # First do the supplied ordering
  tmp <- pdshare.computation(x, nlag)
  is.original.ordering <- as.data.frame(tmp$ishares)
  component.share <- as.data.frame(abs(tmp$alpha.ort)/sum(abs(tmp$alpha.ort)))
  var.covar.matrix <- tmp$omega
  lags.used <- tmp$lags

  # Do the reverse ordering
  tmp <- pdshare.computation(x[,c(2,1)], nlag)
  is.reversed.ordering <- as.data.frame(tmp$ishares)

  
  rownames(var.covar.matrix) <- colnames(var.covar.matrix) <-
    rownames(component.share) <- rownames(is.original.ordering) <- cnames
  rownames(is.reversed.ordering) <- c(cnames[2], cnames[1])
  colnames(is.original.ordering) <- colnames(is.reversed.ordering) <-
    "IS" 
  colnames(component.share) <- "CS"
  
  
  list(is.original.ordering = is.original.ordering,
       is.reversed.ordering = is.reversed.ordering,
       component.share = component.share,
       var.covar.matrix = var.covar.matrix,
       lags.used = lags.used)
}

