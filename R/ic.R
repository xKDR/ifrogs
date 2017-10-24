##' Execute a market order using limit order book data.
##' 
##' @title Execute market order and return execution details.
##' @param lob.p A vector of limit order book snapshot (at a
##' timestamp) of prices.
##' @param lob.q A vector of limit order book snapshot (at a
##' timestamp) of quantities at the quoted price.
##'    
##' @param Q The execution quantity desired.
##' @return A numeric vector containing:
##'  \itemize{
##'  \item q: quantity executed
##'  \item p: price at which the order was fully/ partially executed
##'  \item partial: whether it was a partial execution (0 or 1)
##' }
##' @author Chirag Anand
##' @examples
##'
##' ## Execution with lob.p and lob.q defined in the working
##' ## environment.
##'
##' lob.q <- matrix( nrow = 1, ncol = 4,
##'                  dimnames = list(c(),
##'                  c("bsq1", "bsq2",
##'                     "bsq3", "bsq4")))
##' lob.q[1, ] <- c(60, 70, 100, 50)  ## best sell quantity info
##'
##' lob.p <- matrix( nrow = 1, ncol = 4 ,
##'                  dimnames = list(c(),
##'                  c("bsp1", "bsp2",
##'                     "bsp3", "bsp4")))
##' lob.p[1, ] <- c(101, 102, 103, 103) ## best sell price info
##' str(lob.p)
##' str(lob.q)
##'
##' ans <- execute_order( lob.p, lob.q, Q = 200)
##' 
##' print(ans)
##'
##' ## Demonstration of IC functions using the market by price data
##' ## of CINEMAX from the ic_CINEMAX dataset.
##' try(data( package = "ifrogs", "ic_CINEMAX"))
##' mbp <- cbind(as.data.frame(CINEMAX[1]),
##'              as.data.frame(CINEMAX[2]),
##'              as.data.frame(CINEMAX[3]),
##'              as.data.frame(CINEMAX[4]))
##' 
##' bbqSnap <- create_snapshot( mbp, type = "bbq")
##' head(bbqSnap)
##' lob.q <- bbqSnap[75, ] ##take any random timestamp
##' head(lob.q)
##' 
##' bbpSnap <- create_snapshot( mbp, type = "bbp")
##' head(bbqSnap)
##' lob.p <- bbpSnap[75, ] ## take any random timestamp
##' str(lob.p)
##' str(lob.q)
##' ans <- execute_order( lob.p, lob.q, Q = 1500)
##' print(ans)



execute_order <- function(lob.p, lob.q, Q) {

  stopifnot(Q > 0, length(lob.p) > 0,
            length(lob.p) == length(lob.q))

  cum.lob.q <- c(0, cumsum(lob.q))
  feasible <- tail(cum.lob.q, 1)    # Is a full execution infeasible?
  if (Q > feasible) {
    execution_price <- c(feasible,
             tail(cumsum(lob.p * lob.q), 1) / feasible,
                         TRUE)
    names(execution_price) <- c( "q", "p", "partial")
    return(execution_price)
  } else {
    howdeep <- findInterval(Q, cum.lob.q) + 1 # How deep do we need to
                                              # plumb into the book?
    cum.lob.q <- cum.lob.q[1:(howdeep + 1)]
    
    cum.proceeds <- c(0, cumsum(lob.p[1:howdeep]
                                * lob.q[1:howdeep]))

    p <- approx(cum.lob.q, cum.proceeds,
                xout = Q)$y / Q
    execution_price <- c(q = Q, p = p, partial = FALSE)
    return(execution_price)
  }
}

##' Extract the list of best prices and quantities for a given mbp.
##' @title Prepare data for computing impact cost.
##' @param mbp A 'data.frame' containing market by price data.
##' Rownames of the data frame should contain the timestamp
##' corresponding to the given row.
##' @param type a character scalar: 'bsp' for sell side price,
##' 'bbp' for buy side price, 'bbq' for buy side quantity and
##' 'bsq' for sell side quantity.
##'
##' @return A 'matrix' containing prices/quantities for the buy
##' side/sell side with timestamps as rownames. Example: For type
##' = 'bsp', the fields 'bsp1' and 'bsp2' contain the list of
##' 1st and 2nd best sell prices from the LOB at every timestamp.
##'  
##'  @author Chirag Anand
##' @examples
##' 
##' ## Demonstration of IC functions using the market by
##' ## price data of CINEMAX from the ic_CINEMAX dataset.
##' 
##' try(data( package = "ifrogs", "ic_CINEMAX")) 
##' mbp <- cbind(as.data.frame(CINEMAX[1]), 
##'              as.data.frame(CINEMAX[2]), 
##'              as.data.frame(CINEMAX[3]), 
##'              as.data.frame(CINEMAX[4]))
##' str(mbp)
##'
##' ## create_snapshot for the best buy quantity.
##' bbqSnap <- create_snapshot( mbp, type = "bbq")
##' head(bbqSnap)
##'
##' ## create_snapshot for best sell price
##' bspSnap <- create_snapshot( mbp, type = "bsp")
##' head(bspSnap)
##' 
create_snapshot <- function(mbp, type) {
  cols <- grep(type, colnames(mbp), value = TRUE)
  element_snapshots <- mbp[, cols]
  element_snapshots <- as.matrix(element_snapshots)
  return(element_snapshots)
}

##' Function to compute impact cost for a security for a day.
##' @title Function to compute impact cost.
##'
##' 
##' @param mbp a data.frame, containing best price/quantity
##' pairs for buy and sell side with time stamps as row names.
##' Units:
##' \itemize{
##' \item Price (bbp/bsp): Some currency (INR, USD, etc.)
##' \item Quantities (bbq/bsq): Number of shares
##' }
##' 
##' 
##'
##' @param Q an 'integer' vector containing the quantities
##' for which to compute the Impact Cost.
##' @param d Date
##' @param partial a 'logical' scalar, telling whether to do
##' partial executions
##'
##' @return A 'list' of matrices containing buy and sell IC.
##' 'names' of the list are the Q values used. Each matrix will
##' contain rows having
##' \itemize{
##' \item buyIC percentage: numeric
##' \item sellIC percentage: numeric
##' }
##'
##' The function returns \sQuote{NA} values if \dQuote{partial} is
##'  \sQuote{FALSE}, or, if there were no orders (empty book) at a
##' particular time.For the buy and sell ic measures the
##' corresponding row name for that entry is its timestamp.
##' 
##' 
##' @author Chirag Anand
##' @examples
##' ## Demonstration of IC functions using the market by price data
##' ## of CINEMAX from the ic_CINEMAX dataset.
##' try(data(package="ifrogs", "ic_CINEMAX"))
##' mbp <- cbind(as.data.frame(CINEMAX[1]), 
##'              as.data.frame(CINEMAX[2]), 
##'              as.data.frame(CINEMAX[3]), 
##'              as.data.frame(CINEMAX[4]))
##' print(mbp)
##' 
##' ans1 <- ic( mbp, Q = 600, d, partial = FALSE)
##' head(ans1)
##'
##' ans2 <- ic( mbp, Q = 600, d, partial = TRUE)
##' head(ans2)
ic <- function(mbp, Q, d, partial = FALSE) {
  if (is.null(nrow(mbp)) | nrow(mbp) == 0) {
    warning(d,
            "No data points available.",
            call. = FALSE,
            immediate. = TRUE)
    return(NULL)
  }

  buyQty <- create_snapshot(mbp, "bbq")
  buyPrice <- create_snapshot(mbp, "bbp")
  sellQty <- create_snapshot(mbp, "bsq")
  sellPrice <- create_snapshot(mbp, "bsp")

  p.mid <- (buyPrice[, 1] + sellPrice[, 1]) / 2

  ## estimation of sell and buy IC
  IC.Q <- lapply(Q, function(qty) {

    buy.execution <- lapply(1:nrow(sellQty), function(i) {
      lob.q <- na.omit(as.vector(sellQty[i, ],
                       mode = "integer"))

      lob.p <- na.omit(as.vector(sellPrice[i, ],
                       mode = "numeric"))

      if (length(lob.q) == 0 || length(lob.p) == 0) {
          return(NA)
      }
      execute_order(lob.q = lob.q, lob.p = lob.p, Q = qty)
    })

    sell.execution <- lapply(1:nrow(buyQty), function(i) {

      lob.q <- na.omit(as.vector(buyQty[i, ],
                       mode = "integer"))

      lob.p <- na.omit(as.vector(buyPrice[i, ],
                       mode = "numeric"))

      if (length(lob.q) == 0 || length(lob.p) == 0) {
          return(NA)
      }
      execute_order(lob.q = lob.q, lob.p = lob.p, Q = qty)
    })

    if (all(is.na(sell.execution)) | all(is.na(buy.execution))) {

      warning("No execution possible.",
              call. = FALSE,
              immediate. = TRUE)

      return(NULL)
    }

    buy.execution <- do.call(rbind, buy.execution)
    sell.execution <- do.call(rbind, sell.execution)
    rownames(buy.execution) <- rownames(buyQty)
    rownames(sell.execution) <- rownames(buyQty)

    if (partial == FALSE) {             # make partial execution prices NA
      buy.execution[buy.execution[, "partial"] == 1, "p"] <- NA
      sell.execution[sell.execution[, "partial"] == 1, "p"] <- NA
    }

    buyIC <- ((buy.execution[, "p"] - p.mid) / p.mid) * 100
    sellIC <- ((sell.execution[, "p"] - p.mid) / p.mid) * 100
    IC <- cbind(sellIC, buyIC, deparse.level = 1) # don't lose the labels

    return(IC)
  })

  names(IC.Q) <- Q
  return(IC.Q)
}