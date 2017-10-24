## load the CINEMAX ic data from the ic_CINEMAX dataset.
 require("testthat")
 try(data(package="ifrogs","ic_CINEMAX"))

 mbp <- cbind(as.data.frame(CINEMAX[1]),
              as.data.frame(CINEMAX[2]),
              as.data.frame(CINEMAX[3]),
              as.data.frame(CINEMAX[4]))
 
  
library("testthat")
context("ic")


test_that("test_execute_order for Q = 1500 (partial execution of order)", {
  load(system.file("tests", "data_ic.Rdata", package = "ifrogs"))
   
  bbqSnap <- create_snapshot(mbp,type="bbq")
  lob.q <-bbqSnap[75,] ##take any random timestamp
  bbpSnap <- create_snapshot(mbp,type="bbp")
  lob.p <-bbpSnap[75,] ##take any random timestamp
  names(lob.p) <- NULL
  names(lob.q) <- NULL

  
                                        # to test execute_orders
  test_execute_order_tmp <- execute_order(lob.p, lob.q,
                                          Q = 1500)
                            
  cat("\nTesting execute_order for Q=1500 (partial execution)")
  expect_identical(test_execute_order_tmp,
               execute_order_result$partial_order_execution)

})  


test_that("test_execute_order for Q=900 (full execution)", {
  load(system.file("tests", "data_ic.Rdata", package = "ifrogs"))
   
  bbqSnap <- create_snapshot(mbp,type="bbq")
  lob.q <-bbqSnap[75,] ##take any random timestamp
  bbpSnap <- create_snapshot(mbp,type="bbp")
  lob.p <-bbpSnap[75,] ##take any random timestamp
  names(lob.p) <- NULL
  names(lob.q) <- NULL

  
                                        # to test execute_orders
  test_execute_order_tmp <- execute_order(lob.p, lob.q,
                                          Q = 900)
                            
  cat("\nTesting execute_order for Q=900 (full execution)")
  expect_identical(test_execute_order_tmp,
               execute_order_result$full_order_execution)

})  


test_that("test_execute_order for Q=1108 (boundary case)", {
   load(system.file("tests", "data_ic.Rdata", package = "ifrogs"))
   
  bbqSnap <- create_snapshot(mbp,type="bbq")
  lob.q <-bbqSnap[75,] ##take any random timestamp
  bbpSnap <- create_snapshot(mbp,type="bbp")
  lob.p <-bbpSnap[75,] ##take any random timestamp
  names(lob.p) <- NULL
  names(lob.q) <- NULL
  
                                        # to test execute_orders

  test_execute_order_tmp <- execute_order(lob.p, lob.q,
                                          Q = 1108)
  cat("\nTesting execute_order for Q=1108 (Boundary Value)")
  expect_identical(test_execute_order_tmp,
               execute_order_result$boundary_case)


})  


test_that("test_create_snapshot", {
  load(system.file("tests", "data_ic.Rdata", package = "ifrogs"))
  
                                        # to test create_snapshot
  test_create_snapshot_bsp_tmp <- create_snapshot(mbp, type = "bsp")
                            
  
  cat("\nTesting create_snapshot for type = 'bsp' ")
  expect_identical(test_create_snapshot_bsp_tmp,
               create_snapshot_bsp_result)

  test_create_snapshot_bsq_tmp <- create_snapshot(mbp, type = "bsq")

  cat("\nTesting create_snapshot for type = 'bsq' ")
  expect_identical(test_create_snapshot_bsq_tmp,
               create_snapshot_bsq_result)

  
})  

test_that("test_ic", {
  load(system.file("tests", "data_ic.Rdata", package = "ifrogs"))
  
                                        # to test function ic
  test_ic_partial_false_tmp <- ic( mbp,
                                  Q = 600 ,
                                  partial = FALSE )
                            
  
  cat("\nTesting ic for partial = false ")
  expect_identical(test_ic_partial_false_tmp,
               test_ic_partial_false_result)

  test_ic_partial_true_tmp <- ic( mbp,
                                  Q = 600 ,
                                  partial = TRUE )
  
  cat("\nTesting ic for partial = true ")
  expect_identical(test_ic_partial_true_tmp,
               test_ic_partial_true_result)

  
})  