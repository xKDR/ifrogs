context("VIX")

test_that("test.weighted_iv", {
  load(system.file("data", "vix_spx.RData", package = "ifrogs"))
  load(system.file("data", "vix_nifty.RData", package = "ifrogs"))

                                        # load the test output data
  load(system.file("tests", "data_weighted_iv.RData", package = "ifrogs"))
 
  test_spx_near <- prep_maturity(maturity=vix_spx$opt_near$maturity[[1]],
                                 riskfree=vix_spx$opt_near$riskfree[[1]],
                                 carry=vix_spx$opt_near$riskfree[[1]],
                                 type=vix_spx$opt_near$type,
                                 strike=vix_spx$opt_near$strike,
                                 underlying=vix_spx$opt_near$underlying,
                                 schemes="vega",
                                 bid=vix_spx$opt_near$bid,
                                 ask=vix_spx$opt_near$ask,
                                 tv_filter=FALSE)
                            
  test_spx_near_iv <- weighted_iv(prepped=test_spx_near)

  cat("\ntest.weighted_iv: spx_near ")
  expect_that(test_spx_near, equals(spx_near))
 
  cat("\ntest.weighted_iv: spx_near_iv ")
  expect_that(test_spx_near_iv, equals(spx_near_iv))
   
  ##
  test_nifty_near <- prep_maturity(maturity=vix_nifty$opt_near$maturity[[1]],
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

  test_nifty_near_iv <- weighted_iv(prepped=test_nifty_near)
  
  cat("\ntest.weighted_iv: nifty_near ")
  expect_that(test_nifty_near, equals(nifty_near))
  
  cat("\ntest.weighted_iv: nifty_near_iv ")
  expect_that(test_nifty_near_iv, equals(nifty_near_iv))
})
