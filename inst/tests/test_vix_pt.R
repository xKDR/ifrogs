context("VIX")

test_that("test.vix_pt", {
  load(system.file("data", "vix_spx.RData", package = "ifrogs.dev"))
  load(system.file("data", "vix_nifty.RData", package = "ifrogs.dev"))

                                        # load the test output data
  load(system.file("tests", "data_vix_pt.RData", package = "ifrogs.dev"))

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

  test_spx_next <- prep_maturity(maturity=vix_spx$opt_next$maturity[[1]],
                                 riskfree=vix_spx$opt_next$riskfree[[1]],
                                 carry=vix_spx$opt_next$riskfree[[1]],
                                 type=vix_spx$opt_next$type,
                                 strike=vix_spx$opt_next$strike,
                                 underlying=vix_spx$opt_next$underlying,
                                 schemes="vega",
                                 bid=vix_spx$opt_next$bid,
                                 ask=vix_spx$opt_next$ask,
                                 tv_filter=FALSE)

  test_spx_near_iv <- weighted_iv(prepped=test_spx_near)  
  test_spx_next_iv <- weighted_iv(prepped=test_spx_next)
  
  test_spx_vvix <- vix_pt(iv_near=test_spx_near_iv, 
                      iv_next=test_spx_next_iv)
  
  cat("\ntest.weighted_iv: spx_near ")
  expect_that(test_spx_near, equals(spx_near))
  
  cat("\ntest.weighted_iv: spx_next ")
  expect_that(test_spx_next, equals(spx_next))

  cat("\ntest.weighted_iv: spx_near_iv ")
  expect_that(test_spx_near_iv, equals(spx_near_iv))
   
  cat("\ntest.weighted_iv: spx_next_iv ")
  expect_that(test_spx_next_iv, equals(spx_next_iv))

  cat("\ntest.weighted_iv: spx_vvix ")
  expect_that(test_spx_vvix, equals(spx_vvix))

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

  test_nifty_next <- prep_maturity(maturity=vix_nifty$opt_next$maturity[[1]],
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

  test_nifty_near_iv <- weighted_iv(prepped=test_nifty_near)
  test_nifty_next_iv <- weighted_iv(prepped=test_nifty_next)
 
  test_nifty_vixes <- vix_pt(iv_near=test_nifty_near_iv, 
                        iv_next=test_nifty_next_iv)
  
  cat("\ntest.weighted_iv: nifty_near ")
  expect_that(test_nifty_near, equals(nifty_near))
  
  cat("\ntest.weighted_iv: nifty_next ")
  expect_that(test_nifty_next, equals(nifty_next))

  cat("\ntest.weighted_iv: nifty_near_iv ")
  expect_that(test_nifty_near_iv, equals(nifty_near_iv))
   
  cat("\ntest.weighted_iv: nifty_next_iv ")
  expect_that(test_nifty_next_iv, equals(nifty_next_iv))

  cat("\ntest.weighted_iv: nifty_vixes ")
  expect_that(test_nifty_vixes, equals(nifty_vixes))
 })
