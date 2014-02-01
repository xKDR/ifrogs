context("VIX")

test_that("test.vix_ci", {
  load(system.file("data", "vix_spx.RData", package = "ifrogs.dev"))
  load(system.file("data", "vix_nifty.RData", package = "ifrogs.dev"))

                                        # load the test output data
  load(system.file("tests", "data_vix_ci.RData", package = "ifrogs.dev"))
  set.seed(101)
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

  test_spx_ci <- vix_ci(prep_near=test_spx_near,
                   prep_next=test_spx_next,
                   n_samples=1e3, conf=0.95,
                   verbose=TRUE)

  cat("\ntest.vix_ci: spx_near ")
  expect_that(test_spx_near, equals(spx_near))
  
  cat("\ntest.vix_ci: spx_next ")
  expect_that(test_spx_next, equals(spx_next))

  cat("\ntest.vix_ci: spx_ci ")
  expect_that(test_spx_ci, equals(spx_ci))

  ##
  set.seed(101)
  test_nifty_near <- prep_maturity(maturity=vix_nifty$opt_near$maturity[[1]],
                                   riskfree=vix_nifty$opt_near$riskfree[[1]],
                                   carry=vix_nifty$opt_near$riskfree[[1]],
                                   type=vix_nifty$opt_near$type,
                                   strike=vix_nifty$opt_near$strike,
                                   underlying=vix_nifty$opt_near$underlying,
                                   schemes="vega",
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
                                   schemes="vega",
                                   bid=vix_nifty$opt_next$bid,
                                   ask=vix_nifty$opt_next$ask,
                                   traded_vol=vix_nifty$opt_next$traded_vol,
                                   tv_filter=TRUE)

  test_nifty_ci <- vix_ci(prep_near=test_nifty_near,
                          prep_next=test_nifty_next,
                          n_samples=1e3, conf=0.95,
                          verbose=TRUE)

  cat("\ntest.vix_ci: nifty_near ")
  expect_that(test_nifty_near, equals(nifty_near))
  
  cat("\ntest.vix_ci: nifty_next ")
  expect_that(test_nifty_next, equals(nifty_next))

  cat("\ntest.vix_ci: nifty_ci ")
  expect_that(test_nifty_ci, equals(nifty_ci))
})
