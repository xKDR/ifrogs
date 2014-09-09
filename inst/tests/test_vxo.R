context("VIX")

test_that("test.vxo", {
  load(system.file("data", "vxo_spx.RData", package = "ifrogs"))
  load(system.file("data", "vxo_nifty.RData", package = "ifrogs"))

                                        # load the test output data
  load(system.file("tests", "data_vxo.RData", package = "ifrogs"))
  
  test_spx_vxo <- vxo(maturity=vxo_spx$maturity, riskfree=vxo_spx$riskfree,
                      carry=vxo_spx$riskfree, type=vxo_spx$type,
                      strike=vxo_spx$strike, underlying=vxo_spx$underlying,
                      bid=vxo_spx$bid, ask=vxo_spx$ask)

  cat("\ntest.vxo: spx_vxo ")
  expect_that(test_spx_vxo, equals(spx_vxo))

  ## 
  test_nifty_vxo <- vxo(maturity=vxo_nifty$maturity,
                         riskfree=vxo_nifty$riskfree, carry=vxo_nifty$riskfree,
                         type=vxo_nifty$type, strike=vxo_nifty$strike,
                         underlying=vxo_nifty$underlying, bid=vxo_nifty$bid,
                         ask=vxo_nifty$ask)

  cat("\ntest.vxo: nifty_vxo ")
  expect_that(test_nifty_vxo, equals(nifty_vxo))
})
