test_that("decentLHS", {
  n <- 50
  d <- 3
  expect_error(x <- decentLHS(n, d))
  expect_error(x <- decentLHS(n, d, ndes=3), NA)
  expect_equal(dim(x), c(50,3))
  expect_error(x <- decentLHS(n, d, ndes=3, max.time=1), NA)
  expect_equal(dim(x), c(50,3))
  expect_error(x <- decentLHS(n, d, max.time=.1), NA)
  expect_equal(dim(x), c(50,3))
})
