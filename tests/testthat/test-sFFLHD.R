context("basic sFFLHD test")

test_that("Is LHD", {
  s <- sFFLHD$new(D=2,L=3)
  s$get.batches(3)
  Xint <- floor(9 * s$Xb)
  is_lhd <- apply(Xint, 2, function(colm) {all(sort(colm) == 0:8)})
  expect_true(all(is_lhd))
})
