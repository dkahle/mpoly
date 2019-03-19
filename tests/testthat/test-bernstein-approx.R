context("bernstein_approx()")


test_that("bernstein_approx() works", {
  
  expect_equal(
    bernstein_approx(function(x) x^4, 2L),
    mp(".125 x + .875 x^2")
  )
  
  expect_equal(
    suppressWarnings(bernsteinApprox(function(x) x^4, 2L)),
    mp(".125 x + .875 x^2")
  )
  
})


