context("burst()")


test_that("basic burst works", {
  
  expect_equal(
    burst(2),
    structure(c(0, 2, 1, 2, 0, 1), .Dim = 3:2)
  )
  
})


test_that("bursts of a particular length are computed correctly", {
  
  expect_equal(
    burst(3, 2),
    structure(c(0, 3, 1, 2, 3, 0, 2, 1), .Dim = c(4L, 2L))
  )
  
})


test_that("bursts of length one is itself", {
  
  expect_equal(burst(3, 1), 3L)
  
})
