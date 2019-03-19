context("swap()")


test_that("swap() works for univariate polynomials", {
  
  expect_equal(
    swap(mp("(x + y)^2"), "x", "t"),
    mp("(t + y)^2")
  )
  
  expect_equal(
    swap(mp("(x + y)^2"), "x", "t_1"),
    mp("(t_1 + y)^2")
  )
  
})



test_that("swap() works for bernstein polynomials", {
  
  expect_equal(
    swap(bernstein(3, 5), "x", "t"),
    bernstein(3, 5, indeterminate = "t")
  )
  
})



test_that("swap() works for chebyshev polynomials", {
  
  expect_equal(
    swap(chebyshev(4), "x", "t"),
    chebyshev(4, indeterminate = "t")
  )
  
})


test_that("swap() correctly errors", {
  
  expect_error(swap(mp("x^2 + 1"), "x", "x^2"))
  
  expect_error(swap(mp("x^2 + 1"), "x", "x 2"))
  
  expect_error(swap(mp("x + y"), "x", "y"))
  
})


