context("predicate functions")


test_that("is.constant() works - single", {
  expect_equal(is.constant(mp("5")), TRUE)
  expect_equal(is.constant(mp("5 + x")), FALSE)
})

test_that("is.constant() works - vector", {
  expect_equal(is.constant(mp(c("5", "5 + x"))), c(TRUE, FALSE))
})


test_that("is.mpoly() works", {
  expect_equal(is.mpoly(mp("5")), TRUE)
  expect_equal(is.mpoly(mp("5 + x")), TRUE)
  expect_equal(is.mpoly(mp(c("5", "5 + x"))), FALSE)
})


test_that("is.unipoly() works", {
  expect_equal(is.unipoly(mp("1")), TRUE)
  expect_equal(is.unipoly(mp("x+1")), TRUE)
  expect_equal(is.unipoly(mp("x+1+x^2")), TRUE)
  expect_equal(is.unipoly(mp(c("x+1+x^2", "y"))), c(TRUE, TRUE))
  expect_equal(is.unipoly(mp(c("x+1+x^2", "y", "x+y"))), c(TRUE, TRUE, FALSE))
})



test_that("is.bernstein() works", {
  expect_equal(is.bernstein(bernstein(0, 0)), TRUE)
  expect_equal(is.bernstein(1), FALSE)
  expect_equal(is.bernstein(mp("1")), FALSE)
})



test_that("is.bezier() works", {
  expect_equal(is.bezier(bezier(c(0, 0), c(1, 1), c(2, 0))), TRUE)
  expect_equal(is.bezier(mp(c("2 t", "-2 t^2 + 2 t"))), FALSE)
})



test_that("is.chebyshev() works", {
  expect_equal(is.chebyshev(chebyshev(1)), TRUE)
  expect_equal(is.chebyshev(mp("x")), FALSE)
})



test_that("is.mpolyList() works", {
  expect_equal(is.mpolyList(mp(c("x", "y"))), TRUE)
  
  expect_equal(is.mpolyList(   c("x", "y") ), FALSE)
  expect_equal(is.mpolyList(mp("x")), FALSE)
})




test_that("is.linear() works", {
  
  expect_equal(is.linear(mp("x + z")), TRUE)
  expect_equal(is.linear(mp(c("x + z", "y"))), c(TRUE, TRUE))
  
  expect_equal(is.linear(mp("x y")), FALSE)
  expect_equal(is.linear(mp("x^2")), FALSE)
  expect_equal(is.linear(mp(c("x + z", "y", "x^2"))), c(TRUE, TRUE, FALSE))

})



test_that("is.wholenumber() works", {
  
  expect_equal(is.wholenumber(1L), TRUE)
  expect_equal(is.wholenumber(1), TRUE)
  expect_equal(is.wholenumber(1 + 1e-5), FALSE)
  expect_equal(is.wholenumber(1 + 1e-10), TRUE)
  
})





