context("as.function.mpolyList()")

test_that("basic", {
  
  expect_equal(
    as.function(mp(c("2 x + 1", "x - z^2")), silent = TRUE)(c(1,2)),
    c(3L, -3L)
  )
  
})


test_that("vector", {
  
  expect_equal(
    as.function(mp(c("2 x + 1", "x - z^2")), silent = TRUE, vector = FALSE)(1, 2),
    c(3L, -3L)
  )
  
})


test_that("univariate", {
  
  expect_equal(
    as.function(mp(c("x", "x^2", "x^3")), silent = TRUE)(2),
    c(2L, 4L, 8L)
  )
  
  
  expect_equal(
    as.function(mp(c("x", "x^2", "x^3")), silent = TRUE)(1:3),
    structure(c(1, 2, 3, 1, 4, 9, 1, 8, 27), .Dim = c(3L, 3L))
  )
  
})


test_that("errors when should", {
  
  expect_error(
    as.function(mp(c("x", "y", "z")), silent = TRUE, varorder = c("x", "y")),
    "varorder must contain all of the variables."
  )
  
})