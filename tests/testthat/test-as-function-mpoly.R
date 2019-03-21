context("as.function.mpoly()")



test_that("univariate", {
  
  f <- as.function(mp("(3 - x)^2"), silent = TRUE)
  
  x <- 2
  expect_equal(f(x), (3 - x)^2)
  
  x <- 5
  expect_equal(f(x), (3 - x)^2)
  
  f <- as.function(mp("(3 - x)^2"), silent = TRUE)
  
  x <- 2:10
  expect_equal(f(x), (3 - x)^2)

})



test_that("vector = TRUE", {
  
  f <- as.function(mp("3 x + y^2"), vector = TRUE, silent = TRUE)
  
  expect_equal(f(c(1,2)), 3*(1) + (2)^2)
  
})


test_that("vector = FALSE", {
  
  f <- as.function(mp("3 x + y^2"), vector = FALSE, silent = TRUE)
  
  expect_equal(f(1, 2), 3*(1) + (2)^2)
  
})


test_that("constant mpoly", {
  
  f <- as.function(mp("3"), silent = TRUE)
  
  expect_equal(f(1), 3)
  expect_equal(f(1:4), rep(3, 4))
  
})


test_that("bernstein", {
  
  s <- seq(0, 1, .01)
  
  expect_equal(
    as.function(bernstein(1, 2))(s),
    2*s - 2*s^2
  )
  
  s <- seq(0, 1, .01)
  
  expect_equal(
    as.function(bernstein(1, 2))(-3:3),
    2*(-3:3) - 2*(-3:3)^2
  )
  
})

