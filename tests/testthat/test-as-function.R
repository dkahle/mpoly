context("as.function.mpoly() works properly")



test_that("as.function.mpoly() basic functionality works", {
  
  f <- as.function(mp("(3 - x)^2"), silent = TRUE)
  
  x <- 2
  expect_equal(f(x), (3 - x)^2)
  
  x <- 5
  expect_equal(f(x), (3 - x)^2)

})



test_that("as.function.mpoly() creates a vectorized function", {
  
  f <- as.function(mp("(3 - x)^2"), silent = TRUE)
  
  x <- 2:10
  expect_equal(f(x), (3 - x)^2)
  
})

