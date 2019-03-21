context("tuples()")


test_that("basic tuples() functionality works", {
  
  expect_equal(
    tuples(1:2, 3),
    structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 
      1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L), .Dim = c(8L, 3L))
  )
  
})




test_that("tuples() list = TRUEs properly", {
  
  expect_equal(
    tuples(1:2, 3, list = TRUE),
    list(
      c(1L, 1L, 1L), c(1L, 1L, 2L), c(1L, 2L, 1L), c(1L, 2L, 2L), 
      c(2L, 1L, 1L), c(2L, 1L, 2L), c(2L, 2L, 1L), c(2L, 2L, 2L)
    )
  )
  
})




test_that("tuples() repeats argument works properly", {
  
  expect_equal(
    tuples(c(1, 2, 2), 2, repeats = TRUE),
    structure(c(1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 2), .Dim = c(9L, 2L))
  )
  
  expect_equal(
    tuples(c(1, 2, 2), 2, repeats = FALSE),
    structure(c(1, 1, 2, 2, 1, 2, 1, 2), .Dim = c(4L, 2L))
  )
  
})


