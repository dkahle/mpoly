context("partitions()")

test_that("basic partitions() functionality works", {
  expect_equal(
    partitions(3), 
    structure(c(3L, 2L, 1L, 0L, 1L, 1L, 0L, 0L, 1L), .Dim = c(3L, 3L))
  )
})


test_that("vectorized", {
  expect_equal(
    partitions(1:3), 
    list(
      structure(1L, .Dim = c(1L, 1L)), 
      structure(c(2L, 1L, 0L, 1L), .Dim = c(2L, 2L)), 
      structure(c(3L, 2L, 1L, 0L, 1L, 1L, 0L, 0L, 1L), .Dim = c(3L, 3L))
    )
  )
})
