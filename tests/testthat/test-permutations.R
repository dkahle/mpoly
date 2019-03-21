context("permutations()")

test_that("permutations of a single integer works", {
  
  expect_equal(
    permutations(3L),
    structure(c(1, 1, 2, 2, 3, 3, 2, 3, 1, 3, 1, 2, 3, 2, 3, 1, 2, 1), .Dim = c(6L, 3L))
  )
  
})



test_that("permutations of a vector works", {
  
  expect_equal(
    permutations(letters[1:3]),
    structure(
      c("a", "a", "b", "b", "c", "c", "b", "c", "a", "c", 
      "a", "b", "c", "b", "c", "a", "b", "a"), 
      .Dim = c(6L, 3L)
    )
  )
  
})



test_that("permutations with duplicated entries works", {
  
  expect_equal(
    permutations(c(1, 1, 2)),
    structure(c(1, 1, 2, 1, 2, 1, 2, 1, 1), .Dim = c(3L, 3L))
  )
  
})
