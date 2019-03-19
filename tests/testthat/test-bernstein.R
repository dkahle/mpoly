context("bernstein()")


#' bernstein(0, 0)
#'
#' bernstein(0, 1)
#' bernstein(1, 1)

test_that("bernstein() works", {
  
  expect_equal(
    bernstein(0, 0),
    structure(
      list(c("coef" = 1)), 
      class = c("bernstein", "mpoly"),
      bernstein = list("k" = 0, "n" = 0, "indeterminate" = "x")
    )
  )
  
  
  expect_equal(
    bernstein(0, 1),
    structure(
      list(c("coef" = 1), c("x" = 1, "coef" = -1)), 
      class = c("bernstein", "mpoly"),
      bernstein = list("k" = 0, "n" = 1, "indeterminate" = "x")
    )
  )
  
  
  expect_equal(
    bernstein(1, 1),
    structure(
      list(c("x" = 1, "coef" = 1)), 
      class = c("bernstein", "mpoly"),
      bernstein = list("k" = 1, "n" = 1, "indeterminate" = "x")
    )
  )
  
  
})






test_that("bernstein() chooses variables correctly", {
  
  
  expect_equal(
    bernstein(0, 1, "t"),
    structure(
      list(c("coef" = 1), c("t" = 1, "coef" = -1)), 
      class = c("bernstein", "mpoly"),
      bernstein = list("k" = 0, "n" = 1, "indeterminate" = "t")
    )
  )
  
  
})

