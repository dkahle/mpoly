context("mpolyList()")

test_that("basic", {
  
  expect_equal(
    mpolyList(mp("x"), mp("x^2"), mp("x^3 t")),
    structure(
      list(
        structure(list(c(x = 1, coef = 1)), class = "mpoly"), 
        structure(list(c(x = 2, coef = 1)), class = "mpoly"), 
        structure(list(c(x = 3, t = 1, coef = 1)), class = "mpoly")
      ), 
      class = "mpolyList"
    )
  )
  
  expect_error(
    mpolyList(
      mp("x"),
      "y"
    ),
    "Each argument must be of class mpoly.",
    fixed = TRUE
  )
  
  
})
