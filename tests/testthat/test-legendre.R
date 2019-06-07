context("legendre()")


test_that("basic", {
  
  expect_equal(
    legendre(3),
    structure(
      list(
        c("x" = 1, "coef" = -1.5),
        c("x" = 3, "coef" = 2.5)
      ), 
      "class" = c("legendre", "mpoly"),
      "legendre" = list(
        "degree" = 3, 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
})






test_that("basic", {
  
  expect_equal(
    legendre(3, indeterminate = "t"),
    structure(
      list(
        c("t" = 1, "coef" = -1.5),
        c("t" = 3, "coef" = 2.5)
      ), 
      "class" = c("legendre", "mpoly"),
      "legendre" = list(
        "degree" = 3, 
        "indeterminate" = "t",
        "normalized" = FALSE
      )
    )
  )
  
})







test_that("normalized", {
  
  expect_equal(
    legendre(3, normalized = TRUE),
    structure(
      list(
        c("x" = 1, "coef" = -2.806243),
        c("x" = 3, "coef" = 4.677072)
      ), 
      "class" = c("legendre", "mpoly"),
      "legendre" = list(
        "degree" = 3, 
        "indeterminate" = "x",
        "normalized" = TRUE
      )
    ),
    tolerance = 1e-5
  )
  
})





test_that("vectorized", {
  
  expect_equal(
    legendre(0:2),
    structure(
      list(
        structure(
          list(
            c("coef" = 1)
          ), 
          "class" = c("legendre", "mpoly"),
          "legendre" = list(
            "degree" = 0, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("x" = 1, "coef" = 1)
          ), 
          "class" = c("legendre", "mpoly"),
          "legendre" = list(
            "degree" = 1, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("coef" = -.5),
            c("x" = 2, "coef" = 1.5)
          ), 
          "class" = c("legendre", "mpoly"),
          "legendre" = list(
            "degree" = 2, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        )
      ),
      class = "mpolyList"
    )
  )
  
})

