context("laguerre()")


test_that("basic", {
  
  expect_equal(
    laguerre(2),
    structure(
      list(
        c("coef" = 1),
        c("x" = 1, "coef" = -2),
        c("x" = 2, "coef" = .5)
      ), 
      "class" = c("laguerre", "mpoly"),
      "laguerre" = list(
        "degree" = 2, 
        "alpha" = 0, 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
})



test_that("indeterminate", {
  
  expect_equal(
    laguerre(2, indeterminate = "t"),
    structure(
      list(
        c("coef" = 1),
        c("t" = 1, "coef" = -2),
        c("t" = 2, "coef" = .5)
      ), 
      "class" = c("laguerre", "mpoly"),
      "laguerre" = list(
        "degree" = 2, 
        "alpha" = 0, 
        "indeterminate" = "t",
        "normalized" = FALSE
      )
    )
  )
  
})




test_that("alpha", {
  
  expect_equal(
    laguerre(2, alpha = 3),
    structure(
      list(
        c("coef" = 10),
        c("x" = 1, "coef" = -5),
        c("x" = 2, "coef" = .5)
      ), 
      "class" = c("laguerre", "mpoly"),
      "laguerre" = list(
        "degree" = 2, 
        "alpha" = 3, 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
})




test_that("normalized", {
  
  expect_equal(
    laguerre(2, alpha = 3, normalized = TRUE),
    structure(
      list(
        c("coef" = 1.290994),
        c("x" = 1, "coef" = -0.6454972),
        c("x" = 2, "coef" = 0.06454972)
      ), 
      "class" = c("laguerre", "mpoly"),
      "laguerre" = list(
        "degree" = 2, 
        "alpha" = 3, 
        "indeterminate" = "x",
        "normalized" = TRUE
      )
    ),
    tolerance = 1e-5
  )
  
})





test_that("vectorized", {
  
  expect_equal(
    laguerre(0:2),
    structure(
      list(
        structure(
          list(
            c("coef" = 1)
          ), 
          "class" = c("laguerre", "mpoly"),
          "laguerre" = list(
            "degree" = 2, 
            "alpha" = 0, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("coef" = 1),
            c("x" = 1, "coef" = -1)
          ), 
          "class" = c("laguerre", "mpoly"),
          "laguerre" = list(
            "degree" = 2, 
            "alpha" = 0, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("coef" = 1),
            c("x" = 1, "coef" = -2),
            c("x" = 2, "coef" = .5)
          ), 
          "class" = c("laguerre", "mpoly"),
          "laguerre" = list(
            "degree" = 2, 
            "alpha" = 0, 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        )
      ),
      "class" = "mpolyList"
    )
  )
  
})