context("jacobi()")


test_that("basic", {
  
  expect_equal(
    jacobi(3),
    structure(
      list(
        c("x" = 1, "coef" = -7.5),
        c("x" = 3, "coef" = 17.5)
      ), 
      "class" = c("jacobi", "mpoly"),
      "jacobi" = list(
        "degree" = 0, 
        "kind" = "p", 
        "indeterminate" = "x",
        "normalized" = FALSE,
        "alpha" = 1,
        "beta" = 1
      )
    )
  )
    
})




test_that("normalized", {
  
  expect_equal(
    jacobi(3, normalized = TRUE),
    structure(
      list(
        c("x" = 1, "coef" = -3.557562),
        c("x" = 3, "coef" = 8.300979)
      ), 
      "class" = c("jacobi", "mpoly"),
      "jacobi" = list(
        "degree" = 0, 
        "kind" = "p", 
        "indeterminate" = "x",
        "normalized" = TRUE,
        "alpha" = 1,
        "beta" = 1
      )
    ),
    tolerance = 1e-5
  )
  
})



test_that("alpha and beta", {
  
  expect_equal(
    jacobi(2, alpha = 2, beta = 3),
    structure(
      list(
        c("coef" = -1),
        c("x" = 1, "coef" = -2),
        c("x" = 2, "coef" = 9)
      ), 
      "class" = c("jacobi", "mpoly"),
      "jacobi" = list(
        "degree" = 0, 
        "kind" = "p", 
        "indeterminate" = "x",
        "normalized" = FALSE,
        "alpha" = 2,
        "beta" = 3
      )
    )
  )
  
})



test_that("kind = g", {
  
  expect_equal(
    jacobi(2, kind = "g"),
    structure(
      list(
        c("coef" = 1/6),
        c("x" = 1, "coef" = -1),
        c("x" = 2, "coef" = 1)
      ), 
      "class" = c("jacobi", "mpoly"),
      "jacobi" = list(
        "degree" = 0, 
        "kind" = "g", 
        "indeterminate" = "x",
        "normalized" = FALSE,
        "alpha" = 1,
        "beta" = 1
      )
    )
  )
  
})



test_that("degree is vectorized", {
  
  expect_equal(
    jacobi(0:1),
    structure(
      list(
        structure(
          list(
            c("coef" = 1)
          ), 
          "class" = c("jacobi", "mpoly"),
          "jacobi" = list(
            "degree" = 0, 
            "kind" = "p", 
            "indeterminate" = "x",
            "normalized" = FALSE,
            "alpha" = 1,
            "beta" = 1
          )
        ),
        structure(
          list(
            c("x" = 1, "coef" = 3)
          ), 
          "class" = c("jacobi", "mpoly"),
          "jacobi" = list(
            "degree" = 1, 
            "kind" = "p", 
            "indeterminate" = "x",
            "normalized" = FALSE,
            "alpha" = 1,
            "beta" = 1
          )
        )
      ),
      class = "mpolyList"
    )
  )
  
})

