context("hermite()")


test_that("basic", {
  
  expect_equal(
    hermite(3),
    structure(
      list(
        c("x" = 1, "coef" = -3),
        c("x" = 3, "coef" = 1)
      ), 
      "class" = c("hermite", "mpoly"),
      "hermite" = list(
        "degree" = 0, 
        "kind" = "he", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
})



test_that("normalized", {
  
  expect_equal(
    hermite(3, normalized = TRUE),
    structure(
      list(
        c("x" = 1, "coef" = -0.7735719),
        c("x" = 3, "coef" = 0.2578573)
      ), 
      "class" = c("hermite", "mpoly"),
      "hermite" = list(
        "degree" = 0, 
        "kind" = "he", 
        "indeterminate" = "x",
        "normalized" = TRUE
      )
    ),
    tolerance = 1e-5
  )
  
})








test_that("indeterminate", {
  
  expect_equal(
    hermite(3, indeterminate = "t"),
    structure(
      list(
        c("t" = 1, "coef" = -3),
        c("t" = 3, "coef" = 1)
      ), 
      "class" = c("hermite", "mpoly"),
      "hermite" = list(
        "degree" = 0, 
        "kind" = "he", 
        "indeterminate" = "t",
        "normalized" = FALSE
      )
    ),
    tolerance = 1e-5
  )
  
})





test_that("kind = h", {
  
  expect_equal(
    hermite(3, kind = "h"),
    structure(
      list(
        c("x" = 1, "coef" = -12),
        c("x" = 3, "coef" = 8)
      ), 
      "class" = c("hermite", "mpoly"),
      "hermite" = list(
        "degree" = 0, 
        "kind" = "h", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    ),
    tolerance = 1e-5
  )
  
})




test_that("vectorized", {
  
  expect_equal(
    hermite(0:2),
    structure(
      list(
        structure(
          list(
            c("coef" = 1)
          ), 
          "class" = c("hermite", "mpoly"),
          "hermite" = list(
            "degree" = 0, 
            "kind" = "he", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("x" = 1, "coef" = 1)
          ), 
          "class" = c("hermite", "mpoly"),
          "hermite" = list(
            "degree" = 1, 
            "kind" = "he", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("coef" = -1),
            c("x" = 2, "coef" = 1)
          ), 
          "class" = c("hermite", "mpoly"),
          "hermite" = list(
            "degree" = 2, 
            "kind" = "he", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        )
      ),
      class = "mpolyList"
    )
  )
  
})
