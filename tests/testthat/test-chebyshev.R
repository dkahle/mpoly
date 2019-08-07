context("chebyshev()")


test_that("basic", {
  
  expect_equal(
    chebyshev(3),
    structure(
      list(
        c("x" = 1, "coef" = -3),
        c("x" = 3, "coef" = 4)
      ), 
      "class" = c("chebyshev", "mpoly"),
      "chebyshev" = list(
        "degree" = 0, 
        "kind" = "t", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
  
  expect_equal(
    chebyshev_roots(1:3, 3),
    cos((1:3 - .5) * pi / 3)
  )
  
})







test_that("different kinds", {
  
  expect_equal(
    chebyshev(3, kind = "u"),
    structure(
      list(
        c("x" = 1, "coef" = -4),
        c("x" = 3, "coef" = 8)
      ), 
      "class" = c("chebyshev", "mpoly"),
      "chebyshev" = list(
        "degree" = 0, 
        "kind" = "u", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
  
  expect_equal(
    chebyshev(3, kind = "c"),
    structure(
      list(
        c("x" = 1, "coef" = -1.5),
        c("x" = 3, "coef" = .5)
      ), 
      "class" = c("chebyshev", "mpoly"),
      "chebyshev" = list(
        "degree" = 0, 
        "kind" = "c", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
  
  expect_equal(
    chebyshev(3, kind = "s"),
    structure(
      list(
        c("x" = 1, "coef" = -2),
        c("x" = 3, "coef" = 1)
      ), 
      "class" = c("chebyshev", "mpoly"),
      "chebyshev" = list(
        "degree" = 0, 
        "kind" = "s", 
        "indeterminate" = "x",
        "normalized" = FALSE
      )
    )
  )
  
})






test_that("normalized", {
  
  expect_equal(
    chebyshev(3, normalized = TRUE),
    structure(
      list(
        c("x" = 1, "coef" = -2.393654),
        c("x" = 3, "coef" = 3.191538)
      ), 
      "class" = c("chebyshev", "mpoly"),
      "chebyshev" = list(
        "degree" = 0, 
        "kind" = "t", 
        "indeterminate" = "x",
        "normalized" = TRUE
      )
    ),
    tolerance = 1e-5
  )
  
})






test_that("vectorized", {
  
  expect_equal(
    chebyshev(0:2),
    structure(
      list(
        structure(
          list(
            c("coef" = 1)
          ), 
          "class" = c("chebyshev", "mpoly"),
          "chebyshev" = list(
            "degree" = 0, 
            "kind" = "t", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("x" = 1, "coef" = 1)
          ), 
          "class" = c("chebyshev", "mpoly"),
          "chebyshev" = list(
            "degree" = 1, 
            "kind" = "t", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        ),
        structure(
          list(
            c("coef" = -1),
            c("x" = 2, "coef" = 2)
          ), 
          "class" = c("chebyshev", "mpoly"),
          "chebyshev" = list(
            "degree" = 2, 
            "kind" = "t", 
            "indeterminate" = "x",
            "normalized" = FALSE
          )
        )
      ),
      "class" = "mpolyList"
    )
  )
  
})

