context("as.mpoly()")

test_that("numeric", {
  
  expect_equal(
    as.mpoly(3),
    structure(list(c("coef" = 3)), class = "mpoly")
  )
  
  expect_equal(
    as.mpoly(1:3),
    structure(
      list(
        c("coef" = 1),
        c("x" = 1, "coef" = 2),
        c("x" = 2, "coef" = 3)
      ), 
      class = "mpoly"
    )
  )
  
})



test_that("polynomial", {
  
  expect_equal(
    as.mpoly(hermite.h.polynomials(3)[[4]]),
    structure(
      list(
        c("x" = 1, "coef" = -12),
        c("x" = 3, "coef" = 8)
      ), 
      class = "mpoly"
    )
  )
  
})




test_that("lm", {
  
  expect_equal(
    as.mpoly(lm(y ~ x, data = data.frame("x" = c(0,1), "y" = c(1,3)))),
    structure(
      list(
        c("x" = 1, "coef" = 2),
        c("coef" = 1)
      ), 
      class = "mpoly"
    )
  )
  
  
  expect_equal(
    as.mpoly(
      lm(
        y ~ poly(x, 1, raw = TRUE), 
        data = data.frame("x" = c(0,1), "y" = c(1,3))
      )
    ),
    structure(
      list(
        c("x" = 1, "coef" = 2),
        c("coef" = 1)
      ), 
      class = "mpoly"
    )
  )
  
  
  expect_equal(
    as.mpoly(
      lm(
        y ~ poly(x, 2, raw = TRUE), 
        data = data.frame("x" = c(-1,0,1), "y" = c(0,-1,0))
      )
    ),
    structure(
      list(
        c("x" = 1, "coef" = 0),
        c("x" = 2, "coef" = 1),
        c("coef" = -1)
      ), 
      class = "mpoly"
    )
  )
  
  
  expect_equal(
    as.mpoly(
      lm(
        y ~ x + I(x^2), 
        data = data.frame("x" = c(-1,0,1), "y" = c(0,-1,0))
      )
    ),
    structure(
      list(
        c("x" = 1, "coef" = 0),
        c("x" = 2, "coef" = 1),
        c("coef" = -1)
      ), 
      class = "mpoly"
    )
  )
  
  
  expect_error(
    as.mpoly(
      lm(
        y ~ poly(x, 2, raw = FALSE), 
        data = data.frame("x" = c(-1,0,1), "y" = c(0,-1,0))
      )
    ),
    "poly() statements currently must contain raw = TRUE.",
    fixed = TRUE
  )
  
})




test_that("errors", {
  
  expect_error(
    as.mpoly("x"),
    "objects of class character not supported by as.mpoly().",
    fixed = TRUE
  )
  
})