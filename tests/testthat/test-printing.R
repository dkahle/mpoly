context("Printing works properly")



test_that("mpoly_term's print properly", {
  
  # integer coefficients
  term <- structure(
    list(
      coef = 2L,
      core = c("x" = 1L, "y" = 2L, "z" = 1L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "2 x y\\^2 z")
  expect_output(print(term, stars = TRUE), "2\\*x\\*y\\^2\\*z")
  expect_silent(print(term, silent = TRUE))
  
  
  
  # numeric coefficients
  term <- structure(
    list(
      coef = 3,
      core = c("x" = 1L, "y" = 2L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "3 x y\\^2")
  expect_output(print(term, stars = TRUE), "3\\*x\\*y\\^2")
  expect_silent(print(term, silent = TRUE))
  
  
  
  # complex coefficients
  term <- structure(
    list(
      coef = complex(real = 1, imaginary = 1),
      core = c("x" = 1L, "y" = 2L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "\\(1\\+1i\\) x y\\^2")
  expect_output(print(term, stars = TRUE), "\\(1\\+1i\\)\\*x\\*y\\^2")
  expect_silent(print(term, silent = TRUE))
  
  term <- structure(
    list(
      coef = complex(real = -1, imaginary = 0),
      core = c("x" = 1L, "y" = 2L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "-1 x y\\^2")
  expect_output(print(term, stars = TRUE), "-1\\*x\\*y\\^2")
  
  term <- structure(
    list(
      coef = complex(real = 0, imaginary = -2),
      core = c("x" = 1L, "y" = 2L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "-2i x y\\^2")
  expect_output(print(term, stars = TRUE), "-2i\\*x\\*y\\^2")
  
  
  
   
  
  # coef = 1
  term <- structure(
    list(
      coef = complex(real = 1, imaginary = 0),
      core = c("x" = 1L, "y" = 2L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "x y\\^2")
  expect_output(print(term, stars = TRUE), "x\\*y\\^2")
  expect_silent(print(term, silent = TRUE))
  
  
  
  # constant term
  term <- structure(
    list(
      coef = complex(real = 1, imaginary = 0),
      core = integer(0)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "1\\+0i")
  expect_output(print(term, stars = TRUE), "1\\+0i")
  expect_silent(print(term, silent = TRUE))
  
  term <- structure(
    list(
      coef = -5,
      core = integer(0)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "-5")
  expect_output(print(term, stars = TRUE), "-5")
  expect_silent(print(term, silent = TRUE))
  
  
  
})








test_that("bare_mpoly's print properly", {
  
  # integer coefficients
  term <- structure(
    list(
      coef = 2L,
      core = c("x" = 1L, "y" = 2L, "z" = 1L)
    ),
    class = "mpoly_term"
  )
  
  expect_output(print(term), "2 x y\\^2 z")
  expect_output(print(term, stars = TRUE), "2\\*x\\*y\\^2\\*z")
  expect_silent(print(term, silent = TRUE))
  
  
  
})
