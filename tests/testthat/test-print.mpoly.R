context("print_term() works properly")



test_that("print_term() prints constants properly", {
  
  expect_equal(print_term(c("coef" = 5)), "5")
  expect_equal(print_term(c("coef" = -5)), "-5")
  expect_equal(print_term(c("coef" = 1)), "1")
  expect_equal(print_term(c("coef" = -1)), "-1")
  expect_equal(print_term(c("coef" = 0)), "0")
  
  expect_equal(print_term(c("coef" = .1)), "0.1")
  expect_equal(print_term(c("coef" = -.1)), "-0.1")
  
  expect_equal(print_term(c("coef" = 0.1)), "0.1")
  expect_equal(print_term(c("coef" = -0.1)), "-0.1")
  
  expect_equal(print_term(c("coef" = 1.2)), "1.2")
  expect_equal(print_term(c("coef" = -1.2)), "-1.2")
  
  # scientific notation
  expect_equal(print_term(c("coef" = 1e2)), "100")
  expect_equal(print_term(c("coef" = -1e2)), "-100")
  
  expect_equal(print_term(c("coef" = 1e-2)), "0.01")
  expect_equal(print_term(c("coef" = -1e-2)), "-0.01")
  
  # longer digit examples - rounding to 7 digits
  expect_equal(print_term(c("coef" = 12.3456789)), "12.34568")
  expect_equal(print_term(c("coef" = -12.3456789)), "-12.34568")
  
})





test_that("print_term() prints univariate terms properly", {
  
  expect_equal(print_term(c("x" = 2L, "coef" =  5)), "5 x^2")
  expect_equal(print_term(c("x" = 2L, "coef" =  1)), "x^2")
  expect_equal(print_term(c("x" = 2L, "coef" = -5)), "-5 x^2")
  expect_equal(print_term(c("x" = 2L, "coef" = -1)), "-1 x^2")
  
  expect_equal(print_term(c("x" = 1L, "coef" =  5)),  "5 x")
  expect_equal(print_term(c("x" = 1L, "coef" =  1)),    "x")
  expect_equal(print_term(c("x" = 1L, "coef" = -5)), "-5 x")
  expect_equal(print_term(c("x" = 1L, "coef" = -1)), "-1 x")   
  
  
  expect_equal(print_term(c("x" = 2L, "coef" =  5), times = " * ", expo = "**"),  "5 * x**2")
  expect_equal(print_term(c("x" = 2L, "coef" =  1), times = " * ", expo = "**"),      "x**2")
  expect_equal(print_term(c("x" = 2L, "coef" = -5), times = " * ", expo = "**"), "-5 * x**2")
  expect_equal(print_term(c("x" = 2L, "coef" = -1), times = " * ", expo = "**"), "-1 * x**2")
  
  expect_equal(print_term(c("x" = 1L, "coef" =  5)),  "5 x")
  expect_equal(print_term(c("x" = 1L, "coef" =  1)),    "x")
  expect_equal(print_term(c("x" = 1L, "coef" = -5)), "-5 x")
  expect_equal(print_term(c("x" = 1L, "coef" = -1)), "-1 x")   
  
  
  # sci notation
  expect_equal(print_term(c("x" = 2L, "coef" =   1e2)),   "100 x^2")
  expect_equal(print_term(c("x" = 2L, "coef" =  -1e2)),  "-100 x^2")
  expect_equal(print_term(c("x" = 2L, "coef" =  1e-2)),  "0.01 x^2")
  expect_equal(print_term(c("x" = 2L, "coef" = -1e-2)), "-0.01 x^2")  
  
  expect_equal(print_term(c("x" = 2L, "coef" =   1e2), times = "*", expo = "**"),   "100*x**2")
  expect_equal(print_term(c("x" = 2L, "coef" =  -1e2), times = "*", expo = "**"),  "-100*x**2")
  expect_equal(print_term(c("x" = 2L, "coef" =  1e-2), times = "*", expo = "**"),  "0.01*x**2")
  expect_equal(print_term(c("x" = 2L, "coef" = -1e-2), times = "*", expo = "**"), "-0.01*x**2")  
  
})




test_that("print_term() prints multivariate terms properly", {
  
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" =  5)),  "5 x^2 y^3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" = -5)), "-5 x^2 y^3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" =  1)),    "x^2 y^3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" = -1)), "-1 x^2 y^3")
  
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" =  5)),  "5 x y^3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" = -5)), "-5 x y^3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" =  1)),    "x y^3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" = -1)), "-1 x y^3")
  
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" =  5)),  "5 x^2 y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" = -5)), "-5 x^2 y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" =  1)),    "x^2 y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" = -1)), "-1 x^2 y")
  
  
  
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" =  5), times = " * ", expo = "**"),  "5 * x**2 * y**3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" = -5), times = " * ", expo = "**"), "-5 * x**2 * y**3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" =  1), times = " * ", expo = "**"),      "x**2 * y**3")
  expect_equal(print_term(c("x" = 2L, "y" = 3L, "coef" = -1), times = " * ", expo = "**"), "-1 * x**2 * y**3")
  
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" =  5), times = " * ", expo = "**"),  "5 * x * y**3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" = -5), times = " * ", expo = "**"), "-5 * x * y**3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" =  1), times = " * ", expo = "**"),      "x * y**3")
  expect_equal(print_term(c("x" = 1L, "y" = 3L, "coef" = -1), times = " * ", expo = "**"), "-1 * x * y**3")
  
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" =  5), times = " * ", expo = "**"),  "5 * x**2 * y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" = -5), times = " * ", expo = "**"), "-5 * x**2 * y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" =  1), times = " * ", expo = "**"),      "x**2 * y")
  expect_equal(print_term(c("x" = 2L, "y" = 1L, "coef" = -1), times = " * ", expo = "**"), "-1 * x**2 * y")
  
})









