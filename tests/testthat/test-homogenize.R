context("homogenizing functions")


test_that("is.homogeneous()", {
  
  expect_false( is.homogeneous(mp("x^2 + y")) )
  
  expect_true( is.homogeneous(mp("x^2 + y^2")) )
  
  expect_equal( 
    is.homogeneous(mp(c("x + y", "x + y^2"))),
    c(TRUE, FALSE)
  )
  
})




test_that("homogenize()", {
  
  expect_equal(
    homogenize(mp("x^3 + y")),
    mp("x^3 + y t^2")
  )
  
  expect_equal(
    homogenize(mp("x^3 + y"), indeterminate = "z"),
    mp("x^3 + y z^2")
  )
  
  expect_equal(
    homogenize(mp(c("x^3 + y", "x + 1"))),
    mp(c("x^3 + y t^2", "x + t"))
  )
  
  expect_error(
    homogenize("x + x^2"),
    "homogenize requires mpoly objects.",
    fixed = TRUE
  )
  
})



test_that("dehomogenize()", {
  
  expect_equal(
    dehomogenize(mp("x^3 + y t^2"), indeterminate = "t"),
    mp("x^3 + y")
  )
  
  expect_equal(
    dehomogenize(mp(c("x^2 + y t", "x t^2"))),
    mp(c("x^2 + y", "x"))
  )
  
  expect_error(
    dehomogenize(mp("x^3 + y t"), indeterminate = "t"),
    "polynomial x^3  +  y t is not homogeneous.",
    fixed = TRUE
  )
  
  expect_error(
    dehomogenize("x t + x^2"),
    "dehomogenize requires mpoly objects.",
    fixed = TRUE
  )
  
})




test_that("homogeneous_components()", {
  
  expect_equal(
    homogeneous_components(mp("1 + x + x^2 + x y + y")),
    mp(c("1", "x + y", "x^2 + x y"))
  )
  
})
