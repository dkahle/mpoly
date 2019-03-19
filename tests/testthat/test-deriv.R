context("deriv()")


test_that("basic", {
  
  expect_equal(
    deriv(mp("x y + y z + z^2"), "x"),
    mp("y")
  )
  
  expect_equal(
    deriv(mp("x y + y z + z^2"), "y"),
    mp("x + z")
  )
  
  expect_equal(
    deriv(mp("x y + y z + z^2"), "z"),
    mp("y + 2 z")
  )
  
  expect_equal(
    deriv(mp("x y + y z + z^2"), "t"),
    mp("0")
  )
  
  expect_error(
    deriv(mp("x y + y z + z^2")),
    "var must be specified, see ?deriv.mpoly",
    fixed = TRUE
  )
  
})





test_that("gradient", {
  
  expect_equal(
    gradient(mp("x y + y z + z^2")),
    mp(c("y", "x + z", "y + 2 z"))
  )
  
  
})