context("print.mpolyList()")


test_that("basic print.mpolyList() functionality", {
  
  expect_equal(
    print(mp(c("x + 1", "y^2 - 2")), silent = TRUE),
    c("x  +  1", "y^2  -  2")
  )
  
})




test_that("plus_pad works", {
  
  expect_equal(
    print(mp(c("x + 1", "y^2 - 2")), silent = TRUE, plus_pad = 1L),
    c("x + 1", "y^2 - 2")
  )
  
})



test_that("stars works", {
  
  expect_equal(
    print(mp(c("x z + 1", "y^2 - 2")), silent = TRUE, stars = TRUE),
    c("x * z  +  1", "y**2  -  2")
  )
  
})


test_that("times_pad works", {
  
  expect_equal(
    print(mp(c("x z + 1", "y^2 - 2")), silent = TRUE, stars = TRUE, times_pad = 0L),
    c("x*z  +  1", "y**2  -  2")
  )
  
})


test_that("errors correctly", {
  
  expect_error(
    print(mp(c("x z + 1", "y^2 - 2")), silent = TRUE, varorder = c("x", "y")),
    "If specified, varorder must contain all computed vars - x, z, y"
  )
  
})


test_that("messages correctly", {
  
  expect_message(
    print(mp(c("x z + 1", "y^2 - 2")), silent = TRUE, order = "lex"),
    "Using variable ordering - x, z, y"
  )
  
})


test_that("silent is coded correctly", {
  
  expect_equal(
    capture.output(print(mp(c("x z + 1", "y^2 - 2")), silent = FALSE)),
    c("x z  +  1", "y^2  -  2")
  )
  
})
