context("eq_mp()")


test_that("eq_mp() properly deals with spaces", {
  expect_equal(eq_mp("y = x"), mp("y - x"))
  expect_equal(eq_mp("y   =x"), mp("y - x"))
})


test_that("eq_mp() works with = or ==", {
  expect_equal(eq_mp("y = x"), eq_mp("y == x"))
})


test_that("eq_mp() is properly vectorized", {
  expect_equal(
    eq_mp(c("y = x", "x^2 + y^2 = 1")), 
    mp(c("y - x", "x^2 + y^2 - 1"))
  )
})
