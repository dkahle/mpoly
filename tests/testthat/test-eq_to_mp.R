context("eq_to_mp() is working properly")


test_that("eq_to_mp() properly deals with spaces", {
  expect_equal(eq_to_mp("y = x"), mp("y - x"))
  expect_equal(eq_to_mp("y   =x"), mp("y - x"))
})


test_that("eq_to_mp() works with = or ==", {
  expect_equal(eq_to_mp("y = x"), eq_to_mp("y == x"))
})


test_that("eq_to_mp() is properly vectorized", {
  expect_equal(
    eq_to_mp(c("y = x", "x^2 + y^2 = 1")), 
    mp(c("y - x", "x^2 + y^2 - 1"))
  )
})
