context("LCM() is working properly")

test_that("LCM works", {
  expect_equal(LCM(5L,  7L), 35L)
  expect_equal(LCM(5L,  8L), 40L)
  expect_equal(LCM(5L,  9L), 45L)
  expect_equal(LCM(5L, 10L), 10L)
})
