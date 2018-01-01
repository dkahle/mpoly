context("Arithmetic with constants works properly")

test_that("Addition works", {
  
  expect_equal(0 + mp("1"), mp("1"))
  expect_equal(0 + mp("x"), mp("x"))
  expect_equal(1 + mp("1"), mp("2"))
  expect_equal(1 + mp("x"), mp("x + 1"))
  expect_equal(-1 + mp("1"), mp("0"))
  expect_equal(-1 + mp("x"), mp("-1 + x"))
  
  expect_equal(mp("1") + 0, mp("1"))
  expect_equal(mp("x") + 0, mp("x"))
  expect_equal(mp("1") + 1, mp("2"))
  expect_equal(mp("x") + 1, mp("x + 1"))
  expect_equal(mp("1") + -1, mp("0"))
  expect_equal(mp("x") + -1, mp("x - 1"))
  
})


test_that("Subtraction works", {
  
  expect_equal(0 - mp("1"), mp("-1"))
  expect_equal(0 - mp("x"), mp("-x"))
  expect_equal(1 - mp("1"), mp("0"))
  expect_equal(1 - mp("x"), mp("1 - x"))
  expect_equal(-1 - mp("1"), mp("-2"))
  expect_equal(-1 - mp("x"), mp("-1 - x"))
  
  expect_equal(mp("1") - 0, mp("1"))
  expect_equal(mp("x") - 0, mp("x"))
  expect_equal(mp("1") - 1, mp("0"))
  expect_equal(mp("x") - 1, mp("x - 1"))
  expect_equal(mp("1") - -1, mp("2"))
  expect_equal(mp("x") - -1, mp("x + 1"))
  
})


test_that("Multiplication works", {
  
  expect_equal(0 * mp("1"), mp("0"))
  expect_equal(0 * mp("x"), mp("0"))
  expect_equal(1 * mp("1"), mp("1"))
  expect_equal(1 * mp("x"), mp("x"))
  expect_equal(-1 * mp("1"), mp("-1"))
  expect_equal(-1 * mp("x"), mp("-x"))
  
  expect_equal(mp("1") * 0, mp("0"))
  expect_equal(mp("x") * 0, mp("0"))
  expect_equal(mp("1") * 1, mp("1"))
  expect_equal(mp("x") * 1, mp("x"))
  expect_equal(mp("1") * -1, mp("-1"))
  expect_equal(mp("x") * -1, mp("-x"))
  
})