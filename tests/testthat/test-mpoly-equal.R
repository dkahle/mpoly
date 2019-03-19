context("mpoly == mpoly")

test_that("basic", {
  
  expect_true(mp("x + y^2") == mp("x + y^2"))
  expect_false(mp("x + y^2") == mp("x"))
  expect_error(
    mp("2") == 2,
    "e1 and e2 must be of class mpoly.",
    fixed = TRUE
  )
  
})



context("mpoly != mpoly")

test_that("basic", {
  
  expect_false(mp("x + y^2") != mp("x + y^2"))
  expect_true( mp("x + y^2") != mp("y^2"))
  expect_error(
    mp("2") != 2,
    "e1 and e2 must be of class mpoly.",
    fixed = TRUE
  )
  
})
