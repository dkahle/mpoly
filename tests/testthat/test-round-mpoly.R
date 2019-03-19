context("round_mpoly()")


test_that("basic", {
  
  expect_equal(
    mp("(x + 3.14159265)^4"),
    mp("x^4  +  12.56637 x^3  +  59.21763 x^2  +  124.0251 x  +  97.40909"),
    tolerance = 1e-5
  )
  
  expect_equal(
    round(mp("(x + 3.14159265)^4")),
    mp("x^4  +  12.566 x^3  +  59.218 x^2  +  124.025 x  +  97.409")
  )
  
  expect_equal(
    round(mp("(x + 3.14159265)^4"), 0),
    mp("x^4  +  13 x^3  +  59 x^2  +  124 x  +  97")
  )
  
})