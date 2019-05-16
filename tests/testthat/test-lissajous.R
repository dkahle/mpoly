context("lissajous()")


test_that("lissajous() works properly", {
  
  expect_equal(
    lissajous(3, 2,  -pi/2, 0),
    mp("-4 x^2  +  4 x^4  +  9 y^2  -  24 y^4  +  16 y^6")
  )
  
})

