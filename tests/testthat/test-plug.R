context("plug()")


test_that("plug() inserts numbers properly", {
  
  expect_equal(
    plug(mp("(x + y)^3"), "y", 5),
    mp("(x + 5)^3")
  )
  
})



test_that("plug() changes variables properly", {
  
  expect_equal(
    plug(mp("(x + y)^3"), "x", "t"),
    mp("(t + y)^3")
  )
  
  expect_equal(
    plug(mp("(x + y)^3"), "x", "t_2"),
    mp("(t_2 + y)^3")
  )
  
  expect_equal(
    plug(mp("(x + y)^3"), "x", "xx"),
    mp("(xx + y)^3")
  )
  
})



test_that("plug() inserts mpoly's properly", {
  
  expect_equal(
    plug(mp("(x + y)^3"), "x", mp("x^2")),
    mp("(x^2 + y)^3")
  )
  
})




test_that("plug() works on mpolyLists", {
  
  expect_equal(
    plug(mp(c("(x + y)^3", "2 x")), "x", mp("x^2")),
    mp(c("(x^2 + y)^3", "2 x^2"))
  )
  
})
