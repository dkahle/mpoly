context("solve_unipoly()")


test_that("solve_unipoly() works with real roots", {
  
  expect_equal(
    solve_unipoly(mp("x^2 - 1"), real_only = TRUE),
    c(1, -1)
  )
  
})


test_that("solve_unipoly() works generally", {
  
  expect_equal(
    solve_unipoly(mp("x^2 - 1")),
    c(1+0i, -1+0i)
  )
  
  expect_equal(
    solve_unipoly(mp("x^2 - 2")),
    c(1.41421356237309-0i, -1.41421356237309+0i)
  )
  
  expect_equal(
    solve_unipoly(mp("x^3 - 1")),
    c(1-0i, -0.5+0.866025403784439i, -0.5-0.866025403784439i)
  )
  
  expect_equal(
    solve_unipoly(mp("x^4 - 1")),
    c(0+1i, -1-0i, 0-1i, 1+0i)
  )
  
})