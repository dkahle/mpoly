context("reorder.mpoly()")


# examples from cox little and o'shea, 3e, p.59

test_that("lex works", {
  
  expect_equal(
    reorder.mpoly(mp("4 x y^2 z + 4 z^2 - 5 x^3 + 7 x^2 z^2"), order = "lex", varorder = c("x", "y", "z")),
    structure(
      list(
        c(x = 3, coef = -5), 
        c(x = 2, z = 2, coef = 7), 
        c(x = 1, y = 2, z = 1, coef = 4), 
        c(z = 2, coef = 4)
      ), 
      class = "mpoly"
    )
  )

})



test_that("glex works", {
  
  expect_equal(
    reorder.mpoly(mp("4 x y^2 z + 4 z^2 - 5 x^3 + 7 x^2 z^2"), order = "glex", varorder = c("x", "y", "z")),
    structure(
      list(
        c(x = 2, z = 2, coef = 7), 
        c(x = 1, y = 2, z = 1, coef = 4), 
        c(x = 3, coef = -5), 
        c(z = 2, coef = 4)
      ), 
      class = "mpoly"
    )
  )
  
})



test_that("grlex works", {
  
  expect_equal(
    reorder.mpoly(mp("4 x y^2 z + 4 z^2 - 5 x^3 + 7 x^2 z^2"), order = "grlex", varorder = c("x", "y", "z")),
    structure(
      list(
        c(x = 1, y = 2, z = 1, coef = 4), 
        c(x = 2, z = 2, coef = 7), 
        c(x = 3, coef = -5), 
        c(z = 2, coef = 4)
      ), 
      class = "mpoly"
    )
  )
  
})



test_that("message when no order specified but varorder not", {
  
  expect_message(
    reorder.mpoly(mp("x + y^2"), order = "glex"),
    "Using variable ordering - x, y",
    fixed = TRUE
  )
  
})



test_that("all vars in varorder", {
  
  expect_error(
    reorder.mpoly(mp("x + y^2"), varorder = "x"),
    "If specified, varorder must contain all computed vars - x, y",
    fixed = TRUE
  )
  
})



