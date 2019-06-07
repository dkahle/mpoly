context("Polynomial component extraction")


test_that("[.mpoly", {
  
  expect_equal(mp("x + y")[1], mp("x"))
  expect_equal(mp("x + y + z")[1:2], mp("x + y"))
  expect_equal(mp("x + y + z")[-1], mp("y + z"))
  
})


test_that("LT, LC, and LM work - lex", {
  
  expect_equal(
    LT(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    mp("3 x^10")
  )
  
  expect_equal(
    LC(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    3
  )
  
  expect_equal(
    LM(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    mp("x^10")
  )
  
})


test_that("multideg works - lex", {
  
  expect_equal(multideg(mp("1")), 0L)
  
  expect_equal(
    multideg(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    c(x = 10, y = 0, z = 0)
  )
  
})


test_that("totaldeg works - lex", {
  
  expect_equal(
    totaldeg(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    10
  )
  
  expect_equal(
    totaldeg(mp(c("x y", "y z^2", "0"))),
    c(2L, 3L, 0L)
  )
  
  expect_error(
    totaldeg("5 x"),
    "totaldeg requires an mpoly or mpolyList object.",
    fixed = TRUE
  )
  
})


test_that("monomials works", {
  
  expect_equal(
    monomials(mp("x y^2 + x (x+1) (x+2) x z + 3 x^10")), 
    mp(c("x y^2", "x^4 z", "3 x^3 z", "2 x^2 z", "3 x^10"))
  )
  
  expect_equal(
    monomials(mp("x y^2 + 3 y^3 x^2")), 
    mp(c("x y^2", "3 x^2 y^3"), varorder = c("x", "y"))
  )
  
  expect_error(
    monomials("5 x"),
    "monomials requires an mpoly or mpolyList object.",
    fixed = TRUE
  )
  
  expect_equal(
    suppressWarnings(terms(mp("x y^2 + 3 y^3 x^2"))), 
    mp(c("x y^2", "3 x^2 y^3"), varorder = c("x", "y"))
  )
  
  expect_warning(
    terms(mp("x y^2 + 3 y^3 x^2")),
    "'terms.mpoly' is deprecated.",
    fixed = TRUE
  )
  
})


test_that("exponents works", {
  
  expect_equal(
    exponents(mp("x y^2 + 3 y^3 x^2")), 
    list(structure(1:2, .Names = c("x", "y")), structure(2:3, .Names = c("x", "y")))
  )
  
  expect_equal(
    exponents(mp("x y^2 + 3 y^3 x^2 z"), reduced = TRUE),
    list(structure(1:2, .Names = c("x", "y")), c(x = 2L, y = 3L, z = 1L))
  )
  
})




