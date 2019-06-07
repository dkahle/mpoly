context("mpoly()")

test_that("mpoly() flags non-list arguments",{
  expect_error(mpoly(1:5), "input to mpoly must be a list.")
})



context("mp() is working properly")


test_that("mp() splices *'s properly", {
  
  expect_equal(
    mp("x y"),
    mpoly(list(c(x = 1, y = 1, coef = 1)))
  )
  
  expect_equal(
    mp("x y z"),
    mpoly(list(c(x = 1, y = 1, z = 1, coef = 1)))
  )
  
  expect_equal(
    mp("a b c d e"),
    mpoly(list(c(a = 1, b = 1, c = 1, d = 1, e = 1, coef = 1)))
  )
  
  expect_equal(
    mp("x1 x2"),
    mpoly(list(c(x1 = 1, x2 = 1, coef = 1)))
  )
  
  expect_equal(
    mp("x1a x2a"),
    mpoly(list(c(x1a = 1, x2a = 1, coef = 1)))
  )
  
  expect_equal(
    mp("x1a^2 x2a^3"),
    mpoly(list(c(x1a = 2, x2a = 3, coef = 1)))
  )
  
})










test_that("mp() parses character vectors properly", {
  
  expect_equal(
    mp(c("2 x y", "-3 x y z")),
    structure(
      list(
        structure(list(c(x = 1, y = 1, coef = 2)), class = "mpoly"), 
        structure(list(c(x = 1, y = 1, z = 1, coef = -3)), class = "mpoly")
      ),
      class = "mpolyList"
    )
  )
  
  expect_equal(
    mp(c("x y", "x y z", "a^3 b c^2 d")),
    structure(
      list(
        structure(list(c(x = 1, y = 1, coef = 1)), class = "mpoly"), 
        structure(list(c(x = 1, y = 1, z = 1, coef = 1)), class = "mpoly"),
        structure(list(c(a = 3, b = 1, c = 2, d = 1, coef = 1)), class = "mpoly")
      ),
      class = "mpolyList"
    )
  )
  
  
  
})









test_that("mp() obeys varorder", {
  
  expect_equal(
    mp("3 y^2 x^3", varorder = c("x", "y")),
    structure(list(c(x = 3, y = 2, coef = 3)), class = "mpoly")
  )
  
})












# 
# 
# 
# 
# 
# 
# 
# test_that("mpoly only allows named whole number exponents",{
#   
#   expect_error(
#     mpoly(list(c(x = "a", coef = 1))), 
#     "each element of list must be of type numeric."
#   )
#   
#   expect_error(
#     mpoly(list(c(x = 1,     1, coef = 2))), 
#     "each element of list must be named for every element."
#   )
#   
#   expect_error(
#     mpoly(list(c(x = 1, y = 1.5, coef = 2))), 
#     "degrees must be nonnegative integers."
#   )
#   
#   expect_error(
#     mpoly(list(c(x = 1, y = -2, coef = 2))), 
#     "degrees must be nonnegative integers."
#   )
#   
#   expect_error(
#     mpoly(list(c(x = 1, y = 2, coef = 2)), varorder = "x"), 
#     "if specified varorder must be a permutation of"
#   )
#   
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# context("Quality of polynomial parsing")
# 
# test_that("parse_nonparenthetical_term works", {
#   
#   f <- parse_nonparenthetical_term
#   m <- mpoly
#   
#   expect_equal(f("0"), m(list(c(coef = 0))))
#   expect_equal(f("1"), m(list(c(coef = 1))))
#   expect_equal(f("+1"), m(list(c(coef = 1))))
#   expect_equal(f("-1"), m(list(c(coef = -1))))
#   
#   expect_equal(f("1.5"), m(list(c(coef = 1.5))))
#   expect_equal(f("+1.5"), m(list(c(coef = 1.5))))
#   expect_equal(f("-1.5"), m(list(c(coef = -1.5))))
#   
#   expect_equal(f(".5"), m(list(c(coef = .5))))
#   expect_equal(f("+.5"), m(list(c(coef = .5))))
#   expect_equal(f("-.5"), m(list(c(coef = -.5))))
#   
#   expect_equal(f("2 2"), m(list(c(coef = 4))))
#   expect_equal(f("-2 2"), m(list(c(coef = -4))))
#   expect_error(f("-2 -2"), "Negative signs are only allowed at the beginning of terms.")
#   
#   expect_equal(f(".5 .5"), m(list(c(coef = .25))))
#   expect_equal(f(".5 2"), m(list(c(coef = 1))))  
#   expect_equal(f("-.5 .5"), m(list(c(coef = -.25))))
#   expect_equal(f("-.5 2"), m(list(c(coef = -1))))  
#   expect_equal(f("-2 .5"), m(list(c(coef = -1))))  
# 
#   expect_equal(f("2^2"), m(list(c(coef = 4))))
#   expect_equal(f("-2^2"), m(list(c(coef = -4))))
#   expect_equal(f(".5^2"), m(list(c(coef = .25))))
#   expect_equal(f("-.5^2"), m(list(c(coef = -.25))))
#  
#   expect_equal(f("0x"), m(list(c(coef = 0))))
#   expect_equal(f("-0x"), m(list(c(coef = 0))))
#   
#   expect_equal(f("0 x"), m(list(c(coef = 0))))
#   expect_equal(f("-0 x"), m(list(c(coef = 0))))
#   
#   expect_equal(f("x"), m(list(c(coef = 1, x = 1))))
#   # expect_equal(f("-x"), m(list(c(coef = -1, x = 1))))
#   
#   # expect_equal(f("- x"), m(list(c(coef = -1, x = 1))))
#   
#   expect_equal(f("2x"), m(list(c(coef = 2, x = 1))))
#   expect_equal(f("-2x"), m(list(c(coef = -2, x = 1))))  
#   expect_equal(f("- 2x"), m(list(c(coef = -2, x = 1))))  
#   
#   expect_equal(f(".2x"), m(list(c(coef = .2, x = 1))))
#   expect_equal(f("-.2x"), m(list(c(coef = -.2, x = 1))))  
#   expect_equal(f("- .2x"), m(list(c(coef = -.2, x = 1))))  
#   
#   expect_equal(f("2 x"), m(list(c(coef = 2, x = 1))))
#   expect_equal(f("-2 x"), m(list(c(coef = -2, x = 1))))  
#   expect_equal(f("- 2 x"), m(list(c(coef = -2, x = 1))))    
#   
#   expect_equal(f(".2 x"), m(list(c(coef = .2, x = 1))))
#   expect_equal(f("-.2 x"), m(list(c(coef = -.2, x = 1))))  
#   expect_equal(f("- .2 x"), m(list(c(coef = -.2, x = 1))))    
#   
#   expect_equal(f("x[1]^2"), m(list(c(coef = 1, "x[1]" = 2))))    
#   expect_equal(f("x[1] ^ 2"), m(list(c(coef = 1, "x[1]" = 2))))    
#   expect_equal(f("2 x[1] ^ 2"), m(list(c(coef = 2, "x[1]" = 2))))    
#   expect_equal(f("-2 x[1] ^ 2"), m(list(c(coef = -2, "x[1]" = 2))))    
#   
#   expect_equal(f("x1^2"), m(list(c(coef = 1, x1 = 2))))    
#   expect_equal(f("x1 ^ 2"), m(list(c(coef = 1, x1 = 2))))    
#   expect_equal(f("2 x1 ^ 2"), m(list(c(coef = 2, x1 = 2))))    
#   expect_equal(f("-2 x1 ^ 2"), m(list(c(coef = -2, x1 = 2))))    
#   
#   expect_equal(f("x^2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x ^ 2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x ^2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x^ 2"), m(list(c(coef = 1, x = 2))))
#   
#   expect_equal(f("x^0"), m(list(c(coef = 1))))
#   # expect_equal(f("-x^0"), m(list(c(coef = -1))))
#   
#   expect_equal(f("2x^2"), m(list(c(coef = 2, x = 2))))  
#   expect_equal(f("-2x^2"), m(list(c(coef = -2, x = 2))))  
#   
#   expect_equal(f("5^2x"), m(list(c(coef = 25, x = 1))))  
#   expect_equal(f("-5^2x"), m(list(c(coef = -25, x = 1))))  
#   expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
#   expect_equal(f("-5^2x^2"), m(list(c(coef = -25, x = 2))))  
#   expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
#   expect_equal(f(".5^2x^2"), m(list(c(coef = .25, x = 2))))  
#   
#   expect_equal(f("12xx 2 y 2x"), m(list(c(coef = 48, xx = 1, y = 1, x = 1))))
#   expect_equal(f("2 x y^2 3 2           3^2"), m(list(c(coef = 108, x = 1, y = 2))))  
#   expect_equal(f("x y .2"), m(list(c(coef = .2, x = 1, y = 1))))
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test_that("parse_nonparenthetical_polynomial works", {
#   
#   f <- parse_nonparenthetical_polynomial
#   m <- mpoly
#   
#   ## the ones from before parse_nonparenthetical_term
#   expect_equal(f("0"), m(list(c(coef = 0))))
#   expect_equal(f("1"), m(list(c(coef = 1))))
#   expect_equal(f("-1"), m(list(c(coef = -1))))
#   
#   expect_equal(f("1.5"), m(list(c(coef = 1.5))))
#   expect_equal(f("-1.5"), m(list(c(coef = -1.5))))
#   
#   expect_equal(f(".5"), m(list(c(coef = .5))))
#   expect_equal(f("-.5"), m(list(c(coef = -.5))))
#   
#   expect_equal(f("2 2"), m(list(c(coef = 4))))
#   expect_equal(f("-2 2"), m(list(c(coef = -4))))
#   # expect_error(f("-2 -2"), "negative signs only allowed at the beginning of terms")
#   
#   expect_equal(f(".5 .5"), m(list(c(coef = .25))))
#   expect_equal(f(".5 2"), m(list(c(coef = 1))))  
#   expect_equal(f("-.5 .5"), m(list(c(coef = -.25))))
#   expect_equal(f("-.5 2"), m(list(c(coef = -1))))  
#   expect_equal(f("-2 .5"), m(list(c(coef = -1))))  
#   
#   expect_equal(f("2^2"), m(list(c(coef = 4))))
#   expect_equal(f("-2^2"), m(list(c(coef = -4))))
#   expect_equal(f(".5^2"), m(list(c(coef = .25))))
#   expect_equal(f("-.5^2"), m(list(c(coef = -.25))))
#   
#   expect_equal(f("0x"), m(list(c(coef = 0))))
#   expect_equal(f("-0x"), m(list(c(coef = 0))))
#   
#   expect_equal(f("0 x"), m(list(c(coef = 0))))
#   expect_equal(f("-0 x"), m(list(c(coef = 0))))
#   
#   expect_equal(f("x"), m(list(c(coef = 1, x = 1))))
#   # expect_equal(f("-x"), m(list(c(coef = -1, x = 1))))
#   
#   # expect_equal(f("- x"), m(list(c(coef = -1, x = 1))))
#   
#   expect_equal(f("2x"), m(list(c(coef = 2, x = 1))))
#   expect_equal(f("-2x"), m(list(c(coef = -2, x = 1))))  
#   expect_equal(f("- 2x"), m(list(c(coef = -2, x = 1))))  
#   
#   expect_equal(f(".2x"), m(list(c(coef = .2, x = 1))))
#   expect_equal(f("-.2x"), m(list(c(coef = -.2, x = 1))))  
#   expect_equal(f("- .2x"), m(list(c(coef = -.2, x = 1))))  
#   
#   expect_equal(f("2 x"), m(list(c(coef = 2, x = 1))))
#   expect_equal(f("-2 x"), m(list(c(coef = -2, x = 1))))  
#   expect_equal(f("- 2 x"), m(list(c(coef = -2, x = 1))))    
#   
#   expect_equal(f(".2 x"), m(list(c(coef = .2, x = 1))))
#   expect_equal(f("-.2 x"), m(list(c(coef = -.2, x = 1))))  
#   expect_equal(f("- .2 x"), m(list(c(coef = -.2, x = 1))))    
#     
#   expect_equal(f("x[1]^2"), m(list(c(coef = 1, "x[1]" = 2))))    
#   expect_equal(f("x[1] ^ 2"), m(list(c(coef = 1, "x[1]" = 2))))    
#   expect_equal(f("2 x[1] ^ 2"), m(list(c(coef = 2, "x[1]" = 2))))    
#   expect_equal(f("-2 x[1] ^ 2"), m(list(c(coef = -2, "x[1]" = 2))))      
#   
#   expect_equal(f("x1^2"), m(list(c(coef = 1, x1 = 2))))    
#   expect_equal(f("x1 ^ 2"), m(list(c(coef = 1, x1 = 2))))    
#   expect_equal(f("2 x1 ^ 2"), m(list(c(coef = 2, x1 = 2))))    
#   expect_equal(f("-2 x1 ^ 2"), m(list(c(coef = -2, x1 = 2))))    
#   
#   expect_equal(f("x^2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x ^ 2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x ^2"), m(list(c(coef = 1, x = 2))))
#   expect_equal(f("x^ 2"), m(list(c(coef = 1, x = 2))))
#   
#   expect_equal(f("x^0"), m(list(c(coef = 1))))
#   # expect_equal(f("-x^0"), m(list(c(coef = -1))))
#   
#   expect_equal(f("2x^2"), m(list(c(coef = 2, x = 2))))  
#   expect_equal(f("-2x^2"), m(list(c(coef = -2, x = 2))))  
#   
#   expect_equal(f("5^2x"), m(list(c(coef = 25, x = 1))))  
#   expect_equal(f("-5^2x"), m(list(c(coef = -25, x = 1))))  
#   expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
#   expect_equal(f("-5^2x^2"), m(list(c(coef = -25, x = 2))))  
#   expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
#   expect_equal(f(".5^2x^2"), m(list(c(coef = .25, x = 2))))  
#   
#   expect_equal(f("12xx 2 y 2x"), m(list(c(coef = 48, xx = 1, y = 1, x = 1))))
#   expect_equal(f("2 x y^2 3 2           3^2"), m(list(c(coef = 108, x = 1, y = 2))))  
#   expect_equal(f("x y .2"), m(list(c(coef = .2, x = 1, y = 1))))
#   
#   
#   
#   ## the new ones
#   expect_equal(f("1+1"), m(list(c(coef = 2))))
#   expect_equal(f("1 + 1"), m(list(c(coef = 2))))
#   expect_equal(f("1+ 1"), m(list(c(coef = 2))))
#   expect_equal(f("1 +1"), m(list(c(coef = 2))))
#   
#   expect_equal(f("1-1"), m(list(c(coef = 0))))
#   expect_equal(f("1 - 1"), m(list(c(coef = 0))))
#   expect_equal(f("1- 1"), m(list(c(coef = 0))))
#   # expect_equal(f("1 -1"), m(list(c(coef = 0))))
#   
#   expect_equal(f("1--1"), m(list(c(coef = 2))))
#   expect_equal(f("1-- 1"), m(list(c(coef = 2))))
#   expect_equal(f("1- -1"), m(list(c(coef = 2))))
#   expect_equal(f("1 --1"), m(list(c(coef = 2))))
#   expect_equal(f("1 -- 1"), m(list(c(coef = 2))))
#   expect_equal(f("1--  1"), m(list(c(coef = 2))))
#   expect_equal(f("1- - 1"), m(list(c(coef = 2))))
#   expect_equal(f("1-  -1"), m(list(c(coef = 2))))
#   expect_equal(f("1 -- 1"), m(list(c(coef = 2))))
#   expect_equal(f("1 - -1"), m(list(c(coef = 2))))
#   expect_equal(f("1  --1"), m(list(c(coef = 2))))
#   
#   expect_equal(f("1+-1"), m(list(c(coef = 0))))
#   expect_equal(f("1+- 1"), m(list(c(coef = 0))))
#   expect_equal(f("1+ -1"), m(list(c(coef = 0))))
#   expect_equal(f("1 +-1"), m(list(c(coef = 0))))
#   expect_equal(f("1 +- 1"), m(list(c(coef = 0))))
#   expect_equal(f("1+-  1"), m(list(c(coef = 0))))
#   expect_equal(f("1+ - 1"), m(list(c(coef = 0))))
#   expect_equal(f("1+  -1"), m(list(c(coef = 0))))
#   expect_equal(f("1 +- 1"), m(list(c(coef = 0))))
#   expect_equal(f("1 + -1"), m(list(c(coef = 0))))
#   expect_equal(f("1  +-1"), m(list(c(coef = 0))))
#   
#   expect_equal(f("-1+1"), m(list(c(coef = 0))))
#   expect_equal(f("- 1+1"), m(list(c(coef = 0))))
#   
#   
#   expect_equal(f("x+1"), m(list(c(coef = 1, x = 1), c(coef = 1))))
#   expect_equal(f("x + 1"), m(list(c(coef = 1, x = 1), c(coef = 1))))
#   expect_equal(f("x +1"), m(list(c(coef = 1, x = 1), c(coef = 1))))
#   expect_equal(f("x+ 1"), m(list(c(coef = 1, x = 1), c(coef = 1))))
#   
#   expect_equal(f("x-1"), m(list(c(coef = 1, x = 1), c(coef = -1))))
#   expect_equal(f("x - 1"), m(list(c(coef = 1, x = 1), c(coef = -1))))
#   expect_equal(f("x -1"), m(list(c(coef = 1, x = 1), c(coef = -1))))
#   expect_equal(f("x- 1"), m(list(c(coef = 1, x = 1), c(coef = -1))))
#   
#   expect_equal(f("-x+1"), m(list(c(coef = -1, x = 1), c(coef = 1))))
#   expect_equal(f("-x + 1"), m(list(c(coef = -1, x = 1), c(coef = 1))))
#   expect_equal(f("-x +1"), m(list(c(coef = -1, x = 1), c(coef = 1))))
#   expect_equal(f("-x+ 1"), m(list(c(coef = -1, x = 1), c(coef = 1))))
#   
#   expect_equal(f("-x-1"), m(list(c(coef = -1, x = 1), c(coef = -1))))
#   expect_equal(f("-x - 1"), m(list(c(coef = -1, x = 1), c(coef = -1))))
#   expect_equal(f("-x -1"), m(list(c(coef = -1, x = 1), c(coef = -1))))
#   expect_equal(f("-x- 1"), m(list(c(coef = -1, x = 1), c(coef = -1))))  
#   
#   expect_equal(f("-1 -1 -1"), m(list(c(coef = -3))))  
#   
#   expect_equal(f("-x+1"), m(list(c(coef = -1, x = 1), c(coef = 1))))
#   expect_equal(f("-x-1"), m(list(c(coef = -1, x = 1), c(coef = -1))))
#   expect_equal(f("- x - 1"), m(list(c(coef = -1, x = 1), c(coef = -1))))
#   
#   expect_equal(f("- x^2 - 1^2"), m(list(c(coef = -1, x = 2), c(coef = -1))))
#   expect_equal(f("x^2 + x^2 y"), m(list(c(coef = 1, x = 2), c(coef = 1, x = 2, y = 1))))
#   expect_equal(f("x-x"), m(list(c(coef = 0))))
#   expect_equal(f("x - x"), m(list(c(coef = 0))))
#   expect_equal(f("x - 4 x"), m(list(c(coef = -3, x = 1))))
#   expect_equal(f("x y^2 - y^2 x"), m(list(c(coef = 0))))
#   
#   expect_equal(f("2^2 x + 5^2 + 3^2"), m(list(c(coef = 4, x = 1), c(coef = 34))))
#   expect_equal(f("1  3 5^2 + 2 3^4 x"), m(list(c(coef = 75), c(coef = 162, x = 1))))
#   expect_equal(f("x - 2 x -3"), m(list(c(coef = -1, x = 1), c(coef = -3))))
#   
#   
#   
#   
#   
#   expect_equal(f("5-2x"), m(list(c(coef = 5), c(coef = -2, x = 1))))
#   expect_equal(f("5 + -2x"), m(list(c(coef = 5), c(coef = -2, x = 1))))
#   expect_equal(f("5^2-x"), m(list(c(coef = 25), c(coef = -1, x = 1))))
#   expect_equal(f("1+-x-x"), m(list(c(coef = 1), c(coef = -2, x = 1))))
#   
#   expect_equal(f("-x + 2y - 4x - -4"), m(list(
#     c(coef = -5, x = 1),
#     c(coef = 2, y = 1),
#     c(coef = 4)
#   )))
#   
#   expect_equal(f("x + 2 y + x^2 y + x y z"), m(list(
#     c(coef = 1, x = 1),
#     c(coef = 2, y = 1),
#     c(coef = 1, x = 2, y = 1),
#     c(coef = 1, x = 1, y = 1, z = 1)
#   )))  
#   
#   expect_equal(f("10 x + 2 y 3 + x^2 5 y"), m(list(
#     c(coef = 10, x = 1), 
#     c(coef = 6, y = 1), 
#     c(coef = 5, x = 2, y = 1)))
#   )
#   
#   expect_equal(f("-4 + 2+2 x +   1 x y^4 -3 prq^3 -y - 3 x 2 - 3 y -2"), m(list(
#     c(coef = -4),
#     c(coef = -4, x = 1),
#     c(coef = 1, x = 1, y = 4),
#     c(coef = -3, prq = 3),
#     c(coef = -4, y = 1)
#   )))  
#   
# })
# 
# 
# 
# test_that("Scientific notation is parsed properly", {
#   
#   m <- mpoly
#   
#   expect_equal(mp("1e+2 x"), m(list(c(coef = 100, x = 1))))
#   expect_equal(mp("1e-2 x"), m(list(c(coef = .01, x = 1))))
#   expect_equal(mp("e-a"), m(list(c(coef = 1, e = 1), c(coef = -1, a = 1))))
#   expect_equal(mp("e+a"), m(list(c(coef = 1, e = 1), c(coef =  1, a = 1))))
#   expect_equal(mp("a-e"), m(list(c(coef = 1, a = 1), c(coef = -1, e = 1))))
#   expect_equal(mp("a+e"), m(list(c(coef = 1, a = 1), c(coef =  1, e = 1))))
# })
# 
# 
# 














# mp("(x)")
# mp("((((x))))")
# mp("(x + 0)")
# mp("(x + 1)")
# mp("(x - 1)")
# mp("(-1 x - 1)")
# mp("2 (x + 1)")
# mp("-1 (x + 1)")
# 
# mp("-2 x + -1 x (3 x - (7 - 2 x)) 7 (x + 1) -3")
# 
# mp("(x-.25)^2")
# mp("(x + 1) (x - 1)")
# mp("(x + 1) (x + 2)")
# mp("(x + 1)^5")
# mp("(x+1)^5")
# mp("3 (x + 1) (x + 2)^2")
# mp("(x + y)^10")
#
# mp("(x + y) (x - y)")
# mp("((x + y) (x - y))^2")
# mp("((x + y) (x - y)^2)")
# mp("((x + y) (x - y)^2 3)^2")
#
# mp(c("x","x + y"))