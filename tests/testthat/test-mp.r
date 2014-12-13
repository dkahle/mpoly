context("Quality of polynomial parsing")



test_that("parse_nonparenthetical_term works", {
  
  f <- parse_nonparenthetical_term
  m <- mpoly
  
  expect_equal(f("0"), m(list(c(coef = 0))))
  expect_equal(f("1"), m(list(c(coef = 1))))
  expect_equal(f("-1"), m(list(c(coef = -1))))
  
  expect_equal(f("2^2"), m(list(c(coef = 4))))
  expect_equal(f("-2^2"), m(list(c(coef = -4))))
  
  expect_equal(f("0x"), m(list(c(coef = 0))))
  expect_equal(f("-0x"), m(list(c(coef = 0))))
  
  expect_equal(f("0 x"), m(list(c(coef = 0))))
  expect_equal(f("-0 x"), m(list(c(coef = 0))))
  
  expect_equal(f("x"), m(list(c(coef = 1, x = 1))))
  expect_equal(f("-x"), m(list(c(coef = -1, x = 1))))
  
  expect_equal(f("- x"), m(list(c(coef = -1, x = 1))))
  
  expect_equal(f("2x"), m(list(c(coef = 2, x = 1))))
  expect_equal(f("-2x"), m(list(c(coef = -2, x = 1))))  
  expect_equal(f("- 2x"), m(list(c(coef = -2, x = 1))))  
  
  expect_equal(f("2 x"), m(list(c(coef = 2, x = 1))))
  expect_equal(f("-2 x"), m(list(c(coef = -2, x = 1))))  
  expect_equal(f("- 2 x"), m(list(c(coef = -2, x = 1))))    
  
  expect_equal(f("x^2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x ^ 2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x ^2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x^ 2"), m(list(c(coef = 1, x = 2))))
  
  expect_equal(f("x^0"), m(list(c(coef = 1))))
  expect_equal(f("-x^0"), m(list(c(coef = -1))))
  
  expect_equal(f("2x^2"), m(list(c(coef = 2, x = 2))))  
  expect_equal(f("-2x^2"), m(list(c(coef = -2, x = 2))))  
  
  expect_equal(f("5^2x"), m(list(c(coef = 25, x = 1))))  
  expect_equal(f("-5^2x"), m(list(c(coef = -25, x = 1))))  
  expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
  expect_equal(f("-5^2x^2"), m(list(c(coef = -25, x = 2))))  
  expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
  
  expect_equal(f("12xx 2 y 2x"), m(list(c(coef = 48, xx = 1, y = 1, x = 1))))
  expect_equal(f("-2      7"), m(list(c(coef = -14))))  
  expect_equal(f("2 x y^2 3 2           3^2"), m(list(c(coef = 108, x = 1, y = 2))))  
  
})
























test_that("parse_nonparenthetical_polynomial works", {
  
  f <- parse_nonparenthetical_polynomial
  m <- mpoly
  
  ## the ones from before parse_nonparenthetical_term
  expect_equal(f("0"), m(list(c(coef = 0))))
  expect_equal(f("1"), m(list(c(coef = 1))))
  expect_equal(f("-1"), m(list(c(coef = -1))))
  
  expect_equal(f("2^2"), m(list(c(coef = 4))))
  expect_equal(f("-2^2"), m(list(c(coef = -4))))
  
  expect_equal(f("0x"), m(list(c(coef = 0))))
  expect_equal(f("-0x"), m(list(c(coef = 0))))
  
  expect_equal(f("0 x"), m(list(c(coef = 0))))
  expect_equal(f("-0 x"), m(list(c(coef = 0))))
  
  expect_equal(f("x"), m(list(c(coef = 1, x = 1))))
  expect_equal(f("-x"), m(list(c(coef = -1, x = 1))))
  
  expect_equal(f("- x"), m(list(c(coef = -1, x = 1))))
  
  expect_equal(f("2x"), m(list(c(coef = 2, x = 1))))
  expect_equal(f("-2x"), m(list(c(coef = -2, x = 1))))  
  expect_equal(f("- 2x"), m(list(c(coef = -2, x = 1))))  
  
  expect_equal(f("2 x"), m(list(c(coef = 2, x = 1))))
  expect_equal(f("-2 x"), m(list(c(coef = -2, x = 1))))  
  expect_equal(f("- 2 x"), m(list(c(coef = -2, x = 1))))    
  
  expect_equal(f("x^2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x ^ 2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x ^2"), m(list(c(coef = 1, x = 2))))
  expect_equal(f("x^ 2"), m(list(c(coef = 1, x = 2))))
  
  expect_equal(f("x^0"), m(list(c(coef = 1))))
  expect_equal(f("-x^0"), m(list(c(coef = -1))))
  
  expect_equal(f("2x^2"), m(list(c(coef = 2, x = 2))))  
  expect_equal(f("-2x^2"), m(list(c(coef = -2, x = 2))))  
  
  expect_equal(f("5^2x"), m(list(c(coef = 25, x = 1))))  
  expect_equal(f("-5^2x"), m(list(c(coef = -25, x = 1))))  
  expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
  expect_equal(f("-5^2x^2"), m(list(c(coef = -25, x = 2))))  
  expect_equal(f("5^2x^2"), m(list(c(coef = 25, x = 2))))  
  
  expect_equal(f("12xx 2 y 2x"), m(list(c(coef = 48, xx = 1, y = 1, x = 1))))
  expect_equal(f("-2      7"), m(list(c(coef = -14))))  
  expect_equal(f("2 x y^2 3 2           3^2"), m(list(c(coef = 108, x = 1, y = 2))))
  
  
  
  ## the new ones
  expect_equal(f("1+1"), m(list(c(coef = 2))))
  expect_equal(f("1 + 1"), m(list(c(coef = 2))))
  expect_equal(f("1+ 1"), m(list(c(coef = 2))))
  expect_equal(f("1 +1"), m(list(c(coef = 2))))
  
  expect_equal(f("1-1"), m(list(c(coef = 0))))
  expect_equal(f("1 - 1"), m(list(c(coef = 0))))
  expect_equal(f("1- 1"), m(list(c(coef = 0))))
  expect_equal(f("1 -1"), m(list(c(coef = 0))))
  
  expect_equal(f("1--1"), m(list(c(coef = 2))))
  expect_equal(f("1-- 1"), m(list(c(coef = 2))))
  expect_equal(f("1- -1"), m(list(c(coef = 2))))
  expect_equal(f("1 --1"), m(list(c(coef = 2))))
  expect_equal(f("1 -- 1"), m(list(c(coef = 2))))
  expect_equal(f("1--  1"), m(list(c(coef = 2))))
  expect_equal(f("1- - 1"), m(list(c(coef = 2))))
  expect_equal(f("1-  -1"), m(list(c(coef = 2))))
  expect_equal(f("1 -- 1"), m(list(c(coef = 2))))
  expect_equal(f("1 - -1"), m(list(c(coef = 2))))
  expect_equal(f("1  --1"), m(list(c(coef = 2))))
  
  expect_equal(f("1+-1"), m(list(c(coef = 0))))
  expect_equal(f("1+- 1"), m(list(c(coef = 0))))
  expect_equal(f("1+ -1"), m(list(c(coef = 0))))
  expect_equal(f("1 +-1"), m(list(c(coef = 0))))
  expect_equal(f("1 +- 1"), m(list(c(coef = 0))))
  expect_equal(f("1+-  1"), m(list(c(coef = 0))))
  expect_equal(f("1+ - 1"), m(list(c(coef = 0))))
  expect_equal(f("1+  -1"), m(list(c(coef = 0))))
  expect_equal(f("1 +- 1"), m(list(c(coef = 0))))
  expect_equal(f("1 + -1"), m(list(c(coef = 0))))
  expect_equal(f("1  +-1"), m(list(c(coef = 0))))
  
  expect_equal(f("-1+1"), m(list(c(coef = 0))))
  expect_equal(f("- 1+1"), m(list(c(coef = 0))))
  
  
  expect_equal(f("x+1"), m(list(
    c(coef = 1, x = 1),
    c(coef = 1)
  )))
  
  expect_equal(f("x + 1"), m(list(
    c(coef = 1, x = 1),
    c(coef = 1)
  )))
  
  expect_equal(f("x +1"), m(list(
    c(coef = 1, x = 1),
    c(coef = 1)
  )))
  
  expect_equal(f("x+ 1"), m(list(
    c(coef = 1, x = 1),
    c(coef = 1)
  )))
  
  expect_equal(f("x-1"), m(list(
    c(coef = 1, x = 1),
    c(coef = -1)
  )))
  
  expect_equal(f("x - 1"), m(list(
    c(coef = 1, x = 1),
    c(coef = -1)
  )))
  
  expect_equal(f("x -1"), m(list(
    c(coef = 1, x = 1),
    c(coef = -1)
  )))
  
  expect_equal(f("x- 1"), m(list(
    c(coef = 1, x = 1),
    c(coef = -1)
  )))
  

  
  expect_equal(f("-x+1"), m(list(
    c(coef = -1, x = 1),
    c(coef = 1)
  )))
  
  expect_equal(f("-x-1"), m(list(
    c(coef = -1, x = 1),
    c(coef = -1)
  )))
  
  expect_equal(f("- x - 1"), m(list(
    c(coef = -1, x = 1),
    c(coef = -1)
  )))
  
  expect_equal(f("- x^2 - 1^2"), m(list(
    c(coef = -1, x = 2),
    c(coef = -1)
  )))

  
  
  expect_equal(f("5-2x"), m(list(
    c(coef = 5),
    c(coef = -2, x = 1)
  )))
  
  expect_equal(f("5 + -2x"), m(list(
    c(coef = 5),
    c(coef = -2, x = 1)
  )))
  
  expect_equal(f("5^2-x"), m(list(
    c(coef = 25),
    c(coef = -1, x = 1)
  )))
  
  expect_equal(f("1+-x-x"), m(list(
    c(coef = 1),
    c(coef = -2, x = 1)
  )))
  
  expect_equal(f("-x + 2y - 4x - -4"), m(list(
    c(coef = -5, x = 1),
    c(coef = 2, y = 1),
    c(coef = 4)
  )))
  
  expect_equal(f("-4 + 2+2 x +   1 x y^4 -3 prq^3 -y - 3 x 2 - 3 y -2"), m(list(
    c(coef = -4),
    c(coef = -4, x = 1),
    c(coef = 1, x = 1, y = 4),
    c(coef = -3, prq = 3),
    c(coef = -4, y = 1)
  )))  
  
})


















































#' mp('x - y')
#' mp('x - 1')
#' mp('x +      y')
#' mp('x -      5')
#' mp('x - -5')
#' mp('10 x 6 x') # -> 60 x^2
#' mp('10 x 6 x + 10 x 6 x y y 2') # -> 60 x^2  +  120 x^2 y^2
#'
#' mp('x^2 + x^2 y') # -> x^2  +  x^2 y
#'
#' mp('x - x') # -> 0
#' mp('x - 4 x') # -> -3 x
#' mp('x y^2 - y^2 x') # -> 0
#' 
#' mp('5^2') # -> 25
#' mp('2^2 x + 5^2 + 3^2') # -> 4 x  +  34
#' mp('1 1') # -> 1
#' mp('-1 -1 -1') # -> 1
#' mp('1  3 5^2 + 2 3^4 x') # -> 75  + 162 x
#' mp("x - 2 x -3") # 7 x
#'
#' ( ms <- mp(c('x + y', '2 x')) )
#' is.mpolyList(ms)
#'
#' mp('10 x + 2 y 3 + x^2 5 y') # -> 10 x  +  6 y  +  5 x^2 y
#' mp('x + 2 y + x^2 y + x y z') # -> x  +  2 y  +  x^2 y  +  x y z
#' mp('x + 2 y + x^2 y + x y z', varorder = c('y', 'z', 'x')) # -> x  +  2 y  +  y  +  y z x
#' # mp('x + 2 y + x^2 y', varorder = c('q', 'p')) # -> error
#'
#' mp('p111 + p121 2 p112^2')
#' unclass(mp('p111 + p121 2 p112^2'))
#'
#' mp('0')
#' mp('2')
#' mp('-5')
#' mp('-4 x')
#' mp('y + -1 x')
#' mp('-1 + x')
#' mp('-1 x')
#' mp('-1x')
#' mp('-x')
#' 
#' mp("(x)")
#' mp("((((x))))")
#' mp("(x + 0)")
#' mp("(x + 1)")
#' mp("(x - 1)")
#' mp("(-1 x - 1)")
#' mp("2 (x + 1)")
#' mp("-1 (x + 1)")
#' 
#' mp("-2 x + -1 x (3 x - (7 - 2 x)) 7 (x + 1) -3")
#' 
#' mp("(x + 1) (x - 1)")
#' mp("(x + 1) (x + 2)")
#' mp("(x + 1)^5")
#' mp("(x+1)^5")
#' mp("3 (x + 1) (x + 2)^2")
#' mp("(x + y)^10")
#'
#' mp("(x + y) (x - y)")
#' mp("((x + y) (x - y))^2")
#' mp("((x + y) (x - y)^2)")
#' mp("((x + y) (x - y)^2 3)^2")
#'
#' mp(c("x","x + y"))