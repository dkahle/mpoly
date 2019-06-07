context("mpolyList arithmetic")


test_that("addition", {
  
  expect_equal(
    mp(c("x", "y")) + mp(c("y^2", "x")),
    structure(
      list(
        structure(
          list(
            c(x = 1, coef = 1), 
            c(y = 2, coef = 1)
          ), 
          class = "mpoly"
        ), 
        structure(
          list(
            c(y = 1, coef = 1), 
            c(x = 1, coef = 1)
          ), 
          class = "mpoly")
        ), 
      class = "mpolyList"
    )
  )
  
  # expect_equal(
  #   mp("2") + mp(c("x", "y")),
  #   mp(c("x + 2", "y + 2"))
  # )
  
  expect_equal(
    2 + mp(c("x", "y")),
    mp(c("x + 2", "y + 2"))
  )
  
  # expect_equal(
  #   mp(c("x", "y")) + mp("2"),
  #   mp(c("x + 2", "y + 2"))
  # )
  
  expect_equal(
    mp(c("x", "y")) + 2,
    mp(c("x + 2", "y + 2"))
  )
  
  expect_equal(
    c(2, -3) + mp(c("x", "y")),
    mp(c("x + 2", "y - 3"))
  )
  
  expect_equal(
    mp(c("x", "y")) + c(2, -3),
    mp(c("x + 2", "y - 3"))
  )
  
  expect_error(
    c(2, -3) + mp(c("x", "y", "z")),
    "e1 and e2 must have equal length.",
    fixed = TRUE
  )
  
  expect_error(
    mp(c("x", "y", "z")) + c(2, -3),
    "e1 and e2 must have equal length.",
    fixed = TRUE
  )
    
  
})





test_that("subtraction", {
  
  expect_equal(
    mp(c("x", "y")) - mp(c("x", "x^2")),
    structure(
      list(
        structure(list(c(coef = 0)), class = "mpoly"), 
        structure(
          list(
            c(y = 1, coef = 1), 
            c(x = 2, coef = -1)
          ), 
          class = "mpoly")
        ), 
      class = "mpolyList"
    )
  )
  
  # expect_equal(
  #   mp("2") - mp(c("x", "y")),
  #   mp(c("-1 x + 2", "-1 y + 2"))
  # )
  
  expect_equal(
    2 - mp(c("x", "y")),
    mp(c("-1 x + 2", "-1 y + 2"))
  )
  
  # expect_equal(
  #   mp(c("x", "y")) - mp("2"),
  #   mp(c("x - 2", "y - 2"))
  # )
  
  expect_equal(
    mp(c("x", "y")) - 2,
    mp(c("x - 2", "y - 2"))
  )
  
  expect_equal(
    c(2, -3) - mp(c("x", "y")),
    mp(c("-1 x + 2", "-1 y - 3"))
  )
  
  expect_equal(
    mp(c("x", "y")) - c(2, -3),
    mp(c("x - 2", "y + 3"))
  )
  
  expect_error(
    c(2, -3) - mp(c("x", "y", "z")),
    "e1 and e2 must have equal length.",
    fixed = TRUE
  )
  
  expect_error(
    mp(c("x", "y", "z")) - c(2, -3),
    "e1 and e2 must have equal length.",
    fixed = TRUE
  )
  
})





test_that("multiplication", {
  
  expect_equal(
    mp(c("x", "y")) * mp(c("x", "x^2")),
    structure(
      list(
        structure(list(c(x = 2, coef = 1)), class = "mpoly"), 
        structure(list(c(y = 1, x = 2, coef = 1)), class = "mpoly")
      ), 
      class = "mpolyList"
    )
  )
  
  # expect_equal(
  #   mp("2") * mp(c("x", "y")),
  #   mp(c("2 x", "2 y"))
  # )
  
  expect_equal(
    2 * mp(c("x", "y")),
    mp(c("2 x", "2 y"))
  )
  
  # expect_equal(
  #   mp(c("x", "y")) * mp("2"),
  #   mp(c("2 x", "2 y"))
  # )
  
  expect_equal(
    mp(c("x", "y")) * 2,
    mp(c("2 x", "2 y"))
  )
  
  expect_equal(
    c(2, -3) * mp(c("x", "y")),
    mp(c("2 x", "-3 y"))
  )
  
  expect_error(
    c(2, -3) * mp(c("x", "y", "z")),
    "e1 and e2 must have equal length.",
    fixed = TRUE
  )
  
})











test_that("exponentiation", {
  
  expect_equal(
    mp(c("x", "y"))^2L,
    structure(
      list(
        structure(list(c(x = 2, coef = 1)), class = "mpoly"), 
        structure(list(c(y = 2, coef = 1)), class = "mpoly")
      ), 
      class = "mpolyList"
    )
  )
  
  
  expect_equal(
    mp(c("x", "y"))^0L,
    structure(
      list(
        structure(list(c(coef = 1)), class = "mpoly"), 
        structure(list(c(coef = 1)), class = "mpoly")
      ), 
      class = "mpolyList"
    )
  )
  
  expect_error(
    mp(c("x", "y"))^1.5,
    "exponent must be a positive integer.",
    fixed = TRUE
  )
  
})
