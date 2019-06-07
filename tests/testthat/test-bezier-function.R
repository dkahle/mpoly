context("bezier_function()")


test_that("basic", {
  
  t <- seq(0, 1, length.out = 11)
  points <- data.frame(x = 0:3, y = c(0,1,-1,0))  
  
  expect_equal(
    as.data.frame(bezier_function(points)(t)),
    structure(
      list(
        "x" = c(3, 2.7, 2.4, 2.1, 1.8, 1.5, 1.2, 0.9, 0.6, 0.3, 0), 
        "y" = c(0, -0.216, -0.288, -0.252, -0.144, 0, 0.144, 0.252, 0.288, 0.216, 0)
      ), 
      class = "data.frame", 
      row.names = c(NA, -11L)
    )
  )
  
})



test_that("bezierFunction is deprecated", {
  
  t <- seq(0, 1, length.out = 11)
  points <- data.frame(x = 0:3, y = c(0,1,-1,0))  

  expect_equal(
    suppressWarnings(as.data.frame(bezierFunction(points)(t))),
    structure(
      list(
        "x" = c(3, 2.7, 2.4, 2.1, 1.8, 1.5, 1.2, 0.9, 0.6, 0.3, 0), 
        "y" = c(0, -0.216, -0.288, -0.252, -0.144, 0, 0.144, 0.252, 0.288, 0.216, 0)
      ), 
      class = "data.frame", 
      row.names = c(NA, -11L)
    )
  )
  
  expect_warning(
    bezierFunction(points),
    "'bezierFunction' is deprecated.",
    fixed = TRUE
  )
    
})
