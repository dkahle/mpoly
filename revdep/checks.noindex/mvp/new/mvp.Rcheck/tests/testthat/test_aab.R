## Some specific algebraic identities (here Euler's four-square
## identity and the Brahmagupta-Fibonacci identity), which are usually
## applied to real numbers but work for any commutative ring (such as
## multivariate polynomials).


test_that("Test suite aab.R",{

## Brahmagupta-Fibonacci:

bf <- function(a1,a2,a3,a4,n){
  LHS <- (a1^2 + n*a2^2)*(a3^2 + n*a4^2)
  RHS <- (a1*a3-n*a2*a4)^2 + n*(a1*a4 +a2*a3)^2
  return(LHS == RHS)
}

for(i in 1:10){
    a1 <- rmvp(2,3,2,4)
    a2 <- rmvp(2,3,2,4)
    a3 <- rmvp(2,3,2,4)
    a4 <- rmvp(2,3,2,4)
    n <- rpois(1,lambda=5)
    expect_true(bf(a1,a2,a3,a4,n),info=list(dput(a1),dput(a2),dput(a3),dput(a4),n))
}

# Euler's four-square identity:
checker8 <- function(a1,a2,a3,a4,b1,b2,b3,b4){
  LHS <- 
    (a1^2 + a2^2 + a3^2 + a4^2)*
    (b1^2 + b2^2 + b3^2 + b4^2)
  
  RHS <- (
    +(a1*b1 - a2*b2 - a3*b3 - a4*b4)^2
    +(a1*b2 + a2*b1 + a3*b4 - a4*b3)^2
    +(a1*b3 - a2*b4 + a3*b1 + a4*b2)^2
    +(a1*b4 + a2*b3 - a3*b2 + a4*b1)^2
  )
  return(RHS==RHS)
}


a1 <- rmvp(2,3,2,4)
a2 <- rmvp(2,3,2,4)
a3 <- rmvp(2,3,2,4)
a4 <- rmvp(2,3,2,4)
b1 <- rmvp(2,3,2,4)
b2 <- rmvp(2,3,2,4)
b3 <- rmvp(2,3,2,4)
b4 <- rmvp(2,3,2,4)
expect_true(checker8(a1,a2,a3,a4,b1,b2,b3,b4),
            info=list(dput(a1),dput(a2),dput(a3),dput(a4),
                      dput(b1),dput(b2),dput(b3),dput(b4))
            )

## misc checks:
expect_true(constant(0) == as.mvp(0))
})
