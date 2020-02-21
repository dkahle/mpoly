## This file follows the structure of aaa.R in the free group package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  

test_that("Test suite aaa.R",{

checker1 <- function(x){
    expect_true(x==x, info=dput(x))

    expect_true(x == x + constant(0), info=dput(x))
    expect_true(x == x + 0, info=dput(x))
    expect_true(x == (x + 4) -4, info=dput(x))
    expect_true(x == -(-x), info=dput(x))
    expect_true(x == +(+x), info=dput(x))

    expect_true(x+x-x == x, info=dput(x))

    expect_true(is.zero(x-x), info=dput(x))

    expect_true(0*x == constant(0), info=dput(x))
    expect_true(1*x == x, info=dput(x))
    expect_true(2*x == x+x, info=dput(x))
    expect_true(3*x == x+x+x, info=dput(x))
    expect_true(4*x == x+x+x+x, info=dput(x))
    expect_true(5*x == x+x+x+x+x, info=dput(x))
    expect_true(6*x == x+x+x+x+x+x, info=dput(x))

    expect_true(x^0 == constant(1), info=dput(x))
    expect_true(x^1 == x, info=dput(x))
    expect_true(x^2 == x*x, info=dput(x))
    expect_true(x^3 == x*x*x, info=dput(x))
    expect_true(x^4 == x*x*x*x, info=dput(x))
    
    ## check constant() and constant<-():
    ## checks below include 
    y <- x
    expect_true(constant(x) == constant(y), info=dput(x))
    constant(y) <- 4
    expect_true(constant(y) == 4, info=dput(x))
    constant(y) <- 0
    expect_true(constant(y) == 0, info=dput(x))
  
  
    ## check invert():
    expect_true(invert(invert(x))==x, info=dput(x))
    if(length(allvars(x))>0){
        v <- allvars(x)[1]
        expect_true(invert(invert(x,v[1]),v[1])==x, info=dput(x))
    }
    
    if(length(allvars(x))>0){
        v <- allvars(x)[1]
        expect_true(invert(invert(x,v[1]),v[1])==x, info=dput(x))
    }
    
    ## check subs():
    if(length(allvars(x))>0){
        v <- allvars(x)[1]
        jj1 <- list(2)
        names(jj1) <- v
        jj2 <- list(1/2)
        names(jj2) <- v
        
        LHS <- do.call("subsy",list(x,jj1))
        RHS <- invert(do.call("subsy",list(invert(x),jj2)))
        expect_true(LHS == RHS, info=dput(x))
    }  

    ## check subsmvp():
    if(length(allvars(x))>0){
        v <- allvars(x)[1]
        expect_true(x == subsmvp(x,v,as.mvp(v)), info=list(dput(x),v))
    }

    ## check subvec():
    if(length(allvars(x))>0){
      LHS <- subvec(x,a=1,b=1,c=2,d=1,e=1:3,f=1)
      RHS <- c(
          subs(x,a=1,b=1,c=2,d=1,e=1,f=1),
          subs(x,a=1,b=1,c=2,d=1,e=2,f=1),
          subs(x,a=1,b=1,c=2,d=1,e=3,f=1)
      )
      expect_true(all(LHS==RHS))
    }
    
    ## check d(X^n)/dt = nX^(n-1)dX/dt:
    if(length(allvars(x))>0){
        v <- allvars(x)[1]
        for(i in seq_len(5)){
            expect_true(deriv(x^i,v) == i*x^(i-1)*deriv(x,v), info=list(dput(x),v))
        }
    }
    
    ## check d^2X/dudv = d^2X/dvdu: 
    if(length(allvars(x))>1){
        v <- allvars(x)[1:2]
        expect_true(deriv(x,v) == deriv(x,rev(v)), info=list(dput(x),v))
    }

  ## check the chain rule, here dx/dv = (dx/dy)*(dy/dv):
  if((length(allvars(x))>1)   & (! "y" %in% allvars(x)) ){

    v <- allvars(x)[1]
    s <- as.mvp("1  +  y  -  y^2 zz  +  y^3 z^2")
    
    LHS <- subsmvp(deriv(x,v)*deriv(s,"y"),v,s)
    RHS <- deriv(subsmvp(x,v,s),"y")
    expect_true(LHS == RHS,info=list(LHS,RHS))
}
  return(TRUE)
}  # checker1() closes


checker2 <- function(x,y){
  stopifnot(x == -y+x+y)
  stopifnot(x+y == x-(-y))

  stopifnot(x+y == y+x)

  stopifnot((-x)*y == -(x*y))
  stopifnot(x*(-y) == -(x*y))

  stopifnot(x*y == y*x)


  stopifnot(x^2-y^2 == (x+y)*(x-y))  # not as obvious as it looks
  
  stopifnot(x^3+y^3 == (x+y)*(x^2-x*y+y^2))  # ditto
  stopifnot(x^3-y^3 == (x-y)*(x^2+x*y+y^2))

  
  ## check product rule for differentiation:
  if(length(allvars(x))>1){
    v <- allvars(x)[1]
    stopifnot(deriv(x*y,v) == x*deriv(y,v) + deriv(x,v)*y)
  }
  return(TRUE)
}

checker3 <- function(x,y,z){
  stopifnot(x+(y+z) == (x+y)+z) # associativity
  stopifnot(x*(y*z) == (x*y)*z) # associativity

  stopifnot(x*(y+z) == x*y + x*z)  # distributivity
  stopifnot((y+z)*x == x*y + x*z)  # distributivity

  ## check the product rule for triples:
  if(length(allvars(x))>1){
    v <- allvars(x)[1]
    stopifnot(deriv(x*y*z,v) ==
              deriv(x,v)*y*z + 
              x*deriv(y,v)*z + 
              x*y*deriv(z,v)
              )
  }
  # factorization:
  stopifnot(
  (x+y+z)*(x^2+y^2+z^2-x*y-y*z-x*z) == x^3+y^3+z^3-3*x*y*z
  )

  
  return(TRUE)
} # checker3() closes


for(i in 1:2){
    x <- rmvp(5)
    y <- rmvp(5)
    z <- rmvp(5)
    
    checker1(x)
    checker2(x,y)
    checker3(x,y,z)
}
})
