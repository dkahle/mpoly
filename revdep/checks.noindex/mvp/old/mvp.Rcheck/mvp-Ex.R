pkgname <- "mvp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('mvp')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Ops.mvp")
### * Ops.mvp

flush(stderr()); flush(stdout())

### Name: Ops.mvp
### Title: Arithmetic Ops Group Methods for mvp objects
### Aliases: Ops.mvp Ops mvp_negative mvp_times_mvp mvp_times_scalar
###   mvp_plus_mvp mvp_plus_numeric mvp_plus_scalar mvp_power_scalar
###   mvp_eq_mvp
### Keywords: symbolmath

### ** Examples

p1 <- rmvp(3)
p2 <- rmvp(3)

p1*p2

p1+p2

p1^3


p1*(p1+p2) == p1^2+p1*p2  # should be TRUE




cleanEx()
nameEx("accessor")
### * accessor

flush(stderr()); flush(stdout())

### Name: accessor
### Title: Accessor methods for mvp objects
### Aliases: accessors vars powers coeffs coeffs<- coeffs<-.mvp
###   coefficients

### ** Examples

a <- rmvp(5)
vars(a)
powers(a)
coeffs(a)

coeffs(a) <- 1  # A simpler object
coeffs(a) <- 0  # The zero polynomial




cleanEx()
nameEx("allvars")
### * allvars

flush(stderr()); flush(stdout())

### Name: allvars
### Title: All variables in a multivariate polynomial
### Aliases: allvars
### Keywords: symbolmath

### ** Examples

p <- rmvp(5)
allvars(p)



cleanEx()
nameEx("as.function")
### * as.function

flush(stderr()); flush(stdout())

### Name: as.function.mvp
### Title: Functional form for multivariate polynomials
### Aliases: as.function.mvp

### ** Examples


p <- as.mvp("1+a^2 + a*b^2 + c")
p
f <- as.function(p)

f(a=1)
f(a=1,b=2)
f(a=1,b=2,c=3)
f(a=1,b=2,c=3,drop=FALSE)



cleanEx()
nameEx("constant")
### * constant

flush(stderr()); flush(stdout())

### Name: constant
### Title: The constant term
### Aliases: constant constant constant<- constant.mvp constant<-.mvp
###   constant.numeric

### ** Examples

a <- rmvp(5)+4
constant(a)
constant(a) <- 33
a

constant(0)  # the zero mvp



cleanEx()
nameEx("deriv")
### * deriv

flush(stderr()); flush(stdout())

### Name: deriv
### Title: Differentiation of mvp objects
### Aliases: deriv aderiv deriv.mvp deriv_mvp aderiv.mvp aderiv_mvp
### Keywords: symbolmath

### ** Examples

p <- rmvp(10,9,9,letters[1:4])
deriv(p,letters[1:3])
deriv(p,rev(letters[1:3]))  # should be the same

aderiv(p,a=1,b=2,c=1)

## verify the chain rule:
x <- rmvp(7,symbols=6)
v <- allvars(x)[1]
s <- as.mvp("1  +  y  -  y^2 zz  +  y^3 z^2")
LHS <- subsmvp(deriv(x,v)*deriv(s,"y"),v,s)   # dx/ds*ds/dy
RHS <- deriv(subsmvp(x,v,s),"y")              # dx/dy

LHS - RHS # should be zero




cleanEx()
nameEx("drop")
### * drop

flush(stderr()); flush(stdout())

### Name: drop
### Title: Drop empty variables
### Aliases: drop

### ** Examples


m1 <- as.mvp("1+bish +bash^2 + bosh^3")
m2 <- as.mvp("bish +bash^2 + bosh^3")

m1-m2         # an mvp object
drop(m1-m2)   # numeric



cleanEx()
nameEx("invert")
### * invert

flush(stderr()); flush(stdout())

### Name: invert
### Title: Replace symbols with their reciprocals
### Aliases: invert

### ** Examples

invert("x")

invert(rmvp(10,7,7,letters[1:3]),"a")




cleanEx()
nameEx("kahle")
### * kahle

flush(stderr()); flush(stdout())

### Name: kahle
### Title: A sparse multivariate polynomial
### Aliases: kahle
### Keywords: symbolmath

### ** Examples

kahle()  # a+b+...+z
kahle(r=2,p=1:2)  # Kahle's original example

## example where mvp runs faster than spray (mvp does not need a 200x200 matrix):
k <- kahle(200,r=3,p=1:3,symbols=paste("x",sprintf("%02d",1:200),sep=""))
system.time(ignore <- k^2)
#system.time(ignore <- mvp_to_spray(k)^2)   # needs spray package loaded



cleanEx()
nameEx("knight")
### * knight

flush(stderr()); flush(stdout())

### Name: knight
### Title: Chess knight
### Aliases: knight_mvp knight
### Keywords: symbolmath

### ** Examples


knight(2)      # regular chess knight on a regular chess board
knight(2,TRUE) # regular chess knight that can stay still

# Q: how many ways are there for a 4D knight to return to its starting
# square after four moves?

# A:
constant(knight(4)^4)

# Q ...and how many ways in four moves or fewer?

# A1:
constant(knight(4,TRUE)^4)

# A2:
constant((1+knight(4))^4)




cleanEx()
nameEx("mpoly")
### * mpoly

flush(stderr()); flush(stdout())

### Name: mpoly
### Title: Conversion to and from mpoly form
### Aliases: mpoly mpoly_to_mvp mvp_to_mpoly as.mpoly.mvp
### Keywords: symbolmath

### ** Examples


x <- rmvp(5)

x == mpoly_to_mvp(mpoly::as.mpoly(x))        # should be TRUE



cleanEx()
nameEx("mvp")
### * mvp

flush(stderr()); flush(stdout())

### Name: mvp
### Title: Multivariate polynomials, mvp objects
### Aliases: mvp is.mvp as.mvp is_ok_mvp

### ** Examples


mvp(list("x" , c("x","y"), "a",c("y","x")),list(1,1:2,3,c(-1,4)),1:4)

## Note how the terms appear in an arbitrary order, as do
## the symbols within a term.


kahle  <- mvp(
    vars   = split(cbind(letters,letters[c(26,1:25)]),rep(seq_len(26),each=2)),
    powers = rep(list(1:2),26),
    coeffs = 1:26
)

## again note arbitrary order of terms and symbols within a term







cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: print
### Title: Print methods for mvp objects
### Aliases: print.mvp print_mvp
### Keywords: symbolmath

### ** Examples


a <- rmvp(4)
a
print(a)
print(a,stars=TRUE)
print(a,varorder=rev(letters))



cleanEx()
nameEx("rmvp")
### * rmvp

flush(stderr()); flush(stdout())

### Name: rmvp
### Title: Random multivariate polynomials
### Aliases: rmvp

### ** Examples

rmvp(5)
rmvp(5,symbols=state.abb)



cleanEx()
nameEx("special")
### * special

flush(stderr()); flush(stdout())

### Name: special
### Title: Various functions to create simple multivariate polynomials
### Aliases: special product homog linear xyz numeric_to_mvp
### Keywords: symbolmath

### ** Examples

product(1:3)        #      a * b^2 * c^3
homog(3)            #      a + b + c
homog(3,2)          #      a^2  + a b + a c + b^2 + b c + c^2
linear(1:3)         #      1*a + 2*b + 3*c
constant(5)         #      5
xyz(5)              #      a*b*c*d*e



cleanEx()
nameEx("spray")
### * spray

flush(stderr()); flush(stdout())

### Name: spray
### Title: Spray functionality
### Aliases: spray spray_to_mvp mvp_to_spray
### Keywords: symbolmath

### ** Examples


mvp_to_spray(rmvp(5))
spray_to_mvp(spray::spray(diag(6),1:6))



cleanEx()
nameEx("subs")
### * subs

flush(stderr()); flush(stdout())

### Name: subs
### Title: Substitution
### Aliases: subs subsy substitute subsmvp subs_mvp mvp_subs_mvp
### Keywords: symbolmath

### ** Examples

p <- rmvp(6,2,2,letters[1:3])
p
subs(p,a=1)
subs(p,a=1,b=2)

subs(p,a="1+b x^3",b="1-y")
subs(p,a=1,b=2,c=3,drop=FALSE)

do.call(subs,c(list(as.mvp("z")),rep(c(z="C+z^2"),5)))




cleanEx()
nameEx("zero")
### * zero

flush(stderr()); flush(stdout())

### Name: zero
### Title: The zero polynomial
### Aliases: zero is.zero
### Keywords: symbolmath

### ** Examples


constant(0)

t1 <- as.mvp("x+y")
t2 <- as.mvp("x-y")

stopifnot(is.zero(t1*t2-as.mvp("x^2-y^2")))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
