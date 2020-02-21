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
### Title: Arithmetic Ops Group Methods for 'mvp' objects
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
f(a=1,b=2,c=3)             # coerces to a scalar
f(a=1,b=2,c=3,lose=FALSE)  # formal mvp object



cleanEx()
nameEx("constant")
### * constant

flush(stderr()); flush(stdout())

### Name: constant
### Title: The constant term
### Aliases: constant constant is.constant constant<- constant.mvp
###   constant<-.mvp constant.numeric

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
### Title: Differentiation of 'mvp' objects
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
nameEx("horner")
### * horner

flush(stderr()); flush(stdout())

### Name: horner
### Title: Horner's method
### Aliases: horner
### Keywords: symbolmath

### ** Examples

horner("x",1:5)
horner("x+y",1:3)

w <- as.mvp("x+y^2")
stopifnot(1 + 2*w + 3*w^2 == horner(w,1:3))  # note off-by-one issue

"x+y+x*y" %>% horner(1:3) %>% horner(1:2)



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
nameEx("lose")
### * lose

flush(stderr()); flush(stdout())

### Name: lose
### Title: Drop empty variables
### Aliases: lose lose.mvp drop

### ** Examples


m1 <- as.mvp("1+bish +bash^2 + bosh^3")
m2 <- as.mvp("bish +bash^2 + bosh^3")

m1-m2         # an mvp object
lose(m1-m2)   # numeric






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
nameEx("mvp-package")
### * mvp-package

flush(stderr()); flush(stdout())

### Name: mvp-package
### Title: Fast Symbolic Multivariate Polynomials
### Aliases: mvp-package
### Keywords: package

### ** Examples

p <- as.mvp("1+x+x*y+x^5")

p + as.mvp("a+b^6")

p^3

subs(p^4,x="a+b^2")
aderiv(p^2,x=4)
horner(p,1:3)



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
nameEx("ooom")
### * ooom

flush(stderr()); flush(stdout())

### Name: ooom
### Title: One over one minus a multivariate polynomial
### Aliases: ooom

### ** Examples

ooom("x",5)
ooom("x",5) * as.mvp("1-x")  # zero through fifth order


ooom("x+y",4)

"x+y" %>% ooom(5) %>% `-`(1) %>% ooom(3)




cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

### Name: print
### Title: Print methods for 'mvp' objects
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
nameEx("series")
### * series

flush(stderr()); flush(stdout())

### Name: series
### Title: Decomposition of multivariate polynomials by powers
### Aliases: series taylor trunc trunc1 print.series mvp_taylor_allvars
###   mvp_taylor_onevar mvp_taylor_onepower_onevar mvp_to_series onevarpow
### Keywords: symbolmath

### ** Examples

trunc(as.mvp("1+x")^6,2)

trunc(as.mvp("1+x+y")^3,2)      # neglects all terms with total power>2
trunc1(as.mvp("1+x+y")^3,x=2) # terms like y^3 are treated as constants

p <- horner("x+y",1:4)

onevarpow(p,x=2)   # coefficient of x^2
onevarpow(p,x=3)   # coefficient of x^3

onevarpow(as.mvp("1+x+x*y^2  + z*y^2*x"),x=1,y=2)

series(rmvp(10),"a")

# Works well with pipes:

f <- function(n){as.mvp(sub('n',n,'1+x^n*y'))}
Reduce(`*`,lapply(1:6,f)) %>% series('y')
Reduce(`*`,lapply(1:6,f)) %>% series('x')


p %>% trunc(2)
p %>% trunc1(x=2)
(p %>% subs(x="x+dx") -p) %>% trunc1(dx=2)


## Third order taylor expansion of f(x)=sin(x+y) for x=1.1, about x=1:
sinxpy <- horner("x+y",c(0,1,0,-1/6,0,+1/120,0,-1/5040,0,1/362880))  # sin(x+y)
dx <- as.mvp("dx")
t3 <- sinxpy  + aderiv(sinxpy,x=1)*dx + aderiv(sinxpy,x=2)*dx^2/2 + aderiv(sinxpy,x=3)*dx^3/6
t3 %<>% subs(x=1,dx=0.1)  # t3 = Taylor expansion of sin(y+1.1)
t3 %>% subs(y=0.3)  - sin(1.4)  # numeric; should be small



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
### Aliases: subs subsy subvec substitute subsmvp subs_mvp mvp_subs_mvp
###   varchange varchange_formal namechanger
### Keywords: symbolmath

### ** Examples

p <- rmvp(6,2,2,letters[1:3])
p
subs(p,a=1)
subs(p,a=1,b=2)

subs(p,a="1+b x^3",b="1-y")
subs(p,a=1,b=2,c=3,lose=FALSE)

do.call(subs,c(list(as.mvp("z")),rep(c(z="C+z^2"),5)))

subvec(p,a=1,b=2,c=1:5)   # supply a named list of vectors

M <- matrix(sample(1:3,26*3,replace=TRUE),ncol=26)
colnames(M) <- letters
rownames(M) <- c("Huey", "Dewie", "Louie")
subvec(kahle(r=3,p=1:3),M)  # supply a matrix

varchange(as.mvp("1+x+xy + x*y"),x="newx") # variable xy unchanged

kahle(5,3,1:3) %>% subs(a="a + delta")

pnew <- varchange(p,a="]")  # nonstandard variable names OK
p111 <- varchange_formal(p,"\\]","a")




cleanEx()
nameEx("summary")
### * summary

flush(stderr()); flush(stdout())

### Name: summary
### Title: Summary methods for mvp objects
### Aliases: summary nterms summary.mvp print.summary.mvp rtypical
### Keywords: math

### ** Examples

summary(rmvp(40))
rtypical(rmvp(1000))



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
