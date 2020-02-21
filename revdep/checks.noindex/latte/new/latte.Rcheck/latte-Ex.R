pkgname <- "latte"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('latte')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("genmodel")
### * genmodel

flush(stderr()); flush(stdout())

### Name: genmodel
### Title: Generate a configuration matrix
### Aliases: genmodel

### ** Examples


if (has_4ti2()) {

varlvls <- rep(2, 2)
facets <- list(1, 2)
genmodel(varlvls, facets)
genmodel(varlvls, facets, quiet = FALSE)

varlvls <- rep(3, 3)
facets <- list(1:2, 2:3, c(3,1))
genmodel(varlvls, facets)

# compare this to algstat's hmat function

}




cleanEx()
nameEx("kprod")
### * kprod

flush(stderr()); flush(stdout())

### Name: kprod
### Title: Iterated Kronecker product
### Aliases: kprod

### ** Examples


kprod(diag(2), t(ones(2)))
kprod(t(ones(2)), diag(2))


kprod(diag(2), t(ones(2)), t(ones(2)))
kprod(t(ones(2)), diag(2), t(ones(2)))
kprod(t(ones(2)), t(ones(2)), diag(2))


# cf. aoki, hara, and takemura p.13
rbind(
  kprod(diag(2), t(ones(2))),
  kprod(t(ones(2)), diag(2))
) 





cleanEx()
nameEx("latte-count")
### * latte-count

flush(stderr()); flush(stdout())

### Name: latte-count
### Title: Count integer points in a polytope
### Aliases: latte-count count_core latte_count latte_fcount

### ** Examples


if (has_latte()) {

spec <- c("x + y <= 10", "x >= 1", "y >= 1")
latte_count(spec) # 45
latte_count(spec, quiet = FALSE) # 45
latte_count(spec, dilation = 10) # 3321
latte_count(spec, homog = TRUE) # 45

# by default, the output from LattE is in
list.files(tempdir())
list.files(tempdir(), recursive = TRUE)

# ehrhart polynomials
latte_count(spec, ehrhart_polynomial = TRUE)
latte_count(spec, ehrhart_polynomial = TRUE, mpoly = FALSE)

# ehrhart series (raw since mpoly can't handle rational functions)
latte_count(spec, ehrhart_series = TRUE)

# simplified ehrhart series - not yet implemented
#latte_count(spec, simplified_ehrhart_polynomial = TRUE)

# first terms of the ehrhart series
latte_count(spec, ehrhart_taylor = 1)
latte_count(spec, ehrhart_taylor = 2)
latte_count(spec, ehrhart_taylor = 3)
latte_count(spec, ehrhart_taylor = 4)

# multivariate generating function
latte_count(spec, multivariate_generating_function = TRUE)


# by vertices
spec <- list(c(1,1), c(10,1), c(1,10), c(10,10))
latte_count(spec)
latte_count(spec, vrep = TRUE)

code <- "
5 3
1 -1  0
1  0 -1
1 -1 -1
0  1  0
0  0  1
"
latte_count(code)


# for Ax <= b, see this example from the latte manual p.10
A <- matrix(c(
   1,  0,
   0,  1,
   1,  1,
  -1,  0,
   0, -1
), nrow = 5, byrow = TRUE)
b <- c(1, 1, 1, 0, 0)
latte_count(list(A = A, b = b))









}





cleanEx()
nameEx("latte-files")
### * latte-files

flush(stderr()); flush(stdout())

### Name: latte-files
### Title: Format/read/write a matrix in latte's style
### Aliases: latte-files format_latte write_latte write.latte read_latte
###   read.latte

### ** Examples



(mat <- matrix(sample(9), 3, 3))

format_latte(mat)
cat(format_latte(mat))

(file <- file.path(tempdir(), "foo.hrep"))
write_latte(mat, file)
file.show(file)
read_latte(file)
read_latte(file, "Ab")

attr(mat, "linearity") <- c(1, 3)
attr(mat, "nonnegative") <- 2
mat
format_latte(mat)
cat(format_latte(mat))
write_latte(mat, file)
file.show(file)
read_latte(file)

file.remove(file)






cleanEx()
nameEx("latte-optim")
### * latte-optim

flush(stderr()); flush(stdout())

### Name: latte-optim
### Title: Solve an integer progam with LattE
### Aliases: latte-optim latte_optim latte_max latte_min

### ** Examples



if (has_latte()) {

latte_max(
  "-2 x + 3 y", 
  c("x + y <= 10", "x >= 0", "y >= 0")
)

latte_max(
  "-2 x + 3 y", 
  c("x + y <= 10", "x >= 0", "y >= 0"),
  quiet = FALSE
)


df <- expand.grid("x" = 0:10, "y" = 0:10)
df <- subset(df, x + y <= 10L)
df$objective <- with(df, -2*x + 3*y)
library("ggplot2")
ggplot(df, aes(x, y, size = objective)) +
  geom_point()

latte_min(
  "-2 x + 3 y",
  c("x + y <= 10", "x >= 0", "y >= 0"),
  method = "cones"
)



latte_min("-2 x - 3 y - 4 z", c(
  "3 x + 2 y + z <= 10",
  "2 x + 5 y + 3 z <= 15",
  "x >= 0", "y >= 0", "z >= 0"
), "cones", quiet = FALSE)





}




cleanEx()
nameEx("lattice-bases")
### * lattice-bases

flush(stderr()); flush(stdout())

### Name: lattice-bases
### Title: Compute a basis with 4ti2
### Aliases: lattice-bases basis zbasis markov groebner hilbert graver
###   fzbasis fmarkov fgroebner fhilbert fgraver

### ** Examples



if (has_4ti2()) {


# basic input and output for the 3x3 independence example
(A <- rbind(
  kprod(diag(3), ones(1,3)),
  kprod(ones(1,3), diag(3))
))
markov(A, p = "arb")



# you can get the output formatted in different ways:
markov(A, p = "arb", all = TRUE)
markov(A, p = "arb", "vec")
markov(A, p = "arb", "tab", c(3, 3))
tableau(markov(A, p = "arb"), dim = c(3, 3)) # tableau notation



# you can add options by listing them off
# to see the options available to you by function,
# go to http://www.4ti2.de
markov(A, p = "arb")



# the basis functions are automatically cached for future use.
# (note that it doesn't persist across sessions.)
A <- rbind(
  kprod(  diag(4), ones(1,4), ones(1,4)),
  kprod(ones(1,4),   diag(4), ones(1,4)),
  kprod(ones(1,4), ones(1,4),   diag(4))
)
system.time(markov(A, p = "arb"))
system.time(markov(A, p = "arb"))

# the un-cashed versions begin with an "f"
# (think: "forgetful" markov)
system.time(fmarkov(A, p = "arb"))
system.time(fmarkov(A, p = "arb"))



# you can see the command line code by typing shell = TRUE
# and the standard output wiht quiet = FALSE
# we illustrate these with fmarkov because otherwise it's cached
(A <- rbind(
  kprod(diag(2), ones(1,4)),
  kprod(ones(1,4), diag(2))
))
fmarkov(A, p = "arb", shell = TRUE)
fmarkov(A, p = "arb", quiet = FALSE)



# compare the bases for the 3x3x3 no-three-way interaction model
A <- rbind(
  kprod(  diag(3),   diag(3), ones(1,3)),
  kprod(  diag(3), ones(1,3),   diag(3)),
  kprod(ones(1,3),   diag(3),   diag(3))
)
str(  zbasis(A, p = "arb")) #    8 elements = ncol(A) - qr(A)$rank
str(  markov(A, p = "arb")) #   81 elements
str(groebner(A, p = "arb")) #  110 elements
str(  graver(A))            #  795 elements


# the other bases are also cached
A <- rbind(
  kprod(  diag(3), ones(1,3), ones(1,2)),
  kprod(ones(1,3),   diag(3), ones(1,2)),
  kprod(ones(1,3), ones(1,3),   diag(2))
)
system.time( graver(A))
system.time( graver(A))
system.time(fgraver(A))
system.time(fgraver(A))



# LAS ex 1.2.1, p.12 : 2x3 independence
(A <- rbind(
  kprod(diag(2), ones(1,3)),
  kprod(ones(1,2), diag(3))
))

markov(A, p = "arb", "tab", c(3, 3))
# Prop 1.2.2 says that there should be
2*choose(2, 2)*choose(3,2) # = 6
# moves (up to +-1)
markov(A, p = "arb", "tab", c(3, 3), TRUE)



# LAS example 1.2.12, p.17  (no 3-way interaction)
(A <- rbind(
  kprod(  diag(2),   diag(2), ones(1,2)),
  kprod(  diag(2), ones(1,2),   diag(2)),
  kprod(ones(1,2),   diag(2),   diag(2))
))
plot_matrix(A)
markov(A, p = "arb")
groebner(A, p = "arb")
graver(A)
tableau(markov(A, p = "arb"), dim = c(2,2,2))





# using the markov bases database, must be connected to internet
# commented out for predictable and fast cran checks time
# A <- markov(dbName = "ind3-3")
# B <- markov(rbind(
#   kprod(diag(3), ones(1,3)),
#   kprod(ones(1,3), diag(3))
# ), p = "arb")
# all(A == B)





# possible issues
# markov(diag(1, 10))
# zbasis(diag(1, 10), "vec")
# groebner(diag(1, 10), "vec", all = TRUE)
# graver(diag(1, 10), "vec", all = TRUE)
# graver(diag(1, 4), "tab", all = TRUE, dim = c(2,2))



}





cleanEx()
nameEx("ones")
### * ones

flush(stderr()); flush(stdout())

### Name: ones
### Title: Ones
### Aliases: ones

### ** Examples


ones(5)
ones(5, 1)
ones(1, 5)
ones(2, 3)
ones(2, 3, 2)

str(ones(5))




cleanEx()
nameEx("pathing")
### * pathing

flush(stderr()); flush(stdout())

### Name: pathing
### Title: Set paths to LattE and 4ti2 executables
### Aliases: pathing set_latte_path set_4ti2_path get_4ti2_path
###   get_latte_path has_4ti2 has_latte missing_4ti2_stop
###   missing_latte_stop

### ** Examples



has_4ti2()
if (has_4ti2()) get_4ti2_path()

has_latte()
if (has_4ti2()) get_latte_path()

# these are stored in your .Renviron file; that's where you should put the 
# path to LattE and 4ti2 executables. for example, you should have a lines 
# that look like
# LATTE=/Applications/latte/bin
# 4TI2=/Applications/latte/bin
# you can set these with usethis::edit_r_environ() 

# you can change these in your current session with set_latte_path() and 
# set_4ti2_path(), for example set_4ti2_path("/path/to/4ti2")





cleanEx()
nameEx("plot-matrix")
### * plot-matrix

flush(stderr()); flush(stdout())

### Name: plot-matrix
### Title: Plot a matrix
### Aliases: plot-matrix plot_matrix

### ** Examples


# the no-three-way interaction configuration
(A <- kprod(ones(1,3), diag(3), ones(3)))
plot_matrix(A)


if (has_4ti2()) {

plot_matrix(markov(A))

(A <- genmodel(c(2L, 2L), list(1L, 2L)))
plot_matrix(A)
plot_matrix(markov(A))

(A <- genmodel(c(5L, 5L), list(1L, 2L)))
plot_matrix(A)
plot_matrix(markov(A))

}




cleanEx()
nameEx("ppi")
### * ppi

flush(stderr()); flush(stdout())

### Name: ppi
### Title: Compute the primitive partition identities
### Aliases: ppi

### ** Examples


if (has_4ti2()) {

ppi(3)
t(ppi(3)) %*% 1:3
plot_matrix(ppi(3))

graver(t(1:3))
plot_matrix(graver(t(1:3)))

ppi(5, quiet = FALSE, shell = TRUE)

}





cleanEx()
nameEx("print.tableau")
### * print.tableau

flush(stderr()); flush(stdout())

### Name: print.tableau
### Title: Pretty printing of tableau output.
### Aliases: print.tableau

### ** Examples


# see ?tableau





cleanEx()
nameEx("qsolve")
### * qsolve

flush(stderr()); flush(stdout())

### Name: qsolve
### Title: Solve a linear system over the rationals
### Aliases: qsolve

### ** Examples


if (has_4ti2()) {

# x + y > 0
# x + y < 0

mat <- rbind(
  c( 1,  1),
  c( 1,  1)
)
rel <- c(">", "<")
sign <- c(0, 0)

qsolve(mat, rel, sign, p = "arb")
qsolve(mat, rel, sign, p = "arb", quiet = FALSE)
qsolve(mat, rel, sign, p = "arb", shell = TRUE)

}





cleanEx()
nameEx("tab2vec")
### * tab2vec

flush(stderr()); flush(stdout())

### Name: tab2vec
### Title: Array to vector conversion
### Aliases: tab2vec

### ** Examples


a <- array(1:24, c(2,3,4))
tab2vec(a)

data(Titanic)
tab2vec(Titanic)
Titanic[1,1,1,1]
Titanic[1,1,1,2]





cleanEx()
nameEx("tableau")
### * tableau

flush(stderr()); flush(stdout())

### Name: tableau
### Title: Tableau Notation for Markov
### Aliases: tableau

### ** Examples


vec <- matrix(c(1, -1, -1, 1), nrow = 4)
varlvls <- c(2, 2)
tableau(vec, varlvls)





cleanEx()
nameEx("vec2tab")
### * vec2tab

flush(stderr()); flush(stdout())

### Name: vec2tab
### Title: Vector to array conversion
### Aliases: vec2tab

### ** Examples


data(Titanic)
Titanic
tab2vec(Titanic)
vec2tab(tab2vec(Titanic), dim(Titanic))
vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic
all(vec2tab(tab2vec(Titanic), dim(Titanic)) == Titanic)





cleanEx()
nameEx("zsolve")
### * zsolve

flush(stderr()); flush(stdout())

### Name: zsolve
### Title: Solve a linear system over the integers
### Aliases: zsolve

### ** Examples


if (has_4ti2()) {

mat <- rbind(
  c( 1, -1),
  c(-3,  1),
  c( 1,  1)
)
rel <- c("<", "<", ">")
rhs <- c(2, 1, 1)
sign <- c(0, 1)

zsolve(mat, rel, rhs, sign)
zsolve(mat, rel, rhs, sign, quiet = FALSE)
zsolve(mat, rel, rhs, sign, shell = TRUE)

zsolve(mat, rel, rhs, sign, p = "gmp", quiet = FALSE)

}





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
