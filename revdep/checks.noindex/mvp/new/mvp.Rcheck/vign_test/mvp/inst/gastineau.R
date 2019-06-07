## Following example taken from:


## M. Gastineau and J. Laskar, 2006.  "Development of TRIP: Fast
## Sparse Multivariate Polynomial Multiplication Usig Burst Tries" In
## V. N. Alexandrov et al. (Eds.): ICCS 2006, Part II, LNCS 3992,
## pp446-453, 2006.

## These authors consider p*(p+1) /. {p -> (1+x+y+z+t)^16 using
## different systems such as pari/gp.


library(skimpy)

f <- as.mvp("p+p^2")
u <- as.mvp("1+x+y+z+t")
u16 <- u^16

system.time(ignore <- subsmvp(f,"p",u16))  # about 30 seconds on my mac




## Compare spray, which is faster than the fastest of Gastineau:
p <- (1+spray(diag(4)))^16
system.time(ignore <- p*(p+1))
