## special polynomials: zero, constant, homog, etc.  Names consistent
## with the spray package as far as possible, but not all of spray's
## special polynomials make sense here, and some of them need a
## different approach.



#     product(1:3)    #      x * y^2 * z^3
#     homog(3)        #      x + y + z
#     homog(3,2)      #      x^2  + xy + xz + y^2 + yz + z^2
#     linear(1:3)     #      1*x + 2*y + 3*z
#     linear(1:3,2)   #      1*x^2 + 2*y^2 + 3*z^2
#     lone(3)         #      z
#     lone(2,3)       #      y
#     one(3)          #      1
#     xyz(3)          #      xyz
     


`product` <- function(v,symbols=letters){
    if(length(v)>length(symbols)){stop("not enough symbols")}
    mvp(list(symbols[seq_along(v)]),list(v),1)
}

`homog` <- function(d,power=1,symbols=letters){
    if(d>length(symbols)){stop("not enough symbols")}
    jj <- partitions::compositions(power,d)
    mvp(
        apply(jj,2,function(x){symbols[which(x!=0)]}),
        apply(jj,2,function(x){x[x!=0]}),
        rep(1,ncol(jj))
    )
}

`linear` <- function(x,power=1,symbols=letters){
    if(length(x)>length(symbols)){stop("not enough symbols")}
    mvp(as.list(symbols[seq_along(x)]),rep(power,length(x)),x)
}

`numeric_to_mvp` <- function(x){
    stopifnot(length(x)==1)
    stopifnot(is.numeric(x))
    return(mvp(list(character(0)),list(integer(0)),x))
}


`xyz` <- function(n,symbols=letters){
    if(n>length(symbols)){stop("not enough symbols")}
    mvp(list(symbols[seq_len(n)]), list(rep(1,n)), 1)
}


## constant() defined in skimpy.R


## Generating function for d-dimensional knight

`knight` <- function(d,can_stay_still=FALSE){
  f <- function(d){
    n <- d * (d - 1)
    out <- matrix(0, n, d)
    out[cbind(rep(seq_len(n), each=2), c(t(which(diag(d)==0, arr.ind=TRUE))))] <- seq_len(2)
    out <- rbind(out, -out, `[<-`(out, out==1, -1),`[<-`(out, out==2, -2))
    out <- split(out,row(out))
    out <- lapply(out,function(x){names(x) <- letters[seq_along(x)] ; return(x)})
    list(
        lapply(out,function(x){names(x[x!=0])}),
        lapply(out,function(x){x[x!=0]})
    )
  }
  jj <- f(d)
  out <- mvp(
      jj[[1]],
      jj[[2]],
      rep(1,length(jj[[1]]))
  )
  if(can_stay_still){
    out <- 1+out
  }
  return(out)
}

