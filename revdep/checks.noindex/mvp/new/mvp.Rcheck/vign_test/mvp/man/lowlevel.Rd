\name{lowlevel}
\alias{lowlevel}
\alias{mvp_substitute_mvp}
\alias{mvp_substitute}
\alias{mvp_prod}
\alias{mvp_add}
\alias{mvp_power}
\alias{mvp_deriv}
\alias{simplify}
\title{Low level functions}
\description{Various low-level functions that call the C routines}
\usage{
mvp_substitute(allnames,allpowers,coefficients,v,values)
mvp_substitute_mvp(allnames1, allpowers1, coefficients1, allnames2, allpowers2, 
    coefficients2, v)
mvp_prod(allnames1,allpowers1,coefficients1,allnames2,allpowers2,coefficients2)
mvp_add(allnames1, allpowers1, coefficients1, allnames2, allpowers2,coefficients2)
simplify(allnames,allpowers,coefficients)
mvp_deriv(allnames, allpowers, coefficients, v)
mvp_power(allnames, allpowers, coefficients, n) 
}
\arguments{
  \item{allnames,allpowers,coefficients,allnames1,allpowers1,coefficients1,allnames2,allpowers2,coefficients2,v,values,n}{Variables sent to the C routines}
}
\details{These functions call the functions defined in \code{RcppExports.R}}
\author{Robin K. S.  Hankin}
\note{These functions are not intended for the end-user.  Use the
  syntatic sugar (as in \code{a+b} or \code{a*b} or \code{a^n}), or
  functions like \code{mvp_plus_mvp()}, which are more user-friendly}
\keyword{symbolmath}
