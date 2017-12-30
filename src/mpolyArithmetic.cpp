#include <Rcpp.h>
#include <helpers.h>
using namespace Rcpp;


// [[Rcpp::export]]
List mpolyMult(const List& e1, const List& e2) {
  List res(e1.size() * e2.size());
  int c = 0;
  for (int i = 0; i < e1.size(); i++) {
    NumericVector v = e1[i];
    for (int j = 0; j < e2.size(); j++){
      NumericVector z = e2[j];
      res[c] = vcat(v, z);
      c++;
    }
  }
  return res;
}

// [[Rcpp::export]]
List mpolyPow(const List& e1, int e2) {
  List res = List::create(NumericVector::create(Named("coef", 1)));
  for (int i = 0; i < e2; i++) {
    res = mpolyMult(res, e1);
  }
  return res;
}
