#include <Rcpp.h>
#include <sstream>
#include <unordered_map>
#include <helpers.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]


NumericVector combineDegrees(const NumericVector& vec) {
  CharacterVector names = vec.names();
  CharacterVector unames = unique(names);
  NumericVector combined(unames.size(), 0);
  
  combined.names() = unames;
  combined["coef"] = 1;
  
  for (int i = 0; i < vec.size(); i++) {
    if (names[i] == "coef") {
      combined["coef"] = combined["coef"] * vec[i];
    }
    else {
      combined[(std::string)names[i]] = combined[(std::string)names[i]] + vec[i];
    }
  }
  
  // remove zero degree elements
  NumericVector res = combined[ combined != 0 | vecstreq(unames, "coef") ];
  
  return res;
}


// Variable comparison function. A variable is before another if it comes first in vars vector.
// coef is after (not before) everything
struct compareVarOrder {
  compareVarOrder(const CharacterVector& vars) { this->vars = vars; }
  
  bool operator()(std::string var1, std::string var2) const
  {   
    if (var1 == "coef") return false;
    if (var2 == "coef") return true;
    
    for (int i = 0; i < vars.size(); i++) {
      if (vars[i] == var1) return true;
      if (vars[i] == var2) return false;
    }
    return true;
  }
  
  CharacterVector vars;
};


// [[Rcpp::export]]
NumericVector sortVars(const NumericVector& term, const CharacterVector& vars) {
  std::vector<std::string> names = term.names();
  std::sort(names.begin(), names.end(), compareVarOrder(vars));
  
  NumericVector sorted(term.size());
  for (int i = 0; i < term.size(); i++) {
    sorted[i] = term[names[i]];
  }
  
  sorted.names() = names;
  
  return sorted;
}


// [[Rcpp::export]]
CharacterVector getMonomialStrings(const List& poly) {
  CharacterVector res(poly.size());
  
  for (int i = 0; i < poly.size(); i++) {
    NumericVector term = poly[i];
    CharacterVector names = term.names();
    std::string monom = "";
    
    if (term.size() == 1 && names[0] == "coef") {
      monom = "coef";
    }
    else {
      for (int j = 0; j < term.size(); j++) {
        if (!(names[j] == "coef"))
          monom += (std::string)names[j] + to_string(term[j]);
      }
    }
    
    res[i] = monom;
  }
  
  return res;
}


// [[Rcpp::export]]
List combineMonomials(const List& poly) {
  CharacterVector monomials = getMonomialStrings(poly);
  CharacterVector uniqueMonomials = unique(monomials);
  int nUnique = uniqueMonomials.size();
  int nMonom = monomials.size();
  
  if (nMonom == nUnique)
    return poly;
  
  // make a map of (unique) monomial strings and their coefficients
  std::unordered_map<std::string, double> monomCoefMap;
  // and a monomial string to index in monomials vector
  // we need tese indices to be able to pull vector from poly to use as a template
  // and only change its coefficient
  std::unordered_map<std::string, int> monomIndexMap;
  
  // populate the maps with summed coefficient values and indices
  for (int i = 0; i < nMonom; i++) {
    NumericVector term = poly[i];
    monomCoefMap[(std::string)monomials[i]] += term["coef"];
    monomIndexMap[(std::string)monomials[i]] = i;
  }
  
  // resulting list
  List res(nUnique);
  
  // populate resulting list with summed coefficients
  for (int i = 0; i < nUnique; i++) {
    NumericVector templateTerm = poly[ monomIndexMap[(std::string)uniqueMonomials[i]] ];
    templateTerm["coef"] = monomCoefMap[(std::string)uniqueMonomials[i]];
    res[i] = templateTerm;
  }
  
  return res;
}

bool isSubset(CharacterVector sub, CharacterVector sup) {
  for (int i = 0; i < sub.size(); i++) {
    if(std::find(sup.begin(), sup.end(), sub[i]) == sup.end())
      return false;
  }
  return true;
}

// [[Rcpp::export]]
List tidyTerms(const List& poly, const CharacterVector& vars) {
  List res(poly.size());
  
  // static vector which we'll use to add coef to terms that don't have it
  NumericVector coef1vec = NumericVector::create(Named("coef", 1.0));
  
  for (int i = 0; i < poly.size(); i++) {
    NumericVector term = poly[i];
    CharacterVector names = term.names();
    
    // add coef = 1 if no coef present, but vars are
    if (!term.containsElementNamed("coef") && isSubset(names, vars))
      term = vcat(term, coef1vec);
    
    // merge same variables by adding up their degrees
    term = combineDegrees(term);
    
    // sort the variables
    term = sortVars(term, vars);
    
    res[i] = term;
  }
  
  return res;
}

// [[Rcpp::export]]
List removeZeros(const List& poly){
  // to avoid expanding the list, we'll first check how many zeros there are
  // and then proceed to populate the resulting list without zero elements
  int nZeros = 0;
  
  for (int i = 0; i < poly.size(); i++) {
    NumericVector term = poly[i];
    if (term["coef"] == 0.0) {
      nZeros++;
    }
  }
  
  if (nZeros == 0)
    return poly;
  
  List res(poly.size() - nZeros);
  
  int j = 0;
  for (int i = 0; i < poly.size(); i++) {
    NumericVector term = poly[i];
    if (term["coef"]  != 0) {
      res[j] = term;
      j++;
    }
  }
  
  return res;
}


// [[Rcpp::export]]
List reducePoly(const List& poly, const CharacterVector& vars){
  // tidy up the terms in the poly list
  List tidyPoly = tidyTerms(poly, vars);
  
  // combine the monomials in the polynomial, i.e. add up same monomials
  List combinedPoly = combineMonomials(tidyPoly);
  
  // remove elements with zero coefficients
  List reducedPoly = removeZeros(combinedPoly);
  
  // if everything is removed, we have a zero polynomial and thus return that
  if (reducedPoly.size() == 0) {
    reducedPoly = List::create(NumericVector::create(Named("coef", 0.0)));
  }
  
  return reducedPoly;
}
