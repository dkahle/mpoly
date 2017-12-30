#ifndef mpoly_ADD_H
#define mpoly_ADD_H

using namespace Rcpp;

// concatenate string vectors
std::vector<std::string> scat(std::vector<std::string> x,
                              std::vector<std::string> y);

// concatenate two numeric vectors (as c() in R)
NumericVector vcat(const NumericVector& x, const NumericVector& y);

// helper function to convert types to string
template <typename T>
std::string to_string(T const& value) {
  std::stringstream sstr;
  sstr << value;
  return sstr.str();
};

// Vectorized equality operator for string vs Character vector
LogicalVector vecstreq(const CharacterVector& vec, std::string str);

#endif
