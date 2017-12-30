#include <Rcpp.h>
#include <sstream>
using namespace Rcpp;

// concatenate string vectors
std::vector<std::string> scat(std::vector<std::string> x,
                              std::vector<std::string> y) {
  std::vector<std::string> z(x.size() + y.size());
  std::copy(x.begin(), x.end(), z.begin());
  std::copy(y.begin(), y.end(), z.begin() + x.size());
  return z;
}

// concatenate two numeric vectors (as c() in R)
NumericVector vcat(const NumericVector& x, const NumericVector& y) {
  NumericVector z(x.size() + y.size());

  std::copy(x.begin(), x.end(), z.begin());
  std::copy(y.begin(), y.end(), z.begin() + x.size());

  z.names() = scat(x.names(), y.names());

  return z;
}

// Vectorized equality operator for string vs Character vector
LogicalVector vecstreq(const CharacterVector& vec, std::string str) {
  LogicalVector res(vec.size());

  for (int i = 0; i < vec.size(); i++) {
    res[i] = vec[i] == str;
  }

  return res;
}
