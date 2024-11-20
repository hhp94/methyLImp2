#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector apply_paste0(const LogicalMatrix& mat) {
  int ncol = mat.ncol();
  int nrow = mat.nrow();
  CharacterVector result(ncol);
  std::vector<std::string> strings(ncol);
  
  for (auto& s : strings) {
    s.reserve(nrow);
  }
  
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      strings[j] += mat(i, j) ? '1' : '0';
    }
  }
  
  for (int j = 0; j < ncol; ++j) {
    result[j] = strings[j];
  }
  
  return result;
}
