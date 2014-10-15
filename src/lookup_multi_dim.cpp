#include <Rcpp.h>
using namespace Rcpp;

// Looks up data comprised within our rolling window.
//
// [[Rcpp::export]]
LogicalVector lookup_multi_dim(NumericMatrix current_coords, 
                               NumericMatrix coords,
                               double wdw) {
  int nrow = coords.nrow(), ncol = coords.ncol();
  LogicalVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double curdist = 0;
    for (int j = 0; j < ncol; j++) {
      curdist += pow(coords(i, j) - current_coords(j,0), 2.0);
    }
    curdist = sqrt(curdist);
    if (curdist <= wdw) {
      out[i] = 1;
    } else {
      out[i] = 0;
    }
  }
  return out;
}

