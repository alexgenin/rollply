#include <Rcpp.h>
using namespace Rcpp;

// Looks up data comprised in the rolling window.
//

// [[Rcpp::export]]
LogicalVector lookup_in_wdw(NumericMatrix current_coords, 
                            NumericMatrix coords,
                            double wdw) {
  int nrow = coords.nrow(), ncol = coords.ncol();
  LogicalVector in_wdw(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double curdist = 0;
    for (int j = 0; j < ncol; j++) {
      curdist += pow(coords(i, j) - current_coords(j, 0), 2.0);
    }
    curdist = sqrt(curdist);
    if (curdist <= wdw) {
      in_wdw[i] = 1;
    } else {
      in_wdw[i] = 0;
    }
  }
  return in_wdw;
}

