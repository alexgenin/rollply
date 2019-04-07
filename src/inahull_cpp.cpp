// 
// 
// This function is an alternative to alphahull::inahull designed 
//   for speed. 
// 

#include <Rcpp.h>
#include <stdlib.h>

// Define column names of $complement in hull object
#define COMP_C1 0 // 
#define COMP_C2 1 // 
#define COMP_R 2 // third column in complement matrix (with indexing difference)

// Relaxed floating-point equality
#define feq(x,y) ( std::fabs(x - y) < 1e-10 ? true : false)

using namespace Rcpp;

// This is a c++ implementation of alphahull::inahull
//[[Rcpp::export]]
LogicalVector inahull_cpp_multiple(List ahull_obj, 
                                   NumericVector X, 
                                   NumericVector Y) { 
  
  NumericMatrix comp = as<NumericMatrix>(ahull_obj["complement"]);
  LogicalVector inhull = LogicalVector(X.length());
  
  
  
  // Get list of balls/halfplanes
  NumericVector halfpl = NumericVector(comp.nrow());
  NumericVector ball   = NumericVector(comp.nrow());
  int n_halfpl = 0;
  int n_ball = 0;
  for (int i=0; i<comp.nrow(); i++) { 
    
    if ( comp(i, COMP_R) < 0 ) { 
      halfpl(n_halfpl) = i;
      n_halfpl++;
    }
    if ( comp(i, COMP_R) > 0 ) {
      ball(n_ball) = i; 
      n_ball++;
    }
  }
  
  // For all points in provided vectors
  for (int pt = 0; pt < X.length(); pt++) { 
    
    bool in_compl = false;
    
    int current = 0;
    
    // Scan halfpls and check whether the point falls in them
    while ( !in_compl && current < n_halfpl ) { 
      double sig = comp(halfpl(current), COMP_R); 
      double a   = comp(halfpl(current), COMP_C1); 
      double b   = comp(halfpl(current), COMP_C2); 
      if ( sig < -3 || feq(sig, -3) ) { 
        if ( ( X(pt) > a && feq(sig, -3) ) || ( X(pt) < a && feq(sig, -4) ) ) { 
          in_compl = true; 
        }
      } else {
        if ( ( Y(pt) > a + b * X(pt) && feq(sig, -1) ) || 
             ( Y(pt) < a + b * X(pt) && feq(sig, -2) ) ) { 
          in_compl = true;
        }
      }
    current++;
    }
    
    
    
    // Scan balls and check whether the points fall in them
    current = 0;
    while ( !in_compl && current < n_ball ) { 
      double r  = comp(ball(current), COMP_R); 
      double c1 = comp(ball(current), COMP_C1); 
      double c2 = comp(ball(current), COMP_C2); 
      double d = sqrt( pow(X(pt) - c1, 2) + pow(Y(pt) - c2, 2) );
      
      if (d < r) { 
        in_compl = true;
      }
      
      current++;
    }
    
  
    // return whether it is in the hull => not in the complement
    inhull(pt) = !in_compl;
  }
  
  return inhull; 
}

