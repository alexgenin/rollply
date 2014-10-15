# 
# 

lookup_one_dim <- function(current_coords,coords,wdw) {
  
  return( sqrt( (coords - current_coords[[1]] )^2) < wdw )
  
}

# cppFunction('
# NumericVector lookup_one_dim(NumericMatrix current_coords, 
#                                NumericMatrix coords,
#                                double wdw) {
#   
#   int nrow = coords.nrow(), ncol = coords.ncol();
#   NumericVector out(nrow);
#   
#   /* We interate on output vector items == rows of coords */
#   for (int i = 0; i < nrow; i++) {
#     out[i] = sqrt( pow( coords(i,1) - current_coords(1,1), 2.0 ) );
#   }
#   return out;
# }')
