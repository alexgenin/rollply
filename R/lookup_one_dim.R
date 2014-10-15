# 
# 

.lookup_one_dim <- function(current_coords,coords,wdw) {
  
  return( sqrt( (coords - current_coords[[1]] )^2) < wdw )
  
}

