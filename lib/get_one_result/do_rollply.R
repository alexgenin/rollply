# Do the actual job of computing the result
# 

do_rollply <- function(current_coords,   # the coordinates needs to be the first arg
                       coords,
                       wdw.size,      # wdw size
                       .data,    # 
                       fun,      # 
                       lookup_data_in_wdw,  # function that gives useful indexes in .data
                       ...) {
  
  # Get subset of values required
  # Note: current_coords is converted within dapply into a df (why?)
  lookup <- lookup_data_in_wdw(as.matrix(current_coords), 
                               coords, 
                               wdw.size)
  
  # Return function applied to that subset
  fun(.data[lookup, ],...)
}


