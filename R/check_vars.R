# 
# 
# 
# Checks variables in input data.frame and/or grid.
# 


check_args <- function(.rollvars, 
                       .data, 
                       grid, 
                       grid.type) { 
  
  # Check for proper grid type
  if (!exists(paste0('build_grid_',grid.type))) 
    stop('Unknown grid type. See ?build.grid for a list of supported grid types')
  
  # Check if the grid is a named data.frame
  if ( !is.null(grid) && 
      (is.null(names(grid)) || !is.data.frame(grid)) )
    stop('The supplied grid must be a named data.frame')
    
}
