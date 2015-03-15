# 
# 
# 
# Checks variables in input data.frame and/or grid.
# 

check_coords <- function(coords, grid) { 
  
  # Check if all coordinates involved are numeric
  all_numeric <- all(sapply(coords, is.numeric))
  if (!is.null(grid)) { 
    all_numeric <- all_numeric & all(sapply(coords, is.numeric))
  }
  
  if ( ! all_numeric ) { 
    stop("Can not work with non-numeric coordinates.")
  }
  
  # Check for the presence of NA's
  na_in_coords <- apply(coords, 2, function(X) any(is.na(X)))
  if (!is.null(grid)) {
    na_in_grid   <- apply(grid, 2, function(X) any(is.na(X)))
  } else { 
    na_in_grid <- FALSE
  }
  
  # We do not remove NAs as this would create a copy of 
  # a potentially big dataset.
  if (na_in_coords || na_in_grid) { 
    stop('NA in moving window parameters are not supported.',
         'Try removing them beforehand.')
  }
  
}

check_args <- function(.rollvars, 
                       .data, 
                       grid, 
                       grid.type) { 
  
  # Checks for proper grid type
  if ( ! exists(paste0('build_grid_',grid.type)) ) {
    stop('Unknown grid type. See ?build_grid for a list of supported grid types')
  }
  
  if (  ! is_mat_or_df(.data) ) {
    stop("I do not know what to do with argument .data of class ", class(.data))
  }
  
  # If a grid object was passed
  if ( ! is.null(grid) ) { 
    
    # Rollply accepts matrices as grid, too
    if ( ! is_mat_or_df(grid) ) { 
      stop("I do not know what to do with argument grid of class ", class(grid))
    }
    
    # Need colnames !
    if ( is.null(colnames(grid)) ) { 
      stop("Grid column names should be defined")
    }
    
  }
  
  # Check if the names provided in the formula are present in .data and grid
  formnames <- unlist(Filter(function(str) { ! any(str == "NA") },
                             lapply(.rollvars, names))) # this should be moved to formulr
  names_are_in(colnames(.data), formnames, "not found in provided .data", stop)
  
  # Check if formnames are present in grid names too
  if (!is.null(grid)) { 
    names_are_in(colnames(grid), formnames, "not found in provided grid", stop)
  }
  
}

# Reports if one name is not found in another bunch of names
names_are_in <- function(obj.names, vars, error.suffix, error.fun) { 
  for (var in vars) { 
    if ( ! var %in% obj.names ) error.fun(paste(var, error.suffix))
  }
}

is_mat_or_df <- function(obj) { 
  inherits(obj,'matrix') | inherits(obj, 'data.frame')
}
