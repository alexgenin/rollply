#'
#' @title Regular grid in alpha-hull
#'
#' @description Create a grid within the alpha hull of a set of points.
#' 
#' @details This function computes the alpha hull of a set of points, then 
#'          iteratively finds the best grid of \code{npts} points fitting in the hull.
#'
#' 
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The approximate number of points of the requested grid
#' @param pad ignored.
#' @param grid_opts A list with component \code{alpha} that controls the 
#'                     shape of the alpha hull, \code{error_tol} the error
#'                     tolerance as a fraction of the number of points, 
#'                     \code{run_max} the maximum number of iterations and 
#'                     \code{verbose} to get some debug output.
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns. Names
#'         are transfered from the \code{coords} data frame.
#' 
#' @family grid building functions
#' 
#' @export
build_grid_ahull_fill <- function(coords, npts, 
                                  pad = NULL, # pad is ignored for this function
                                  grid_opts = list(alpha = .3, 
                                                   error_tol = .05, 
                                                   run_max = 20, 
                                                   verbose = FALSE)) {
  
  build_grid_check_vars(coords, npts)
  
  # Remove duplicates otherwise ahull throws an error
  coords <- coords[! duplicated(coords), ]
  
  if (ncol(coords)!=2) {
    stop('This type of grid is only implemented for 2 dimensions.')
  }
  
  # Take parameters into account
  opts <- list(alpha = .3, error_tol = .05, run_max = 20, verbose = FALSE)
  opts[names(grid_opts)] <- grid_opts # alter defaults
  verbose <- opts[['verbose']]
  
  # We build an alpha hull of our x/y points.
  # NB: We rescale everything between 0 and 1 so the given alpha is easy to 
  #     interpret and inahull() works (it has trouble with far-from zero values
  #     it seems).
  # <!todo!> this introduces a bug in grid_square tile and gives it the 
  # same behavior than grid_identical
  coords.scaled <- apply(coords, 2, scales::rescale, to = c(0,1))
  coords.hull   <- alphahull::ahull(coords.scaled, alpha = opts[['alpha']])
  
  if (verbose) cat('Building grid in alphahull...\n')
  
  # Iterate to find the closest number of points
  error <- -1
  run <- 1
  npts_target <- npts
  while ( error < - opts[['error_tol']] && run <= opts[['run_max']] ) {
    
    # Compute a grid with the given number of points
    rect_grid <- build_grid_squaretile(coords.scaled, npts)
    
    # Strip points and see how many fall in alphahull
    to_keep <- apply(rect_grid, 1, 
                     function(X) alphahull::inahull(coords.hull, X))
    error <- (sum(to_keep) - npts_target) / npts_target
    
    if (verbose) { 
      cat(paste0('run ', run,', error=', round(error, digits = 2), 
                '% (', sum(to_keep), ' points)\n'));
    }
    
    # Increase number of points
    run <- run + 1
    npts <- round(npts - error * npts)
  }
  
  # Scale back to original size
  grid <- rect_grid[to_keep, ]
  grid <- data.frame(x = grid[ ,1]*diff(range(coords[ ,1])) + min(coords[ ,1]),
                     y = grid[ ,2]*diff(range(coords[ ,2])) + min(coords[ ,2]))
  
  return(grid)
}

# Debug snippet: makes an output with a plot
#   plot(hull)
#   points(grid, col='red', pch=3)
#   title(paste0('run ', run, ' (error: ', 
#                paste0(round( error * 100 )), '%)', sep=''))

