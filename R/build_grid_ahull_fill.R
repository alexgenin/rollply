#'
#' @title Create a grid within the alpha hull of a set of points.
#' 
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The approximate number of points of the requested grid
#' @param grid.opts A list with component \code{alpha} that controls the 
#'                     shape of the alpha hull, \code{error.tol} the error
#'                     tolerance (in percentage) on the number of points and 
#'                     \code{run.max} the maximum number of iterations.
#' @param verbose Show plots of the grid being built
#' @param ... other arguments are silently ignored
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns.
#' 
#' @family grid building functions
#' 
#' @export

# Requires splancs

build_grid_ahull_fill <- function(coords, npts, 
                                  grid.opts=list(alpha=.3, error.tol=.05, run.max=20),
                                  verbose=FALSE,
                                  ...) {
  
  coords <- coords[!duplicated(coords), ]
  
  if (ncol(coords)!=2) 
    stop('This type of grid is only implemented for 2 dimensions.')
  
  # Take parameters into account
  opts <- list(alpha=.3, error.tol=.05, run.max=20)
  opts[names(grid.opts)] <- grid.opts # alter defaults
  
  # We build an alpha hull of our x/y points.
  # NB: We rescale everything between 0 and 1 so the given alpha is easy to 
  #     interpret and inahull() works (it has trouble with far-from zero values
  #     it seems).
  # <!todo!> this introduces a bug in grid_proportional and gives it the 
  # same behavior than grid_proportional
  coords.scaled <- apply(coords, 2, scales::rescale, to=c(0,1))
  coords.hull   <- alphahull::ahull(coords.scaled, alpha=opts[['alpha']])
  
  # Iterate to find the best proportional fitting 
  error <- -1; # init
  npts_iter <- npts
  run <- 1
  while (error < -opts[['error.tol']] && run <= opts[['run.max']]) {
    grid <- get_grid(npts_iter, coords.scaled, coords.hull)
    error <- (nrow(grid) - npts) / npts
    
    if (verbose & error < 0) { 
      verbose.plot(coords.hull, grid, error, run)
    }
    
    run <- run+1
    npts_iter <- round(npts_iter - error*npts)
  }
  
  # Scale back to original size
  grid <- as.data.frame(mapply(function(m,c) m * diff(range(c)) + min(c), 
                               grid2, coords))
  
  return(grid)
}

get_grid <- function(npts, coords, hull) {
  
  # Compute a grid with the given number of points
  grid.test <- build_grid_proportional(coords, npts)
  
  # Strip points and see how many are left
  to_keep <- apply(grid.test, 1, function(X) { alphahull::inahull(hull, X) })
  
  # Return the grid
  return( grid.test[to_keep, ] )
}

# Makes an output with a plot
verbose.plot <- function(hull, grid, error, run) {
  plot(hull)
  points(grid, col='red', pch=3)
  title(paste0('run ', run, ' (error: ', 
               paste0(round(error*100)), '%)', sep=''))
}
