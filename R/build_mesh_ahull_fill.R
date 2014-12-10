#'
#' @title Create a grid within the alpha hull of a set of points.
#' 
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The approximate number of points of the requested grid
#' @param mesh.opts A list with component \code{alpha} that controls the 
#'                     shape of the alpha hull, \code{error.tol} the error
#'                     tolerance (in percentage) on the number of points and 
#'                     \code{run.max} the maximum number of iterations.
#' @param verbose Show plots of the grid being built
#' @param ... other arguments are silently ignored
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns.
#' 
#' @family mesh building functions
#' 
#' @export

# Requires splancs

build_mesh_ahull_fill <- function(coords, npts, 
                                  mesh.opts=list(alpha=.3, error.tol=.05, run.max=20),
                                  verbose=FALSE,
                                  ...) {
  
  coords <- coords[!duplicated(coords), ]
  
  if (ncol(coords)!=2) 
    stop('This type of mesh is only implemented for 2 dimensions.')
  
  # Take parameters into account
  opts <- list(alpha=.3, error.tol=.05, run.max=20)
  opts[names(mesh.opts)] <- mesh.opts # alter defaults
  
  # We build an alpha hull of our x/y points.
  # NB: We rescale everything between 0 and 1 so the given alpha is easy to 
  #     interpret and inahull() works (it has trouble with far-from zero values
  #     it seems).
  # <!todo!> this introduces a bug in grid_proportional and gives it the 
  # same behavior than grid_proportional
  coords.scaled <- apply(coords, 2, scales::rescale, to=c(0,1))
  coords.hull <- alphahull::ahull(coords.scaled, alpha=opts[['alpha']])
  
  # Iterate to find the best proportional fitting 
  error <- -1; # init
  npts_iter <- npts
  run <- 1
  while (error < -opts[['error.tol']] && run <= opts[['run.max']]) {
    mesh <- get_mesh(npts_iter, coords.scaled, coords.hull)
    error <- (nrow(mesh) - npts) / npts
    
    if (verbose & error < 0) { 
      verbose.plot(coords.hull, mesh, error, run)
    }
    
    run <- run+1
    npts_iter <- round(npts_iter - error*npts)
  }
  
  # Scale back to original size
  for (i in seq.int(ncol(coords))) {
    mesh[ ,i] <- mesh[ ,i] * diff(range(coords[ ,i])) + min(coords[ ,i])
  }
  
  return(mesh)
}

get_mesh <- function(npts, coords, hull) {
  
  # Compute a grid with the given number of points
  grid.test <- build_mesh_grid_proportional(coords, npts)
  
  # Strip points and see how many are left
  to_keep <- apply(grid.test, 1, function(X) { alphahull::inahull(hull, X) })
  
  # Return the mesh
  return( grid.test[to_keep, ] )
}

# Makes an output with a plot
verbose.plot <- function(hull, mesh, error, run) {
  plot(hull)
  points(mesh, col='red', pch=3)
  title(paste0('run ', run, ' (error: ', 
               paste0(round(error*100)), '%)', sep=''))
}