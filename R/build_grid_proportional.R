#'
#' @title Create a grid with a number of points on each dimension 
#'        proportional to the length of that dimension.
#'
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The approximate total number of points of the output grid 
#' @param pad Padding on each dimension (a positive number makes a grid
#'            that is larger than the ranges of the coordinates).
#' @param ... other arguments are silently ignored
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns.
#' 
#' @family grid building functions
#' 
#'@export

#' @export
build_grid_proportional <- function(coords, npts, pad=0, 
                                         ...) {  # ignored
  coords.ranges <- apply(coords, 2, range)
  ndims <- ncol(coords)
  
  if (ncol(coords)!=2) 
    stop('This type of grid is only implemented for 2 dimensions.')
  
  npts.dims <- get_npts_dims(coords.ranges, npts, pad)
  
  # Build grid seed to feed to expand.grid
  grid.seed <- list()
  for (col in seq.int(ncol(coords))) {
    grid.seed[[col]] <- build_grid_seed_onedim(col, coords.ranges, 
                                               npts.dims[col], pad)
  }
  
  names(grid.seed) <- colnames(coords)
  
  expand.grid(grid.seed, KEEP.OUT.ATTRS = FALSE)
}

get_npts_dims <- function(coords.ranges, npts, pad) { 
  # Given npts to put in a L*l rectangle of size ratio r (=L/l), then
  # m=sqrt(r*npts) and n=r*sqrt(npts) to have m x n tiles.
  sides.l <- apply(coords.ranges, 2, diff) + pad
  r <- max(sides.l) / min(sides.l)
  npts.dims <- c(sqrt(npts/r), sqrt(npts*r))[order(sides.l)]
  npts.dims <- round(npts.dims)
  
  return(npts.dims)
}

