#' 
#' @title Create a regular grid with the same number of points on each 
#'        dimension.
#' 
#' @param coords A matrix or data.frame of coordinates
#' @param npts The approximate total number of points of the output grid 
#' @param pad Padding on each dimension (a positive number makes a grid
#'            that is larger than the ranges of the coordinates).
#' @param ... other arguments are silently ignored
#'
#' @return The coordinates of a regulary spaced grid of points as a 
#'         \code{data.frame} with approximately \code{npts} rows and 
#'         \code{ncol(coords)} columns.
#'
#' @family grid builders
#' 
#'@export
# 

build_grid_identical <- function(coords, npts, pad = 0, ...) {
  coords.ranges <- apply(coords, 2, range)
  ndims <- ncol(coords)
  
  # The input npts is the *total* number of points wanted so we need ^(1/ndims)
  # so we use the correct, identical amount of points on each dimension.
  length.onedim <- floor(npts^(1/ndims))
  
  grid.seed <- sapply(seq.int(ncol(coords)), 
                      build_grid_seed_onedim,coords.ranges,length.onedim,pad,
                      simplify = FALSE)
  names(grid.seed) <- colnames(coords)
  
  return( expand.grid(grid.seed, KEEP.OUT.ATTRS = FALSE) )
}
