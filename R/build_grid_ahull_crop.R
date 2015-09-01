#'
#' @title Regular grid in alpha-hull
#' 
#' @description Create a grid within the alpha hull of a set of points.
#' 
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The number of points before cropping to the alpha hull shape
#' @param grid_opts A list with component \code{alpha} that controls the 
#'                     shape of the alpha hull
#' @param pad Ignored
#' @param ... other arguments are silently ignored
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns.
#' 
#' @family grid building functions
#' 
#'@export
build_grid_ahull_crop <- function(coords, npts, 
                                  pad = NULL, # pad is ignored for this function
                                  grid_opts = list(alpha = .3)) { 

  build_grid_ahull_fill(coords, npts, 
                        grid_opts = c(grid_opts, 
                                      list(run_max = 1,
                                           verbose = FALSE)))
}
