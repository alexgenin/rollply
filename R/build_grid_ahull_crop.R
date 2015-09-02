#'
#' @title Regular grid in alpha-hull
#'
#' @description Create a grid within the alpha hull of a set of points.
#'
#' @details This function creates a rectangular grid, then computes the alpha
#'          hull of a set of points, and discards all the points of this grid
#'          that fall outside of the hull.
#'
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The number of points before cropping to the alpha hull shape
#' @param grid_opts A list with component \code{alpha} that controls the
#'                     shape of the alpha hull
#' @param pad Ignored
#' @param ... other arguments are silently ignored
#'
#' @return The coordinates of a grid of points as a \code{data.frame} with
#'         \code{ncol(coords)} columns. Names are transfered from the
#'         \code{coords} data frame.
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
