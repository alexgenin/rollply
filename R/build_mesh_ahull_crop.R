#'
#' @title Create a grid within the alpha hull of a set of points.
#' 
#' @param coords A matrix or data.frame of coordinates with two columns
#' @param npts The number of points before cropping to the alpha hull shape
#' @param mesh.opts A list with component \code{alpha} that controls the 
#'                     shape of the alpha hull
#' @param ... other arguments are silently ignored
#' 
#' @return The coordinates of a grid of points as a \code{data.frame} with 
#'         approximately \code{npts} rows and \code{ncol(coords)} columns.
#' 
#' @family mesh building functions
#' 
#'@export
build_mesh_ahull_crop <- function(coords, npts, 
                                  mesh.opts=list(alpha=.3),
                                  verbose=FALSE,
                                  ...) {
  build_mesh_ahull_fill(coords, npts, pad, 
                        mesh.opts=c(mesh.opts, list(run.max=1)), 
                        verbose)
}
