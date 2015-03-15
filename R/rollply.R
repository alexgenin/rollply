# 
# 
# rollply: applies a function on a data frame over a sliding window. Slow but
#   generic.
# Originally written by Alexandre Genin, 2014, still uncomplete
# 
# Note: sometimes parallel computing fails saying that "..." is used in an 
#   incorrect context. This is probably related to [1] and is unfixed at the 
#   moment.
# 
# [1] https://github.com/hadley/plyr/issues/203
# 



#' @title Rollply
#' 
#' @description
#' For each subset of a data.frame falling in a moving window, computes the 
#' results of a function on this subset, then combine results in a data.frame.
#' 
#' @param .data data.frame to be processed
#' @param .rollvars variables describing the moving window: a formula of the 
#'                  form ~ a + b | c, where a and b denote the variables used 
#'                  for the rolling window and c an optional grouping variable
#' @param fun function to apply to each subset
#' @param wdw.size window size
#' @param grid data.frame of points at which the computation is done. If 
#'             \code{NULL} then a grid is generated using \code{grid.npts}
#'             and \code{grid.type}.
#' @param grid.npts if grid is unspecified, then the number of points for the 
#'                  resolution of the grid to build. Otherwise ignored.
#' @param grid.type The type of grid to generate
#' @param grid.options Options to be passed to the grid-generating function
#' @param padding padding policy outside of data range, one of 'none', 
#'                'outside', 'inside', or a numeric value 
#' @param .parallel whether to use parallel processing (see \link{ddply} for 
#'                  more information on parallelism). 
#' @param ... other arguments passed to ddply and fun
#' 
#' @details
#' 
#' Rollply allows applying a function on subsets falling in a window moving over
#' one or more variables. It is built internally over ddply and follows a very 
#' similar syntax. 
#' 
#' @return 
#' A named data.frame with the function results at each grid point.
#' 
#' @examples
#' 
#' library(plyr)
#' # Generate 1D random walk
#' dat <- data.frame(time=seq.int(1000),
#'                   position=cumsum(rnorm(1000,0,10)))
#' 
#' rollav <- rollply(dat, ~ time, wdw.size=10, 
#'                   summarise, position=mean(position))
#' 
#' plot(position ~ time, data=dat)
#' lines(rollav, col='red')
#' 
#' 
#' # see http://github.com/alexgenin/rollply for more examples
#' 
#' @useDynLib rollply
#' @importFrom Rcpp sourceCpp
#' @export
#' 

rollply <- function(.data,
                    .rollvars,
                    wdw.size,
                    fun,
                    grid=NULL,
                    grid.npts=200,
                    grid.type='identical', # identical, proportional, ahull_crop, ahull_fill
                    grid.options=NULL,
                    padding='none', # outside/inside/none or value
                    .parallel=FALSE,
                    ...) {  # passed to fun
  
  # Parse formula before checks
  .rollvars <- formulr::as.formulr(.rollvars)
  
  #<!todo!> add checks that variables are present in data.frame otherwise we 
  # will have wierd results due to lexical scoping.
  check_args(.rollvars, .data, grid, grid.type)
  
  if ( ! is.data.frame(.data)) .data <- as.data.frame(.data)
  
  
  # Handle groups: if we provide groups, then we call rollply within each
  # groups using ddply.
  if (formulr::has.g(.rollvars)) {
    # Build new argument list
    args.grps <- as.list(match.call(), expand.dots=TRUE)
    # Pass the group argument of the formula as the group to ddply and delete
    # it in the original formula
    args.grps[['.variables']] <- formulr::form.g(.rollvars) # grabbed by ddply
    formulr::form.g(.rollvars) <- NA
    args.grps[['.rollvars']]  <- .rollvars 
    # Call ddply
    return( do.call(plyr::ddply, args.grps, envir=parent.frame()) )
  }
  
  # We extract variables used for computing and build a matrix
  # <!todo!> Add check that variables used for rolling windows are numeric!
  coords <- formulr::eval.formulr(.rollvars, envir=.data, enclos=parent.frame())
  coords <- do.call(cbind, coords[['x']])
  
  # Check for NAs
  check_coords(coords, grid)
  
  # Determine sides policy
  if (!is.numeric(padding)) {
    pad <- switch(padding,
                  outside = wdw.size/2,
                  inside  = - wdw.size/2,
                  none    = 0)
  }
  
  # Build output grid
  if (is.null(grid)) {
    grid <- build_grid(grid.type, coords, grid.npts, pad, grid.options)
  } else if ( ! is.data.frame(grid)) { 
    grid <- as.data.frame(grid)
  }
  
  
  # Do the work 
  # Note: we use ddply here as it has a more robust behaviour than adply, 
  # (most notably when the inner function returns a data.frame with multiple
  # lines).
  result <- plyr::ddply(grid, names(grid),
                        do_rollply, coords, wdw.size, .data, fun,
                        ...)
  
  return(result)
}
