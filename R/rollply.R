#
# rollply: split-apply-combine strategy for moving-window computations.
# Originally written by Alexandre Genin, 2014
#
# Note: sometimes parallel computing warns that "..." is used in an
#   incorrect context. This is probably related to [1] and is unfixed at th
#   moment.
#
# [1] https://github.com/hadley/plyr/issues/203
#
#' @title Rollply
#'
#' @description Applies a function on data falling in a moving window, then
#'              combine the results in a data.frame.
#'
#' @param .data \code{data.frame} to be processed
#' @param .rollvars variables describing the moving window: a formula of the
#'                  form \code{~ a + b | c}, where \code{a} and \code{b} denote
#'                  the variables used for the rolling window and \code{c} an
#'                  optional grouping variable
#' @param fun function to apply
#' @param wdw.size window size
#' @param grid data.frame of points at which the computation is done. If
#'             \code{NULL} then a grid is generated using \code{grid_npts}
#'             and \code{grid_type}.
#' @param grid_npts if grid is unspecified, then the number of points for the
#'                  resolution of the grid to build. Otherwise ignored.
#' @param grid_type The type of grid to generate
#' @param grid_opts Options to be passed to the grid-generating function
#' @param padding padding policy one the edgse of the dataset, one of 'none',
#'                'outside', 'inside', or a numeric value
#' @param .parallel whether to use parallel processing (see \code{\link{ddply}}
#'                  for more information on parallelism).
#' @param ... other arguments passed to \code{\link{ddply}} and \code{fun}
#'
#' @details
#'
#' rollply applies a function in a window moving over one or more variables. It
#' is built upon \code{\link{ddply}} so it inherits many of its useful options
#' 'such as \code{.parallel} or \code{.progress}.
#'
#' rollply will create a grid spanning the coordinates specificied in the
#' formula. For each point of this grid, it then selects the corresponding
#' subset of the data frame within \code{wdw.size}, and pass it to the
#' function \code{fun}. The results are then combined into a data frame.
#'
#' In case the user provides a grid, its column names must match the dimension
#' names specified in the formula (the handling of groups in a user-supplied
#' grid is not (yet?) implemented.')
#'
#' @return
#'
#' A named data.frame with the function results at each grid point.
#'
#' @examples
#'
#' library(plyr)
#' # 1D example
#' dat <- data.frame(time=seq.int(1000),
#'                   position=cumsum(rnorm(1000,0,10)))
#'
#' rollav <- rollply(dat, ~ time, wdw.size=10,
#'                   summarise, position=mean(position))
#'
#' plot(position ~ time, data=dat)
#' lines(rollav, col='red')
#'
#' # 2D example
#'
#' # Generate three 2D random walks
#' dat <- ddply(data.frame(person=c('francois','nicolas','jacques')), ~ person,
#'              summarise,
#'               time=seq.int(1000),
#'               x=cumsum(rnorm(1000,0,1)),
#'               y=cumsum(rnorm(1000,0,1)))
#'
#' # Smoothed trajectory over ten time-steps
#' rollav <- rollply(dat, ~ time | person, wdw.size=10, grid_res=1000,
#'                   summarise, x=mean(x), y=mean(y))
#'
#' library(ggplot2)
#' if (require(ggplot2)) {
#' ggplot(dat,aes(x,y,color=person)) +
#'   geom_point(alpha=.5, shape='+') +
#'   geom_path(data=rollav)
#' }
#'
#  # Where did people spend their time ?
#'  fixed_grid <- build_grid_squaretile(dat[ ,c('x','y')], 2000) # we fix the grid across groups
#'  rollav <- rollply(dat, ~ x + y | person, wdw.size=2, grid=fixed_grid,
#'                    summarise, time.spent=length(time))
#'
#' if (require(ggplot2)) {
#'   ggplot(subset(rollav, time.spent>0)) +
#'     geom_point(aes(x,y, color=person, size=time.spent), alpha = .5) +
#'     facet_grid(~person)
#' }
#'
#' # see also vignette(rollply_intro)
#'
#' @useDynLib rollply
#' @importFrom Rcpp sourceCpp
#' @export
#'

rollply <- function(.data,
                    .rollvars,
                    wdw.size,
                    fun,
                    grid = NULL,
                    grid_npts = NULL,
                    grid_type = 'identical', # identical, proportional, ahull_crop, ahull_fill
                    grid_opts = NULL,
                    padding   = 'none', # outside/inside/none or value
                    .parallel = FALSE,
                    ...) {  # passed to ddply/fun

  vars <- parse_formula(.rollvars, enclos=parent.frame())

  check_args(vars, .data, grid, grid_type, wdw.size)

  # Handle groups: if we provide groups, then we call rollply within each
  # groups using ddply.
  if ( ! is.na(vars[["groups"]]) ) {
    # Build new argument list
    args.grps <- as.list(match.call(), expand.dots=TRUE)
    # Pass the group argument of the formula as the group to ddply and delete
    # it in the original formula
    args.grps[['.variables']] <- vars[["groups"]] # .variable is ddply's arg
    vars[["groups"]] <- NA
    args.grps[['.rollvars']]  <- vars
    # Call ddply
    return( do.call(plyr::ddply, args.grps, envir=parent.frame()) )
  }

  # We extract variables used for computing and build a matrix
  # <!todo!> Add check that variables used for rolling windows are numeric!
  coords <- plyr::eval.quoted(vars[["coords"]],
                              envir=as.data.frame(.data))
  coords <- do.call(cbind, coords)

  # Set the number of points for the grid
  if ( is.null(grid_npts) ) {
    coords_span <- apply(coords, 2, function(x) diff(range(x)))
    grid_npts <- floor(max(coords_span / wdw.size ^ ncol(coords)) )
  }

  # Check for NAs, zero area, etc
  check_coords(coords, grid)

  # Determine sides policy
  if (!is.numeric(padding)) {
    padding <- switch(padding,
                      outside = wdw.size / 2,
                      inside  = - wdw.size / 2,
                      none    = 0)
  }

  # Build output grid
  if (is.null(grid)) {
    grid <- build_grid(grid_type, coords, grid_npts, padding, grid_opts)
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
