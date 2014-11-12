# 


# rollply: applies a function on a data frame over a sliding window. Slow but
#   generic.
# Originally written by Alexandre Genin, 2014, still uncomplete
# 
# Note: sometimes parallel computing fails saying that "..." is used in an 
#   incorrect context. This has probably to do with [1] and is unfixed at the 
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
#'                  for the rolling window and c an eventual grouping 
#' @param fun function to apply to each piece
#' @param wdw.size window size
#' @param mesh data.frame of points at which the computation is done
#' @param mesh.res if mesh is unspecified, then the number of points for the 
#'                 resolution of the mesh to build. Otherwise ignored.
#' @param padding padding policy outside of data range, one of 'none', 
#'                'outside', 'inside', or a numeric value
#' @param .parallel whether to use parallel processing (see \link{ddply} for 
#'                  more information on parallelism). 
#' @param ... other arguments passed to ddply and fun
#' 
#' @details
#' 
#' Rollply uses internally ddply on the provided or automatically built mesh. 
#' The mesh can be predefined
#' 
#' @examples
#' 
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
# 
rollply <- function(.data,
                    .rollvars,
                    wdw.size,
                    fun,
                    mesh=NULL,
                    mesh.res=200,
                    mesh.type='grid_identical', # grid_identical, grid_proportional, ahull_fill
                    mesh.options=NULL,
                    padding='none', # outside/inside/none or value
                    .parallel=FALSE,
                    ...) {  # passed to fun
  
  #<!todo!> add checks that variables are present in data.frame otherwise we 
  # will have wierd ass results due to lexical scoping.
  check_vars(.rollvars, names(.data), names(mesh), mesh.type, mesh)
  
  # Handle groups: if we provide groups, then we dispatch rollply within each
  # groups using ddply.
  if (.has_groups(.rollvars)) {
    # Build new argument lists
    args.grps <- as.list(match.call(), expand.dots=TRUE)
    args.grps[['.rollvars']]  <- .split_groups(.rollvars)[['vars']]
    args.grps[['.variables']] <- .split_groups(.rollvars)[['groups']]
    return( do.call(plyr::ddply, args.grps, envir=parent.frame()) )
  }
  
  # We extract variables used for computing and build a matrix
  # <!todo!> Add check that variables used for rolling windows are numeric!
  .rollvars.quoted <- plyr::as.quoted(.rollvars)
  coords <- lapply(.rollvars.quoted, eval, envir=.data)
  if (any(!unlist(lapply(coords,is.numeric)))) { 
    stop('Moving window parameters non-numeric.')
  }
  coords <- matrix(unlist(coords), 
                   ncol=length(.rollvars.quoted),
                   dimnames=list(NULL, names(.rollvars.quoted)))
  
  # Check if NAs, and if yes then act
  NA_lines <- apply(coords, 1, function(X) any(is.na(X)))
  if (any(NA_lines)) {
    # We do not implement a removing of NAs as this would create a copy of 
    # a potentially big dataset.
    stop('NA in moving window parameters are not supported. Try removing them.')
  }
  
  # Determine sides policy
  if (!is.numeric(padding)) {
    pad <- switch(padding,
                  outside = wdw.size/2,
                  inside  = -wdw.size/2,
                  none    = 0)
  }
  
  # Build output mesh
  if (is.null(mesh)) {
    mesh <- build_mesh(mesh.type, coords, mesh.res, pad, mesh.options)
  }
  
  # Do the work brah.
  # Note: we use ddply here as it has a more robust behaviour than adply, 
  # (most notably when the inner function returns a data.frame with multiple
  # lines).
  result <- plyr::ddply(mesh, names(mesh),
                        do_rollply, coords, wdw.size, .data, fun,
                        ...)
  
  return(result)
}
