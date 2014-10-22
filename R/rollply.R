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



#' Rollply
#'
#' For each subset of a data.frame falling in a moving window, computes the 
#' results of a function on this subset, then combine results in a data.frame.
#' 
#' @param .data data.frame to be processed
#' @param .rollvars variables describing the moving window: a formula of the 
#'                  form ~ a + b | c, where a and b denote the variables used 
#'                  for the rolling window and c an eventual grouping 
#' @param fun function to apply to each piece
#' @param wdw.size window size
#' @param mesh mesh of points at which the moving window is evaluated
#' @param mesh.res if mesh is unspecified, then the number of points for the 
#'                 resolution of the default mesh to build. Otherwise ignored.
#' @param padding padding policy outside of data range, one of 'none', 
#'                'outside', 'inside', or a numeric value
#' @param .parallel whether to use parallel processing (see \link{ddply} for 
#'                  more information on parallelism)
#' @param ... other arguments passed first to ddply and/or adply, then to fun
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
                    padding='none', # outside/inside/none or value
                    .parallel=FALSE,
                    ...) {  # passed to fun
  
  #<!todo!> add checks that variables are present in data.frame otherwise we 
  # will have wierd ass results due to lexical scoping.
  if (!all(all.vars(.rollvars) %in% names(.data))) 
    stop('Required variables are not contained in supplied data.frame')
  
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
  coords <- matrix(unlist(coords), 
                   ncol=length(.rollvars.quoted),
                   dimnames=list(NULL, names(.rollvars.quoted)))
  
  # Determine sides policy
  if (!is.numeric(padding)) {
    pad <- switch(padding,
                  outside = wdw.size/2,
                  inside  = -wdw.size/2,
                  none    = 0)
  }
  
  # Build output mesh
  if (is.null(mesh)) {
    mesh <- build_mesh(coords,mesh.res,pad)
  }
  if (!is.matrix(mesh)) mesh <- as.matrix(mesh)
  
  # Get lookup function
  lookup_fun <- lookup_one_dim
  if (ncol(coords) > 1) {
    lookup_fun <- lookup_multi_dim
  }
  
  # Do the work brah
  result <- plyr::adply(mesh,1,
                  do_rollply, coords, wdw.size, .data, fun, lookup_fun,
                  ...)
  
  return( data.frame(mesh, result[ ,-1,drop=FALSE]) )
}
