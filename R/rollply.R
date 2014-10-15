#!/usr/bin/R
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
# 


#' Applies a function over a rolling window.
#'
#' @export

rollply <- function(.data,
                    .rollvars,
                    wdw.size,
                    fun,
                    npts=200,
                    mesh=NULL,
                    padding='none', # outside/inside/none or value
                    .parallel=FALSE,
                    ...) {  # passed to fun
  require(plyr)
  
  #<!todo!> add checks that variables are present in data.frame otherwise we 
  # will have wierd ass results due to lexical scoping.
  
  #<!todo!> We have trouble with summarise!
  
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
    mesh <- expand.grid(build_mesh_seed(coords,npts,pad),
                        KEEP.OUT.ATTRS = FALSE)
  }
  if (!is.matrix(mesh)) mesh <- as.matrix(mesh)
  
  # Get lookup function
  lookup_fun <- .lookup_one_dim
  if (ncol(coords) > 1) {
    lookup_fun <- lookup_multi_dim
  }
  
  # Do the work brah
  result <- plyr::adply(mesh,1,
                  do_rollply, coords, wdw.size, .data, fun, lookup_fun,
                  ...)
  
  return( data.frame(mesh, result[,-1,drop=FALSE]) )
}
