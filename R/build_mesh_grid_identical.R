# 
# Builds the mesh needed for rollply.
# 
#' @export

# Builds a "seed" that is the set of coordinates to be fed to expand.grid
build_mesh_grid_identical <- function(coords, npts, pad=0, mesh.options=NULL, 
                                      ...) { # ignored
  coords.ranges <- apply(coords, 2, range)
  ndims <- ncol(coords)
  
  # The input npts is the *total* number of points wanted so we need ^(1/ndims)
  # so we use the correct, identical amount of points on each dimension.
  length.onedim <- floor(npts^(1/ndims))
  
  mesh.seed <- sapply(seq.int(ncol(coords)), 
                      build_mesh_seed_onedim,coords.ranges,length.onedim,pad,
                      simplify=FALSE)
  names(mesh.seed) <- colnames(coords)
  
  return( expand.grid(mesh.seed, KEEP.OUT.ATTRS = FALSE) )
}
