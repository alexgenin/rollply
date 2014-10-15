# 
# Builds the mesh needed for rollply.
# 
# <!todo!> Start thinking about using raster_fill which is really nice.

build_mesh_seed <- function(coords,npts,pad) {
  coords.ranges <- apply(coords, 2, range)
  
  # Note: we use sa
  mesh.seed <- sapply(seq.int(ncol(coords)), 
                      build_mesh_seed_onedim,coords.ranges,npts,pad,
                      simplify=FALSE)
  names(mesh.seed) <- colnames(coords)
  
  return(mesh.seed)
}

build_mesh_seed_onedim <- function(col,range,npts,pad) {
  seq(min(range[ ,col])-pad, 
      max(range[ ,col])+pad, 
      length.out=npts) 
}
