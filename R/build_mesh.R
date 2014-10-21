# 
# Builds the mesh needed for rollply.
# 

build_mesh_seed <- function(coords,npts,pad) {
  coords.ranges <- apply(coords, 2, range)
  ndims <- ncol(coords)
  
  # The input npts is the *total* number of points wanted so we need ^(1/ndims)
  # so we use the correct amount of points on each dimension.
  length.onedim <- round(npts^(1/ndims))
  
  mesh.seed <- sapply(seq.int(ncol(coords)), 
                      build_mesh_seed_onedim,coords.ranges,length.onedim,pad,
                      simplify=FALSE)
  names(mesh.seed) <- colnames(coords)
  
  return(mesh.seed)
}

build_mesh_seed_onedim <- function(col,range,npts,pad) {
  seq(min(range[ ,col])-pad, 
      max(range[ ,col])+pad, 
      length.out=npts) 
}
