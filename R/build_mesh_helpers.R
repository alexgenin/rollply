# 
# Helpers for mesh building
# 


# Builds the mesh, dispatching to the right function depending on provided
# mesh type.
build_mesh <- function(type, ...) {
  
  mesh.genfun.name <- paste0('build_mesh_',type)
  
  if (!exists(mesh.genfun.name)) {
    stop('Unknown mesh type specified')
  }
  
  mesh.genfun <- get(mesh.genfun.name)
  return(as.data.frame(mesh.genfun(...)))
}

# Builds a set of points equally spaced, taking pad into account
build_mesh_seed_onedim <- function(col, range, npts, pad) {
  seq(min(range[ ,col]) - pad, 
      max(range[ ,col]) + pad, 
      length.out=npts) 
}
