
#
#'@export

build_mesh_ahull_crop <- function(coords, npts, 
                                  pad=0, # ignored
                                  mesh.opts=NULL,
                                  verbose=FALSE) {
  build_mesh_ahull_fill(coords, npts, pad, 
                        mesh.opts=c(mesh.opts, list(run.max=1)), 
                        verbose)
}
