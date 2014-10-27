# 
# 
# 

#' @export
build_mesh_grid_proportional <- function(coords, npts, pad=0, 
                                         mesh.options=NULL,
                                         ...) {  # ignored
  coords.ranges <- apply(coords, 2, range)
  ndims <- ncol(coords)
  
  if (ncol(coords)!=2) 
    stop('This type of mesh is only implemented for 2 dimensions.')
  
  npts.dims <- get_npts_dims(coords.ranges, npts, pad)
  
  # Build mesh seed to feed to expand.grid
  mesh.seed <- list()
  for (col in seq.int(ncol(coords))) {
    mesh.seed[[col]] <- build_mesh_seed_onedim(col, coords.ranges, 
                                               npts.dims[col], pad)
  }
  
  names(mesh.seed) <- colnames(coords)
  
  expand.grid(mesh.seed, KEEP.OUT.ATTRS = FALSE)
}

get_npts_dims <- function(coords.ranges, npts, pad) { 
  # Given npts to put in a L*l rectangle of size ratio r (=L/l), we can show 
  # that m=sqrt(r*npts) and n=r*sqrt(npts) to have mxn tiles.
  sides.l <- apply(coords.ranges, 2, diff) + pad
  r <- max(sides.l) / min(sides.l)
  npts.dims <- c(sqrt(npts/r), sqrt(npts*r))[order(sides.l)]
  npts.dims <- round(npts.dims)
  
  return(npts.dims)
}

