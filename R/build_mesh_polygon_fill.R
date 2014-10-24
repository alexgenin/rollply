# 
# 
# 
#' @export

build_mesh_polygon_fill <- function(coords, npts, pad=0, mesh.options=list(nb=5)) {
  
  if (ncol(coords)!=2) 
    stop('This type of mesh is only implemented for 2 dimensions.')
  
  # Get distance threshold: for each point we get its 10 nearest neighbors and 
  # take the maximum of this distance. Then we average these maxima.
  nb <- ifelse(is.null(mesh.options[['nb']]), 7, mesh.options[['nb']])
  nb.dists <- apply(as.matrix(dist(coords)), 1, function(X) X[rank(X) <= nb+1])
  thresh <- mean(nb.dists[-1, ])
  
  # Iterate to find the best proportional fitting 
#   browser()
  error <- -1; # init
  npts_iter <- npts
  while (error < 0) {
    mesh <- get_mesh(npts_iter, coords, thresh)
    error <- (nrow(mesh) - npts)
    npts_iter <- npts_iter - error
  }
  
  return(mesh)
  
}

get_mesh <- function(npts, coords, dist.thresh) {
  
  npts <- round(npts)
  
  # Compute a grid with the given number of points
  grid.test <- build_mesh_grid_proportional(coords, npts, pad=0)
    
  # Strip points and see how many are left
  to_keep <- apply(grid.test, 1, strip_points, coords, dist.thresh)
  
  # Return the mesh
  return( grid.test[to_keep, ] )
}

get_error_perc <- function(mesh, target) { 
  (nrow(mesh) - target) / target
}


# 
strip_points <- function(cur_points, coords, pad) {
  cur_points <- t(as.matrix(cur_points))
  any(lookup_multi_dim(cur_points, coords, pad))
}