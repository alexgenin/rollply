# 
# 
# 
# Checks variables in input data.frame and/or mesh.
# 


check_args <- function(.rollvars, 
                       .data, 
                       mesh, 
                       mesh.type) { 
  
  # Check for proper mesh type
  if (!exists(paste0('build_mesh_',mesh.type))) 
    stop('Unknown mesh type. See ?build.mesh for a list of supported mesh types')
  
  # Check if the mesh is a named data.frame
  if ( !is.null(mesh) && 
      (is.null(names(mesh)) || !is.data.frame(mesh)) )
    stop('The supplied mesh must be a named data.frame')
}
