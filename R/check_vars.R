# 
# 
# 
# Checks variables in input data.frame and/or mesh.
# 


check_vars <- function(.rollvars, .data_names, mesh_names, mesh.type) { 
  if ( !all(all.vars(.rollvars) %in% .data_names) ) 
    stop('Some variables could not be found in supplied data.frame')
  
  if ( !all( mesh_names %in% .data_names ) ) 
    stop('Some variables provided in the mesh could not be found in the data.frame')
  
  # Check for proper mesh type
  if (!exists(paste0('build_mesh_',mesh.type))) 
    stop('Unknown mesh type. See ?build.mesh for a list of supported mesh types')
  
}
