#
# 
# 
# 
# This file contains functions that deal with package lodaing and unloading.
# 


.onAttach <- function(libname, pkgname) {
  
  # We load plyr with rollply as many functions will be directly useful to 
  # interactive work with rollply (e.g. summarise)
  if (!"package:plyr" %in% search()) {
    R.methodsS3::pkgStartupMessage('Attaching package plyr')
    library(plyr, quietly=TRUE)
  }
  
}