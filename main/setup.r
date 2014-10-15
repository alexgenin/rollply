#!/usr/bin/R -f
# 
# 
# This file defines a proper work environment. Variables prefixed with .S_ are 
# deleted at the end of the script.
# 

## ========================================================================== ##
##                            Work directories                                ##
## ========================================================================== ##

# Base directory. Providing an absolute path is heavily recommended
.S_BASEDIR  <- '~/info/R/rollply' 
# Directory containing functions definitions
.S_LIBDIR   <- './lib'
# Directory containing variables definitions
.S_DEFDIR   <- './def'
# Directory containing data
.S_DATADIR  <- './data'
# Directory containing temporary data
.S_TMPDIR   <- './tmp'
# Get hostname (or set it here)
.S_HOSTNAME <- system('hostname', show=FALSE)

## ========================================================================== ##
##                      Packages to load at start                             ##
## ========================================================================== ##
# Specify your favorite packages here
.STARTPKGS <- c('ggplot2',
                'Rcpp',
                'plyr')

## ========================================================================== ##
##                            Computing options                               ##
## ========================================================================== ##

# Parallel computing options. Parallel computing is provided by foreach + parallel
# packages. They will be installed if necesary

# Enable parallel computing ?
.S_PARALLEL        <- TRUE

# Number of cores to use ? Setting it to 'auto' will detect and use all the 
# available cores.
# Use only 2 for MDS on soniaserv as it duplicates a lot of stuff apparently.
.S_PARALLEL.NCORES <- 2

# Caching options
.S_CACHE    <- TRUE 
.S_CACHEDIR <- .S_TMPDIR

## ========================================================================== ##
##                             Verbosity level                                ##
## ========================================================================== ##
.S_VERBOSE  <- 4 # Set verbosity level 0-6

## ========================================================================== ##
##                             External tools                                 ##
## ========================================================================== ##
 # Array of scripts to launch at start
.S_EXTTOOLS  <- c()

## ========================================================================== ##
##                            Misc. R options                                 ##
## ========================================================================== ##
# Default repo for package installation
.S_REPOS <-  "http://cran.us.r-project.org"

# Display options
# Number of digits displayed for number
.S_DIGITS   <- 4   # defaults to 7
# Number of maximum lines for print, cat, etc.
.S_MAXPRINT <- 40000 # defaults to 99999
# width of R terminal, auto or number of columns
.S_WIDTH    <- 'auto'

# R interpreter options
# set this to recover to debug on the spot when an error occurs
.S_ERRORF   <- stop




# Code below will apply chosen settings.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# We define a function to install packages if necesary
.S_libinst <- function(name) {
  for (nm in name) {
    .is.present <- require(nm,character.only = TRUE,quietly=TRUE)
    if (.is.present) {
      .S_mvb(5,'Loading package ',nm,'.')
      library(nm,character.only = TRUE)
    } else {
      .S_mvb(5,'Installing package ',nm,'.')
      install.packages(nm,repos=.S_REPOS,quiet=.S_VERBOSE<2)
      .S_mvb(5,'Loading package ',nm,'.')
      library(nm,character.only = TRUE)
    }
  }
}

# Print if verbose enough
.S_mvb <- function(vb,...) {
  if (.S_VERBOSE >= vb) { # vb = verbosity level
    message(...)
  }
}


# Code below applies functions
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Set R interpreter options
options(digits=.S_DIGITS,
        max.print=.S_MAXPRINT,
        width=ifelse(.S_WIDTH=='auto',
                     as.numeric(system('tput cols',intern=TRUE)),
                     200),
        verbose=ifelse(.S_VERBOSE>=6,TRUE,FALSE))

## Load packages
.S_mvb(1,'Loading packages')
for (.S_p in .STARTPKGS) .S_libinst(.S_p)

## Parallel computing
# Register parallel backend if desired
.S_libinst(c('doParallel','foreach'))
if (.S_PARALLEL) {
  .S_mvb(1,'Setting up parallel computing')
  if (.S_PARALLEL.NCORES == 'auto') {
    .S_PARALLEL.NCORES <- detectCores()
    .S_mvb(4,'Figuring out the number of cores')
    .S_libinst('R.utils')
    if (.S_PARALLEL.NCORES > 8) {
      .S_PARALLEL.NCORES <- .S_PARALLEL.NCORES - 1 # use all but one core on big computers
    }
  }
  
  if (.S_PARALLEL.NCORES>1) {
   .S_mvb(3,'Parallel computing enabled, using ',.S_PARALLEL.NCORES,' cores.')
    if (exists('.LOCALCLUST')) stopCluster(.LOCALCLUST)
    .LOCALCLUST <- makeCluster(.S_PARALLEL.NCORES)
    registerDoParallel(.LOCALCLUST)
  } else {
    .S_mvb(4,'Stopping leftover cluster')
    print("coin")
    if (exists('.LOCALCLUST')) stopCluster(.LOCALCLUST)
  }
} else {
  .S_mvb(3,'Parallel computing disabled')
  if (exists('.LOCALCLUST')) stopCluster(.LOCALCLUST)
}

# Export variables to created cluster
.exportEnv <- function() {
  cat('Exporting variables to workers...')
  if (exists('.LOCALCLUST')) {
    # Export parent environment
    clusterExport(cl=.LOCALCLUST, 
                  varlist=ls(name=parent.frame()),
                  envir=parent.frame())
    # Export global environment
    clusterExport(cl=.LOCALCLUST, 
                  varlist=ls(name=globalenv()),
                  envir=globalenv())
    # Export loaded packages
    for (pkg in .STARTPKGS) {
      clusterExport(.LOCALCLUST,"pkg",envir=environment())
      clusterEvalQ(cl=.LOCALCLUST,library(pkg,character.only=TRUE))
    }
  }
  cat(' done\n')
}
.clearEnv <- function(...) {
  if (exists('.LOCALCLUST')) {
    clusterEvalQ(cl=.LOCALCLUST,rm(list=ls()))
    clusterEvalQ(cl=.LOCALCLUST,gc())
  } else {
    message('No cluster registered to name .LOCALCLUST')
  }
}

# Returns local cluster if existent
.getLocalClust <- function(clustname='.LOCALCLUST',no=FALSE) {
  if (exists(clustname)) {
    return(get(clustname)) 
  } else {
    return(no) # we could argue on what to retrieve here
  }
}

## Setting up directories
.S_mvb(1,'Setting working directory to ',.S_BASEDIR)
setwd(.S_BASEDIR)

## Load libraries
# Load functions
.S_mvb(1,'Loading functions in ',.S_LIBDIR)
.S_nobj.before <- length(ls())
for (i in dir(.S_LIBDIR,pattern='.*\\.(R|r)$',
              full.names=TRUE,
              recursive=TRUE)) { 
  source(i,chdir=FALSE)
}
.S_mvb(2,'Loaded ',length(ls()) - .S_nobj.before,' new objects')

## Load variables
.S_mvb(1,'Importing variables defined in ',.S_DEFDIR)
.S_nobj.before <- length(ls())
for (i in dir(.S_DEFDIR,pattern='.*\\.(R|r)$',
              full.names=TRUE,
              recursive=TRUE)) { 
  source(i,chdir=FALSE)
}
.S_mvb(2,'Loaded ',length(ls()) - .S_nobj.before,' new objects')

## Set up caching
.S_mvb(1,'Setting up cache')
if (.S_CACHE) {
  .S_libinst(c('R.utils','R.cache','digest'))
  
  if (!file.exists(.S_CACHEDIR)) {
  .S_mvb(4,.S_CACHEDIR,' non existent, creating it.')
    mkdirs(.S_CACHEDIR)
  }
  
} else {
  .S_mvb(2,'Cache disabled')
}

## Launch external tools
if (length(.S_EXTTOOLS)>=1) {
  .S_mvb(1,'Launching tools')
  for (script in .S_EXTTOOLS) {
    .S_mvb(3,'Running ',script)
    system(.S_EXTTOOLS,
           ignore.stdout=(.S_VERBOSE>=5),intern=FALSE,wait=FALSE)
  }
}


# Code below contains functions to be used in scripts
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Memoizing functions
FC <- fcache <- function(fun,...,
                   cache.dir='./tmp/cache',  # caching directory
                   cache.ignore=FALSE,  # ignore cache, always compute result
                   cache.clear=FALSE,
                   verbose=FALSE) {  # other args passed to function
# fcache will analyse the call to a function, and return the function result 
# either by calculting it on the fly, or using the result already stored in a 
# file if available
# 
# DO NOT USE fcache FOR STOCHASTIC FUNCTIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# 
  setCacheRootPath(cache.dir)
  
  # Compute cache key by eval'ing all arguments and the passed function itself.
  # => If either the function or one of the call arguments has changed, the 
  # result is recomputed.
  args.all    <- as.list(match.call(expand.dots=FALSE))
  passed.args <- lapply(as.list(args.all[['...']]),eval,envir=parent.frame())
  passed.fun  <- deparse(eval(fun)) # we work on the character representation
  cache.key   <- list(fun=passed.fun,args=passed.args)
  
  # Build subdirectory corresponding to function name
  passed.fun.name <- as.character(args.all$fun)
  
  if (cache.clear) {
    cat('Removing cache files...\n')
    sapply(dir(cache.dir,pattern='Rcache',full.names=TRUE), file.remove)
    return(invisible(NULL))
  }
  
  if (verbose) {
    cat('Looking up result for ',
        as.character(args.all[[2]]),
        ' [key:',digest(cache.key),']',sep='')
  }
  
  fun.result <- NULL  # init
  
  # Try to load cache for given expression
  if (!cache.ignore) {
    fun.result <- loadCache(cache.key,dirs=passed.fun.name)
  }
  if (verbose && !is.null(fun.result)) {
    cat(' -- OK\n')
  }
  
  # Compute result if no cached result is available
  if (is.null(fun.result)) {
    if (verbose) cat(' -- Nothing found\n')
    fun.result <- fun(...)
    saveCache(fun.result,key=cache.key,dirs=passed.fun.name)
  }
  
  return(fun.result)
}

## Abbreviate source
sc <- source

## Inspect element in widened pager
insp <- function(obj,wide.width=9999) { 
  width.old <- options('width')$width
  if (class(obj) == 'data.frame' && ncol(obj)>6)
    options(width=wide.width) # for large tables
    
  page(obj,method='print')
  options(width=width.old)
}

## Little functions useful in graph formulas
# paste two factors together
p <- function(...,sep='-') paste(...,sep=sep)
# standardize
std <- function(X,na.rm=TRUE) 
  (X - mean(X,na.rm=na.rm)) / sd(X,na.rm=na.rm) 
# scale to 0-1
scl <- function(X,na.rm=TRUE) 
  (X - min(X,na.rm=na.rm)) / max(X,na.rm=na.rm) 

# That's all !
# 

## Clean up
.S_mvb(4,"Cleaning up")
rm(list=ls(all.names=TRUE,pattern='^\\.S_'))
