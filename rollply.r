#!/usr/bin/R
# 
# rollply: applies a function on a data frame over a sliding window. Slow but
#   generic.
# Originally written by Alexandre Genin, 2014, stil uncomplete
# 
# Usage : rollply(.data,form,fun,npts)
#   Arguments
#       .data           a data.frame
#       form            a formula of the form ~b|grp1+grp2
# 
# Note: sometimes parallel computing fails saying that "..." is used in an 
#   incorrect context. This has probably to do with [1] and is unfixed at the 
#   moment.
# 
# 
# [1] https://github.com/hadley/plyr/issues/203
# 
rollply <- function(.data,
                    .vars,
                    fun,
                    .groups=NULL,
                    npts=200,
                    xpts=NULL,
                    wdw.size=NULL,
                    sides='none', 
                    .debug=TRUE,
                    .parallel=FALSE,
                    .progress='none',
                    .inform=FALSE,
                    ...) {  # passed to fun
  require(plyr)
  browser()
  
  # Handle groups: if we provide groups, then we dispatch rollply within each
  # groups.
  if (has_groups(.vars)) {
    # Build new argument lists
    args.grps <- as.list(match.call) 
    args.grps[['.vars']] <- split_groups(.vars)[['vars']]
    args.grps[['.groups']] <- NULL
    
    do.call(rollply, .data, .groups, rollply, 
  }
  
  # Create vector of values
  if (is.null(xpts)) {
    # Get what the padding should be: it determines behaviour when the window 
    # is outside of the X range.
    if (!is.numeric(sides)) {
      pad <- switch(sides,
                    outside = wdw/2,
                    inside  = -wdw/2,
                    none    = 0)
    }
    
    # get X range
    xpts <- seq(min(.data[ ,form$x],na.rm=TRUE) - pad,
                max(.data[ ,form$x],na.rm=TRUE) + pad,
                length.out=npts)
  }
  
  # Create window value if not specified
  if (is.null(wdw.size)) {
    wdw.size <- diff(range(.data[ ,form$x],na.rm=TRUE)) / length(xpts) / 2
    message(paste0('Choosing window automatically: wdw.size=',round(wdw.size,2)))
  }
  
  # Create grouping vector if needed
  gps <- NULL
  if (!is.null(form$gps)) {
    gps <- sapply(form$gps,function(gp) { unique(.data[ ,gp]) }, simplify=FALSE)
  }
  
  # Create groups dfc
  if (!is.null(form$gps)) {
    y.result <- ddply(querydf,form$gps,
                      .getOneResult.group,
                        yvar=form$x,
                        gps=form$gps,
                        db=.data,
                        fun=fun,
                        wdw=wdw,
                        .proggrp=.progress,
                      .inform=.inform,
                      .parallel=.parallel,
                      ...)
  } else {
    y.result <- ddply(querydf,form$x,
                      .getOneResult,
                        yvar=form$x,
                        db.grp=.data,
                        fun=fun,
                        wdw=wdw,
                      .inform=.inform,
                      .parallel=.parallel,
                      .progress=.progress,
                      ...)
  }
  return(y.result)
}


# Given a query df
.getOneResult.group <- function(gridval,yvar,gps,db,fun,wdw,
                                .proggrp='none',...) {
  
  # Get group db subset and applies
  db.grpsub <- sapply(gps,function(gp) { db[ ,gp] == gridval[1,gp] })
  db.grpsub <- apply(db.grpsub,1,all)
  
  res.grp <- ddply(gridval,yvar,.getOneResult,
                   yvar=yvar,
                   db.grp=db[db.grpsub,],
                   fun=fun,
                   wdw=wdw,
                   .progress=.proggrp,
                   ...)
}

.getOneResult <- function(gridval,yvar,db.grp,fun,wdw,
                          wintype="center",...) {
  
  # Compute subset of values
  yval <- gridval[ ,yvar]  # should be only one value
  db.yval <- db.grp[ ,yvar]
  
  db.subs <- switch(wintype,
                    left=yval - db.yval > wdw,
                    right=yval - db.yval < wdw,
                    center=abs(yval - db.yval) < wdw/2)
  db.subs <- db.subs & !is.na(db.subs)
  
  # Return function applied to that subset
  fun(db.grp[db.subs, ],...)
}

.parseFormula <- function(form) {
  
  # Check sanity of formula
  if (! class(form) == 'formula') 
    stop('Cannot parse a non-formula object')
  
  # some stuff like a+b~c
  form.char <- as.character(form)
  if (length(form.char) < 3) {
    form.char <- c(form.char[1],
                   ".",
                   form.char[2:length(form.char)])
  }
  
  form.char <- unlist(strsplit(form.char[c(2,1,3)],' '))
  
  x.start <- which(form.char=="~") + 1
  x.end <- length(form.char)
  
  hasgroup <- any(form.char=='|')
  gps <- NULL
  if (hasgroup) {
    group.start <- which(form.char=='|') + 1
    group.end   <- length(form.char)
    x.end <- which(form.char=='|') - 1
    gps <- .removePlus(form.char[group.start:group.end])
  }
  
  if(form.char[1]=='.') {
    y <- NULL
  } else {
    y.start <- 1
    y.end   <- x.start - 2
    y <- .removePlus(form.char[y.start:y.end])
  }
  
  x   <- .removePlus(form.char[x.start:x.end])
  
  return(list(y=y,x=x,gps=gps))
}

.removePlus <- function(X) { X[!grepl('\\+|\\*',X)] }
