#!/usr/bin/R -f
# 
# 
# Simple example for rollply.r: make the average of a random walk
# 
# 

library(sp)
data(meuse)


# Parameters
RW_LENGTH <- 1000
RW_NA_ADDED <- 500

RW <- rbinom(RW_LENGTH,1,.5) 
RW[RW==0] <- -1
RW <- cumsum(RW) + runif(RW_LENGTH,0,4)
RW[sample(length(RW),RW_NA_ADDED)] <- NA

# Build input df
RW <- data.frame(time=seq_along(RW), 
                 x=rnorm(RW_LENGTH),
                 y=rnorm(RW_LENGTH),
                 position=RW,
                 grp=ifelse(rnorm(RW_LENGTH)>0,'yes','no'))

# Inspect input
# Build moving average
.exportEnv()
df <- rollply(meuse, ~ x+y, 100, 
              function(dat) {  },
              npts=30,
              .progress='time',
              .parallel=TRUE)
qplot(x,y,fill=div,geom='tile',data=df)

ggplot(RW,aes(time,position)) + 
  facet_grid(~grp) + 
  geom_point() + ??
  geom_line(data=df,color='red')
