#!/usr/bin/R -f
# 
# 
# Simple example for rollply.r: make the average of a random walk
# 
# 


# Parameters
RW_LENGTH <- 1000
RW_NA_ADDED <- 500

RW <- rbinom(RW_LENGTH,1,.5) 
RW[RW==0] <- -1
RW <- cumsum(RW) + runif(RW_LENGTH,0,4)
RW[sample(length(RW),RW_NA_ADDED)] <- NA

# Build input df
RW <- data.frame(time=seq_along(RW), position=RW)

# Inspect input


# Build moving average
df <- rollply(RW, ~time, 
              summarise, 
              position=mean(position,na.rm=TRUE),
              wdw=10,
              npts=1000)

ggplot(RW,aes(time,position)) + 
  geom_point() + 
  geom_line(data=df,color='red')
