#!/usr/bin/R -f
# 
# 
# Simple example for rollply.r: make the average of a random walk
# 
# 

library(ggplot2)
library(plyr)
library(rollply)


# Generate data
dat <- data.frame(time=seq.int(1000),
                  position=cumsum(rnorm(1000,0,10)))

rollav <- rollply(dat, ~ time, wdw.size=10, 
                  summarise, position=mean(position))

ggplot(NULL,aes(time,position)) + 
  geom_point(data=dat) +
  geom_line(color='red', data=rollav)

ggsave('./examples/random_walk.png', width=5, height=3)


# Rollav with groups

# Generate three 2D random walks
dat <- ddply(data.frame(person=c('françois','nicolas','jacques')), ~ person, 
             summarise, 
              time=seq.int(1000),
              x=cumsum(rnorm(1000,0,1)),
              y=cumsum(rnorm(1000,0,1)))

# Smoothed trajectory over ten time-steps
rollav <- rollply(dat, ~ time | person, wdw.size=10, mesh.res=1000,
                  summarise, x=mean(x), y=mean(y))

ggplot(dat,aes(x,y,color=person)) + 
  geom_point(alpha=.5, shape='+') + 
  geom_path(data=rollav) 

ggsave('./examples/random_walk_groups.png', width=6, height=5)


# Where did people spend their time ?
fixed_mesh <- build_mesh(dat[ ,c('x','y')], 5000) # we fix the mesh across groups
rollav <- rollply(dat, ~ x + y | person, wdw.size=2, mesh=fixed_mesh,
                  summarise, time.spent=length(time))

ggplot(subset(rollav, time.spent>0)) + 
  geom_point(aes(x,y, color=person, size=time.spent)) + 
  facet_grid(~person)

ggsave('./examples/random_walk_time_spent.png', width=10, height=4)
