#!/usr/bin/R -f
# 
# 
# Simple example for rollply.r: make the average of a random walk
# 
# 

dat <- data.frame(time=seq.int(1000),
                  position=cumsum(rnorm(1000,0,10)))

rollav <- rollply(dat, ~ time, wdw.size=10, 
                  summarise, position.mean=mean(position))

ggplot() + 
  geom_point(aes(time,position), data=dat) +
  geom_line(aes(time,position.mean), color='red', data=rollav)

ggsave('./examples/random_walk.png', width=7, height=4)
