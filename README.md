rollply
=======

Rollply is an R package built on top on plyr's **ply functions to make 
moving-window based computations easier. Just provide a function that works on
a data.frame, rollply will take care of building the appropriate subsets and 
apply that function to each of them. 

Some packages out there contain functions that compute moving-window statistics 
(e.g. TTR::SMA), but were not flexible enough for my use and required specific 
time-series classes. Rollply works on generic data.frames and builds upon plyr's
advantages (parallelism, progress report, etc.).

In short, it allows writing something like this:

```r
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
```

![rollply_example: random walk](/examples/random_walk.png?raw=true "Average of a 1D random walk")

Rollply supports groups and 2D moving windows too: 

```r
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
```

![rollply_example: random walk with groups](/examples/random_walk_groups.png?raw=true "Average of 2D random walks")

```r
# Where did people spend their time ?
fixed_mesh <- build_mesh(dat[ ,c('x','y')], 5000) # we fix the mesh across groups
rollav <- rollply(dat, ~ x + y | person, wdw.size=2, mesh=fixed_mesh,
                  summarise, time.spent=length(time))

ggplot(subset(rollav, time.spent>0)) + 
  geom_point(aes(x,y, color=person, size=time.spent)) + 
  facet_grid(~person)
```

![rollply_example: random walk with 2D window](/examples/random_walk_time_spent.png?raw=true "Time spent in each window")


Installation 
----

 - From github (recommended): 
 
 ```r
  install.packages('devtools')
  library(devtools)
  install_github('alexgenin/formulr')
  install_github('alexgenin/rollply')
  library(rollply)
 ```

 - Rollply is also available through R-forge, which might be a better options for
windows users: 

 ```r
  install.packages("rollply", repos="http://R-Forge.R-project.org")
 ```

Rollply is still under heavy development so please be patient in case of 
breakage !

todo
----
 - finish documenting !
 - Allow other outputs (currently we only allow a data.frame output)
 - The implementation of the mesh resolution requires more polishing
 - Maybe implement the subset selection in C++ for 1D moving windows
 - ?
