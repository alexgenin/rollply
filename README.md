rollply
=======

Rollply is an R package built on top on plyr's **ply functions to make 
moving-window based computations easier. Just provide a function that works on
a data.frame, rollply will take care of slicing the dataset into appropriate
subsets and apply that function to each of them. 

In short, it allows writing something like: 
```r
dat <- data.frame(time=seq.int(1000),
                  position=cumsum(rnorm(1000,0,10)))

rollav <- rollply(dat, ~ time, wdw.size=10, 
                  summarise, position.mean=mean(position))

ggplot() + 
  geom_point(aes(time,position), data=dat) +
  geom_line(aes(time,position.mean), color='red', data=rollav)
```

![rollply_example: random walk](/examples/random_walk.png?raw=true "Average of a random walk")

