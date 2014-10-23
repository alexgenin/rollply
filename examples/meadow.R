# 
# 
# 
# 

load('./examples/meadow_example.dat')
mdw <- yosemite_meadow[ , c('lon','lat','meanvwc','tcover')]
loc <- apply(mdw[,c('lon','lat')], 2, mean)

library(ggplot2)
library(ggmap)
library(rollply)

# Points surveyed
# ggmap(get_map(loc, zoom=15)) + 
#   geom_point(aes(lon,lat), data=mdw, shape='+')

qplot(lon, lat, data=mdw, shape='+')

mdw.sq <- rollply(mdw, ~ lon + lat, .001, summarise, 
                  mesh.res=200,
                  mesh.type='grid_proportional',
                  meanvwc.mean = mean(meanvwc,na.rm=TRUE),
                  tcover.mean  = mean(tcover,na.rm=TRUE))

mdw.id <- rollply(mdw, ~ lon + lat, .001, summarise, 
                  mesh.res=200,
                  mesh.type='grid_identical',
                  meanvwc.mean = mean(meanvwc,na.rm=TRUE),
                  tcover.mean  = mean(tcover,na.rm=TRUE))

mdw.pf <- rollply(mdw, ~ lon + lat, .001, summarise, 
                  mesh.res=5000,
                  mesh.type='polygon_fill',
                  meanvwc.mean = mean(meanvwc,na.rm=TRUE),
                  tcover.mean  = mean(tcover,na.rm=TRUE),
                  .progress='time')

# Debug mesh paramaters
ggplot(NULL, aes(lon, lat)) +
#   geom_point(data=mdw.sq, shape='+') +
#   geom_point(data=mdw.id, shape=1) +
  geom_point(data=mdw.pf, shape=1) +
  geom_point(data=mdw, color='red') 

ggplot(NULL, aes(lon, lat)) +
#   geom_point(data=mdw.sq, shape='+') +
#   geom_point(data=mdw.id, shape=1) +
  geom_point(data=mdw, color='red') + 
  geom_tile(aes(fill=tcover.mean),data=mdw.pf, shape=1)

mdw.pf <- rollply(mdw, ~ lon + lat, .001, 
                  function(dat) { 
                    data.frame(pval=cor.test(~tcover+meanvwc,data=dat)$p.value)
                  },
                  mesh.res=5000,
                  mesh.type='polygon_fill',
                  .progress='time')

qplot(lon,lat,fill=pval,geom='tile',data=mdw.pf)

      