### plot the results of JAGS simulation
### date: 2016-09-14
### run sf.runJags.r fisrt.
library(plot3D)
library(IDPmisc)

#traceplot(sf.sim)

## smooth scatter of i and j
#with(sf.sim$BUGSoutput$sims.list, smoothScatter(i,j,xlim = c(0,ni), ylim = c(0,nj)))
#with(sf.sim$BUGSoutput$sims.list, smoothScatter(x,y,xlim = c(min(meshx),max(meshx)), ylim = c(min(meshy),max(meshy))))


## Gelman plot
#gelman.plot(sf.coda)

## pixel density plot
#with(sf.sim$BUGSoutput$sims.list, iplot(i, j ,pixs=1))
with(mcmc, iplot(x, y ,pixs=1))

## histogram
par(mfrow = c(1,1))
#with(sf.sim$BUGSoutput$sims.list, hist(i,breaks = ni,xlim = c(0,ni)))
#with(sf.sim$BUGSoutput$sims.list, hist(j,breaks = nj,xlim = c(0,nj)))
with(sf.sim$BUGSoutput$sims.list, hist(q,breaks = 300,xlim=c(0,10)))
abline(v=1.67, col= 'red', lwd = 4)
with(sf.sim$BUGSoutput$sims.list, hist(Sct,xlim=c(0.15,1.35),breaks=n.Sct+2))

with(sf.sim$BUGSoutput$sims.list, hist(x,breaks = 200,xlim=c(-2.5,1.5)))
abline(v=0, col= 'red', lwd = 4)
with(sf.sim$BUGSoutput$sims.list, hist(y,breaks = 200,xlim=c(-2,6)))
abline(v=1.5, col= 'red', lwd = 4)

## boxplot
#with(sf.sim$BUGSoutput$sims.list, boxplot(q, log = 'y'))

## standard deviation
#print(with(sf.sim$BUGSoutput$sims.list, sd(j)))
