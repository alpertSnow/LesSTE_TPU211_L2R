### run STE
Sct <- '0.7'
source('01params.R')
source('02construct.R')
source('03runjags.R')
source('05mcmc2probVec.R')
x.wt <- sf.sim$BUGSoutput$mean$x
y.wt <- sf.sim$BUGSoutput$mean$y
q.wt <- sf.sim$BUGSoutput$mean$q
q.sig <- sf.sim$BUGSoutput$sd$q
#output <- data.frame(x=x.wt,y=y.wt,q=q.wt,HPD50=HPD50.wt,HPD95=HPD95.wt,q.sig=q.sig)
output <- data.frame(x=x.wt, x95l=x.95low, x50l=x.50low, x50u=x.50up, x95u=x.95up, 
                 y=y.wt, y95l=y.95low, y50l=y.50low, y50u=y.50up, y95u=y.95up, 
                 q=q.wt, q.sig=q.sig)
print(output)
