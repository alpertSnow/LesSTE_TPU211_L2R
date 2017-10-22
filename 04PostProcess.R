### Post process of SctFinder

mcmc <- data.frame(sf.sim$BUGSoutput$sims.list)
write.csv(mcmc,paste0('mcmc',Sct,'.csv'))

# random coordinates
mcmc$xr <- mapply(runif,1, xc[mcmc$i] - 0.5*dx[mcmc$i],xc[mcmc$i] + 0.5*dx[mcmc$i])
mcmc$yr <- mapply(runif,1, yc[mcmc$j] - 0.5*dy[mcmc$j],yc[mcmc$j] - 0.5*dy[mcmc$j])
x.est <- mean(mcmc$x)
y.est <- mean(mcmc$y)
q.est <- mean(mcmc$q)

#mcmc <- mcmc[mcmc$Sct==0.6,]
mu.mcmc <- matrix(nrow = M, ncol = nrow(mcmc))
for (k in 1:nrow(mcmc)) {
        #mu.mcmc[,k] <- H[, ,as.integer((mcmc[k,]$i-1)*nj+mcmc[k,]$j)]*mcmc[k,]$q
        mu.mcmc[,k] <- H[ ,as.integer((mcmc[k,]$i-1)*nj+mcmc[k,]$j)]*mcmc[k,]$q
}
mu.est <- rowMeans(mu.mcmc)
mu.obs <- mu
mu.pre <- H[,(i.real-1)*nj+j.real]*q.real

plot(mu, type= 'l')
lines(mu.est, lty = 2, col = 'red')
lines(mu.pre, lty =2, col = 'blue')

thres <- 0.04 # detector limit
#n0 <- which(mu.obs < thres & mu.pre < thres) 
n.over <- which(mu.obs > thres | mu.pre > thres)
co <- mu.obs[n.over]
cp <- mu.est[n.over]
co <- pmax(co, thres)
cp <- pmax(cp, thres)

FB <- (mean(co-cp))/0.5/(mean(co)+mean(cp))
MG <- exp(mean(log(co))-mean(log(cp)))
NMSE <- mean((co-cp)^2)/mean(co)/mean(cp)
VG <- exp(mean((log(co)-log(cp))^2))
COR <- mean((co-mean(co))*(cp-mean(cp)))/sd(cp)/sd(co)
FAC2 <- sum(cp/co>=0.5 & cp/co<=2)/M
NAD <- (mean(abs(co-cp)))/(mean(co)+mean(cp))
IA <- 1 - sum( (co - cp)^2) / sum( ( abs(cp - mean(co)) + abs(co - mean(co)) )^2 )
print(data.frame(FB=FB,NMSE=NMSE,MG=MG,VG=VG,R=COR,FAC2=FAC2,NAD=NAD, IA=IA))
