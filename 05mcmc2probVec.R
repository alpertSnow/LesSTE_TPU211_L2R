library(plot3D)
library(reshape2)
### mcmc to probVec
mcmc <- data.frame(sf.sim$BUGSoutput$sims.list)
n.mcmc <- dim(mcmc)[1]
probVec <- vector('integer', length = N)
for (k in 1:n.mcmc){
        # probVec[(i - 1) * nj + j] += 1
        probVec[(mcmc$i[k] - 1) * nj + mcmc$j[k]] = 
                probVec[(mcmc$i[k] - 1) * (nj) + mcmc$j[k]] +1
}
probVec = probVec / n.mcmc # normalization
probMat <- t(matrix(probVec, nrow = nj, ncol = ni)) # matrix, ni*nj

## some thing from the old PostProcess.R
i.prob <- apply(probMat,1,sum)
j.prob <- apply(probMat,2,sum)
x.wt <- sum(i.prob * xc)
y.wt <- sum(j.prob * yc)
i.wt <- max(which(xc < x.wt))
j.wt <- max(which(yc < y.wt))
#h.wt <- H[,(i.wt-1)*nj + j.wt]  # h at x_s
#h.est <- H %*% probVec   # posterior mean h 
#cosTheta.wt <- sum(mu/R*h.est)/sqrt(sum(mu^2/R)*sum(h.est^2/R))
#cosT.wt <- sum(mu*h.est)/sqrt(sum(mu^2)*sum(h.est^2))

## real location
x.real <- xc[i.real]
y.real <- yc[j.real]
# EL: distance between real location and estimated location
EL.wt <- sqrt((x.wt - x.real)^2 + (y.wt-y.real)^2)

## location HPD
## 50% and 95% HPD calculation
DF <- data.frame(n=c(1:N),prob=probVec)
DF <- DF[order(DF$prob),]
DF$cumProb <- cumsum(DF$prob)
DF$HPD50 <- DF$cumProb > 0.5
DF$HPD95 <- DF$cumProb > 0.05
#DF$prob0 <- DF$prob == 0
DF <- DF[order(DF$n),]

HPD50 <- t(matrix(DF$HPD50, nrow = nj, ncol = ni))
HPD95 <- t(matrix(DF$HPD95, nrow = nj, ncol = ni))
#prob0 <- t(matrix(DF$prob0, nrow = nj, ncol = ni))
HPD50.wt <- dx %*% HPD50 %*% dy
HPD95.wt <- dx %*% HPD95 %*% dy

HPD <- HPD50+HPD95 #-prob0

## x, y, q CI HPD
## x, y HPD CI
DFx <- data.frame(prob = i.prob, n = 1:i)
DFx <- DFx[order(DFx$prob),]
DFx$cumProb <- cumsum(DFx$prob)
x.95low <- meshx[min(DFx[DFx$cumProb > 0.05, ]$n)]
x.95up <- meshx[max(DFx[DFx$cumProb > 0.05, ]$n)+1]
x.50low <- meshx[min(DFx[DFx$cumProb > 0.5, ]$n)]
x.50up <- meshx[max(DFx[DFx$cumProb > 0.5, ]$n)+1]

DFy <- data.frame(prob = j.prob, n = 1:j)
DFy <- DFy[order(DFy$prob),]
DFy$cumProb <- cumsum(DFy$prob)
y.95low <- meshy[min(DFy[DFy$cumProb > 0.05, ]$n)]
y.95up <- meshy[max(DFy[DFy$cumProb > 0.05, ]$n)+1]
y.50low <- meshy[min(DFy[DFy$cumProb > 0.5, ]$n)]
y.50up <- meshy[max(DFy[DFy$cumProb > 0.5, ]$n)+1]

### the coordianation info about block array
## case 1 & 2
map.cx <- 0
map.cy <- -2.5
## case 4 & 5

### CALCULATION ###
x.block <- c(-2, 0, 2) - map.cx
y.block <- c(-2, 0, 2) - map.cy
xy.mesh <- mesh(x.block, y.block)
x.mesh <- as.vector(xy.mesh$x)
y.mesh <- as.vector(xy.mesh$y)

### reshape data
m.HPD <- melt(HPD)
## count x first, then count y

xv <- rep(xc, times = nj)
yv <- rep(yc, each = ni)
w <- rep(dx, times = nj)
h <- rep(dy, each = ni)
m.prob <- melt(probMat)/w/h
#m.prob0 <- melt(prob0)

## output 3-column csv
HPD_DF <- data.frame(x=xv,y=yv,value=m.HPD$value)
write.csv(HPD_DF,'L2R_HPD.csv', row.names = FALSE)

contourDF <- data.frame(x=xv,y=yv,value=m.prob$value)
write.csv(contourDF,'L2R_contour.csv', row.names = FALSE)