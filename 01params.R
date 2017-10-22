### Load and calculate the parameters used in JAGS
### both for measured data and synthetic data
### for LesSTE TPU211_3.3M, ground source, 10 sensors
### date: 2017-05-01

library(tmvtnorm)
library(R2WinBUGS)
library(coda)
library(R2jags)
library(data.table)
## settings
Sct <- '0.7'
srrChar <- 'SRR'
x.center <- 0.5  # reference center x-coord in CFD model
y.center <- 0.6  # reference center y-coord in CFD model
Zref <- 0.20  # reference height for wind speed (m)

Uref <- 4.2  # reference wind speed (m/s)

Href <- 0.20  # reference length (m)

norm.fac <- Uref * Zref^2  # normalization factor
# norm.fac <- 1.0

header <- as.vector(data.matrix(read.table(paste0(srrChar,Sct,'.TXT'), nrows = 1, skip = 1)))
M <- header[1]
ni <- header[2]
nj <- header[3]
nk <- header[4]
N <- ni * nj * nk
H <- array(0, dim = c(M, N))

srr.file <- paste0(srrChar,Sct,'.TXT')
SRR <- as.vector(data.matrix(fread(srr.file, skip = 2)))
H[,] <- t(matrix(SRR, nrow= N, ncol = M))  # Source-receptor matrix for the i-th Sct value

### measured data
r.info <- data.matrix(fread('RINFO.TXT', nrows = M, skip = 1))
mu <- r.info[,4]/norm.fac # Measurement vector, M
#R <- pmax((mu), 10) # Measuremnet covariance vector, M
R <- (r.info[,5]/norm.fac)^2/4
#R <- rep(3,M)
tau <- 1/R # tau vector, M

### Synthetic data
#i.real <- 87 #for xmin=0.0
i.real <- 68 #for xmin=0.2
j.real <- 55
ij.real <- (i.real-1)*nj + j.real # source location
q.real <- 1.0
#sig.rate <- 0.5 # sigma/mu
#mu <- H[1, ,ij.real] * q.real
#mu <- as.vector(mu + rtmvnorm(1, rep(0,M), diag(mu * sig.rate)))
#R <- (mu * sig.rate)^2
#tau <- 1/R

## cell info
## cell center (xc, yc, xz) and cell width (dx, dy dz) calculation
meshx <- as.vector(as.matrix(fread('MESHX.txt', header = TRUE)))
meshy <- as.vector(as.matrix(fread('MESHY.txt', header = TRUE)))
meshz <- as.vector(as.matrix(fread('MESHZ.txt', header = TRUE)))
meshx <- (meshx-x.center)/Href
meshy <- (meshy-y.center)/Href
xc <- vector(length = ni)
dx <- vector(length = ni)
for(i in 1:ni){
        xc[i] <- (meshx[i]+meshx[i+1])/2
        dx[i] <- meshx[i+1]-meshx[i]
}
yc <- vector(length = nj)
dy <- vector(length = nj)
for(j in 1:nj){
        yc[j] <- (meshy[j]+meshy[j+1])/2
        dy[j] <- meshy[j+1]-meshy[j]
}

## location prior distribution, decomposed into one 1d distribution and a 2d distribution (pi -> pij).
pMat.01 <- matrix(as.numeric(apply(H[,], 2, sum) != 0), nrow = nj, ncol = ni) # 0 if it is impossible to have a prior
pMat <- pMat.01  * (dy %*% t(dx))
pi <- apply(pMat, 2, sum)/sum(apply(pMat, 2, sum))
pij <- apply(pMat, 2, function(x) x/sum(x))  # prior of j after i is chosen from pi
iCat <- 1:ni
jCat <- 1:nj

## emission rate q prior
logqUpper <- 2 # uper limit: 10^x
logqLower <- -2 # lower limit: 10^x