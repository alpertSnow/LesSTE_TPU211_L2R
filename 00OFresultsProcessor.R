########### OF results processor ##############
### Read .raw file from OF sample surfaces  ###
### Write MESHX(Y&Z).TXT and SRR*.TXT       ###
### Date: 2017-02-27                        ###
### By: xf                                  ###
###############################################

# 对于Ground，缺了个Source的点！！需要手动添加！！

## settings ##
M <- 10  # number of sensors
xlim <- c(0.2, 0.8)
ylim <- c(0.3, 0.9)
tol <- 2e-5 # tolarance
digits <- 5  # decimal places of coordinations
surfaceName <- 'Ground_Surface'
extention <- '.raw'
nk <- 1  # this is for 2-d STE
srrFile <- 'SRR0.7.TXT'

## parameters calculation ##
iM <- 1:M
iM.char <- sprintf('%02d', iM)

## read .raw file
for (i in 1:M){
        inputFile <- paste0('Tr', iM.char[i], '_', surfaceName, extention)
        tempDF <- read.table(inputFile, skip = 2, header = FALSE)
        if (i == 1){
                raw.adjoints <- tempDF$V4
                raw.x <- tempDF$V1
                raw.y <- tempDF$V2
        }
        else{
                raw.adjoints <- cbind(raw.adjoints, tempDF$V4)
        }
}
colnames(raw.adjoints) <- paste0('Tr', iM.char)

## cell center coords
raw.xlevels <- as.numeric(names(table(raw.x)))
raw.ylevels <- as.numeric(names(table(raw.y)))
for (i in 1:length(raw.xlevels)){
        if (i == 1){
                raw.xc <- raw.xlevels[i]
        }
        else if (raw.xc[length(raw.xc)] + tol < raw.xlevels[i] |
                 raw.xc[length(raw.xc)] - tol > raw.xlevels[i]){
                raw.xc <- rbind(raw.xc, raw.xlevels[i])
        }
}
for (i in 1:length(raw.ylevels)){
        if (i == 1){
                raw.yc <- raw.ylevels[i]
        }
        else if (raw.yc[length(raw.yc)] + tol < raw.ylevels[i] |
                 raw.yc[length(raw.yc)] - tol > raw.ylevels[i]){
                raw.yc <- rbind(raw.yc, raw.ylevels[i])
        }
}

## cell size & cell boundary coords
raw.dx <- vector(length = length(raw.xc))
raw.dy <- vector(length = length(raw.yc))
raw.meshx <- vector('numeric', length = length(raw.xc) + 1)
raw.meshy <- vector('numeric', length = length(raw.yc) + 1)
for (i in 1:length(raw.xc)){
        raw.dx[i] <- 2 * (raw.xc[i] - raw.meshx[i])
        raw.meshx[i+1] <- raw.xc[i] + 0.5 * raw.dx[i]
}
for (i in 1:length(raw.yc)){
        raw.dy[i] <- 2 * (raw.yc[i] - raw.meshy[i])
        raw.meshy[i+1] <- raw.yc[i] + 0.5 * raw.dy[i]
}

## possible region (based on cell center coords)
ix.region <- which(raw.xc >= xlim[1] & raw.xc <= xlim[2])
iy.region <- which(raw.yc >= ylim[1] & raw.yc <= ylim[2])
ni <- length(ix.region)
nj <- length(iy.region)

## meshx and meshy: calculate and output
meshx <- raw.meshx[c(ix.region, ix.region[length(ix.region)]+1)]
meshy <- raw.meshy[c(iy.region, iy.region[length(iy.region)]+1)]
meshx <- round(meshx, digits)
meshy <- round(meshy, digits)
write.table(meshx, 'MESHX.TXT', col.names = 'X', row.names = FALSE)
write.table(meshy, 'MESHY.TXT', col.names = 'Y', row.names = FALSE)

## make SRR
adjoints <- matrix(0, nrow = ni*nj, ncol = M)
xc <- raw.xc[ix.region]
yc <- raw.yc[iy.region]
for (i in 1:ni){
for (j in 1:nj){
        l <- which(raw.x < xc[i] + tol & raw.x > xc[i] - tol &
                   raw.y < yc[j] + tol & raw.y > yc[j] - tol)
        if (length(l) > 0){
                adjoints[(i-1)*nj+j,] <- raw.adjoints[l,]
        }
}
}
## for TPU211_3.3M, LES
#i=87 #for xmin=0.0
i=68 #for xmin=0.2
j=55
ij=(i-1)*nj+j
adjoints[ij,]=c(56.927332, 47.684619, 11.12362, 24.351986, 20.627329, 11.052046, 51.275308, 11.522462, 21.178797, 11.371041)
################

SRR <- as.vector(adjoints)

## output SRR.TXT
write.table(t(c('NSEN=','I=','J=','K=')), srrFile, col.names = FALSE, row.names = FALSE)
write.table(t(c(M, ni, nj, nk)), srrFile, col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(SRR, srrFile, col.names = FALSE, row.names = FALSE, append = TRUE)