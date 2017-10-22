### Construct source finder BUGS/JAGS model
### using uniform prior, i.e. p(q) ~ U(qLower, qUpper)
### using dcat() location prior, i.e. some location with 0 possibility
### date: 2016-10-27
library(R2WinBUGS)
library(coda)
sf <- function(){
        # likelihood
        ij <- (i-1) * nj + j    # grid location
        for(m in 1 : M) {
                a[m] <- H[m,ij]
                c[m] <- a[m] * q
                mu[m] ~ dnorm(c[m],tau[m])
                #mu[m] ~ dt(c[m],tau[m],2)
        }
        # prior
        # location prior
        Mi ~ dcat(pi[])
        i <- iCat[Mi]
        x <- xc[i]
        Mij ~ dcat(pij[1:nj,i])
        j <- jCat[Mij]
        y <- yc[j]
        #
        log.q ~ dunif(logqLower,logqUpper)
        q <- 10^log.q
        #q ~ dunif(0,100)
}
sf.path <- file.path(getwd(), "sf.bug")
write.model(sf, sf.path)