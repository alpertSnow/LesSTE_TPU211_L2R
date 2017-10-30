### produce the probVec for joint probability of q vs x1
library(gplots)
library(fields)
hist2d_breaks = function (x, y = NULL, nbins = 200,same.scale = FALSE, na.rm = TRUE, 
                          show = TRUE, col = c("black", heat.colors(12)), FUN = base::length, 
                          xlab, ylab,x.breaks,y.breaks, legend,...) 
{
        if (is.null(y)) {
                if (ncol(x) != 2) 
                        stop("If y is ommitted, x must be a 2 column matirx")
                y <- x[, 2]
                x <- x[, 1]
        }
        if (length(nbins) == 1) 
                nbins <- rep(nbins, 2)
        nas <- is.na(x) | is.na(y)
        if (na.rm) {
                x <- x[!nas]
                y <- y[!nas]
        }
        else stop("missinig values not permitted if na.rm=FALSE")
        if(same.scale){
                x.cuts = x.breaks;
                y.cuts = x.breaks;
        }else{
                x.cuts <- x.breaks
                y.cuts <- y.breaks   
        }
        
        
        index.x <- cut(x, x.cuts, include.lowest = TRUE)
        index.y <- cut(y, y.cuts, include.lowest = TRUE)
        m <- tapply(x, list(index.x, index.y), FUN)
        if (identical(FUN, base::length)) 
                m[is.na(m)] <- 0
        if (missing(xlab)) 
                xlab <- deparse(substitute(xlab))
        if (missing(ylab)) 
                ylab <- deparse(substitute(ylab))
        if (show){
                if(legend){
                        image.plot(x.cuts, y.cuts, m, col = col, xlab = xlab, ylab = ylab, 
                                   ...)
                }else{
                        image(x.cuts, y.cuts, m, col = col, xlab = xlab, ylab = ylab, 
                              ...)
                }
        } 
        midpoints <- function(x) (x[-1] + x[-length(x)])/2
        retval <- list()
        retval$counts <- m
        retval$counts_rel <- m/max(m)  
        retval$x.breaks = x.cuts
        retval$y.breaks = y.cuts
        retval$x = midpoints(x.cuts)
        retval$y = midpoints(y.cuts)
        retval$nobs = length(x)
        retval$bins = c(length(x.cuts),length(y.cuts))
        retval$call <- match.call()
        class(retval) <- "hist2d"
        retval
}
# hist2d_breaks(df,x.breaks=seq(0,10,1),y.breaks=seq(-10,10,1),legend=TRUE)
x1 <- mcmc$x
q <- mcmc$q * 0.35
mydata <- cbind(x1,q)
x1.breaks <- meshx
q.binwidth <- 0.02
q.max <- 2
q.bin.n <- q.max/q.binwidth
q.breaks <- seq(0, q.max, q.binwidth)
qc <- seq(q.binwidth/2, q.max-q.binwidth/2, length.out = q.bin.n)

hist2d <- hist2d_breaks(mydata, x.breaks = x1.breaks, y.breaks = q.breaks, legend = TRUE)

counts.mat <- hist2d$counts
counts.mat.normalized <- counts.mat / dx / nrow(mcmc) / q.binwidth
counts.vec <- as.vector(counts.mat.normalized)
xv.q <- rep(xc, times = q.bin.n)
qv.x <- rep(qc, each = ni)

jointDF <- data.frame(x=xv.q,q=qv.x,value=counts.vec)
write.csv(jointDF,'L2R_x1-q_joint.csv', row.names = FALSE)