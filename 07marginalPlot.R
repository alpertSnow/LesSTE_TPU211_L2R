## grey marginal plot for Kaiti ppt
### plots of x,y marginal, using ggplot2
### theme_bw style
### date: 2017-05-01
library(ggplot2)
library(gridExtra)

### barplots of marginal posterior of x and y
### pxls: 1500*1500
fmt_decimals <- function(decimals=0){
        # return a function responpsible for formatting the 
        # axis labels with a given number of decimals 
        function(x) formatC( round( x, decimals ), format='f', digits=decimals, width = 2, flag = '0' )
}
italic.text <- element_text(face = "italic", size = 32)
xMargin.plot <- ggplot(data = NULL, aes(x = xc, y = i.prob/dx)) +
        geom_bar(stat = 'identity', width = dx, fill = gray(0.6)) +
        geom_vline(xintercept = x.real, colour = 'red', size=1.2, lty =2) + 
        geom_vline(xintercept = x.wt, colour = 'black', size=1.2, lty =1) +
        theme_bw()+
        labs(x='x/H',y='p(x|μ)') +
        theme(axis.title = italic.text, axis.text = element_text(size = 32)) +
        theme(axis.title.y=element_text(margin = margin(r=0.5, unit = 'cm'))) +
        scale_x_continuous(limits = c(-1.5,1.5), expand = c(0, 0),breaks = seq(-1.5,1.5,0.5)) +
        scale_y_continuous(limits = c(0,6),expand = c(0, 0),breaks = seq(0,6,2),labels=fmt_decimals(0))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(plot.margin = margin(l=0.5, b=0.5,r=1, t=0.5, unit='cm'))+
        theme(axis.text = element_text(colour = 'black'))

yMargin.plot <- ggplot(data = NULL, aes(x = yc, y = j.prob/dy)) +
        geom_bar(stat = 'identity', width = dy, fill = gray(0.6)) +
        geom_vline(xintercept = y.wt, colour = 'black', size=1.2, lty =1) +
        geom_vline(xintercept = y.real, colour = 'red', size=1.5, lty =2) + 
        theme_bw()+
        labs(x='y/H',y='p(y|μ)') +
        theme(axis.title = italic.text, axis.text = element_text(size = 32)) +
        theme(axis.title.y=element_text(margin = margin(r=0.5, unit = 'cm'))) +
        scale_x_continuous(limits = c(-1.5,1.5),expand = c(0, 0),breaks = seq(-1.5,1.5,0.5)) +
        scale_y_continuous(limits = c(0,12),expand = c(0, 0),breaks = seq(0,12,4), labels=fmt_decimals(0))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(plot.margin = margin(l=0.5, b=0.5,r=1, t=0.5, unit='cm'))+
        theme(axis.text = element_text(colour = 'black'))

breaks <- 2000
qHist <- hist(mcmc$q, breaks = breaks)
q.est <- mean(mcmc$q)
q.grid <- qHist$mids
q.probNorm <- qHist$density/sum(qHist$density)
dq <- qHist$breaks[2]-qHist$breaks[1]
q.plot <- ggplot(data = NULL) +
        labs(x = 'q (L/min)',y = 'p(q|μ)') +
        geom_bar(aes(x = q.grid*0.35, y = q.probNorm/dq/0.35), stat = 'identity', width = dq, fill = gray(0.6)) +
        #geom_polygon(aes(x = q.grid, y = q.xs.probNorm/dq), fill='blue', alpha= 0.25)+
        theme_bw()+
        scale_x_continuous(limits = c(0,2),expand = c(0, 0),breaks = seq(0,2,0.4)) +
        scale_y_continuous(limits = c(0,3),expand = c(0, 0),breaks = seq(0,3,1),labels=fmt_decimals(0))+
        theme(axis.title = italic.text, axis.text = element_text(size = 32))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.title.y=element_text(margin = margin(r=0.5, unit = 'cm')))+
        theme(plot.margin = margin(l=0.5, b=0.5,r=1, t=0.5, unit='cm'))+
        theme(axis.ticks = element_line(size = 1.2))+
        geom_vline(xintercept = q.real*0.35, colour = 'red', size=1.2, lty =2)+
        geom_vline(xintercept = q.est*0.35, colour = 'black', size=1.2, lty =1)+
        theme(axis.text = element_text(colour = 'black'))
        #geom_vline(xintercept = q.xs.wt, colour = 'blue', size=1.2, lty =5)

plot(grid.arrange(xMargin.plot, yMargin.plot, q.plot, nrow=3))
#plot(yMargin.plot)
