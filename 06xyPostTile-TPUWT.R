### contour plots of location marginal, using ggplot2
### date: 2017-05-01
library(RColorBrewer)
library(ggplot2)


### contour plot of HPD
### pxls: 1200*1000
italic.text <- element_text(face = "italic", size = 32)
## define jet colormap
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
Gr2Bl <- colorRampPalette(c("#f1eef6", "#74a9cf", "#00007f"))
HPD.plot <- ggplot(data= NULL) +
        labs(x="x/H", y="y/H") +
        geom_tile(aes(x=xv, y=yv, fill=m.HPD$value), width = w, height = h) +
#        stat_contour(aes(z=m.HPD$value), breaks = c(0,1,2), colour = 'black',size = 0.5, linejoin='bevel') +
        geom_point(aes(x=x.wt,y=y.wt),pch=21,colour='black',fill='white', size=4.5)+
        geom_point(aes(x=x.real,y=y.real),pch=19,colour='red', size=4)+
        coord_equal() +
        scale_fill_gradientn(name='p(x,y|μ)',colors = Gr2Bl(3)) +
        geom_tile(aes(x = -0.25, y = 0), width = 0.5, height = 0.5, colour='black', fill = 'white')+
        scale_x_continuous(expand = c(0, 0)) +
#        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_bw() +
        theme(axis.title = italic.text, axis.text = element_text(size = 32))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(legend.text = element_text(size = 24))+
        theme(legend.title = element_text(size = 24, face = 'italic'))+
        theme(legend.key.size = unit(1,"cm"))+
        theme(axis.text = element_text(colour = 'black'))
plot(HPD.plot)

### contour plot of location posterior
### pxls: 1200*1000
locationMargin.plot <- ggplot(data= NULL) +
        labs(x="x/H", y="y/H") +
        geom_tile(aes(x=xv, y=yv, fill=m.prob$value), width = w, height = h) +
        geom_tile(aes(x = -0.25, y = 0), width = 0.5, height = 0.5, color = 'black', fill = 'white')+
        geom_point(aes(x=x.wt,y=y.wt),pch=21,colour='black',fill='white', size=4.5)+
        geom_point(aes(x=x.real,y=y.real),pch=19,colour='red', size=4)+
        coord_equal() +
        scale_fill_gradientn(name='p(x,y|μ)',colors = jet.colors(7)) +
        #scale_fill_gradientn(name='p(x,y|μ)',colors = brewer.pal(7,'PuBu')) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_bw() +
        theme(axis.title = italic.text, axis.text = element_text(size = 32))+
        theme(panel.border = element_rect(fill=NA, colour = "black", size=1.2))+
        theme(axis.ticks = element_line(size = 1.2))+
        theme(legend.text = element_text(size = 24))+
        theme(legend.title = element_text(size = 24, face = 'italic'))+
        theme(legend.key.size = unit(1,"cm"))+
        theme(axis.text = element_text(colour = 'black'))
        
plot(locationMargin.plot)