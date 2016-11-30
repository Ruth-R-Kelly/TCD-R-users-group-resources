setwd("C:/R/Rusers_group_testing/ggplot2rk")
library(ggplot2)

All_mods <- read.csv("pollinator_mods.csv")

summary(All_mods)


### Plot of model coefficients
zp1 <- ggplot(All_mods, aes(colour = Species, shape = Species))

zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/4), lty = 2) + 
        geom_pointrange(aes(x = modelName, y = Coefficient, 
                                 ymin = Confint_L,
                                 ymax = Confint_U),show.legend=T,
                             lwd = 0.5, position = position_dodge(width = 2/3)) + 
      ggtitle("Visiting pollinator abundance") + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_flip() + theme_bw() + 
      theme(axis.title.y=element_blank()) + 
      theme(plot.title = element_text(face="bold", size=15, hjust = 0.5)) + 
#zp1 <- zp1 + scale_colour_manual(values = c("dark red", "tomato2", "goldenrod1"))
#zp1 <- zp1 + scale_colour_manual(values = c("dark blue", "#3182bd", "#9ecae1"))
#zp1 <- zp1 + scale_colour_manual(values = c( "dark green", "#31a354", "#a1d99b"))
#zp1 <- zp1 + scale_colour_grey(start = 0, end = 0.8)
      theme(legend.text = element_text(face = "italic", size = 8)) + 
      theme(axis.text.x = element_text(size = 8)) + 
      theme(axis.text.y = element_text(size = 8)) + 
      theme(axis.title.x = element_text(size = 8)) + 
      theme(plot.title = element_text(size = 11)) + 
      guides(colour = guide_legend(title = NULL))  + 
      guides(shape = guide_legend(title = NULL))


print(zp1)


#dev.off()
