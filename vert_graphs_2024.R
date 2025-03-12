
### RUN ONLY AFTER THE "guion_2024.R" script

### vertical graphs with the averaged models and the forest dependence colors
library(ggpubr)
setwd("C:/Users/Atila/Dropbox/tesis/")

prom.coefs <- read.csv("coefficients_2024.csv")

group.colors <- c(yes="#00BFC4",none='#F8766D') 

# Graph Canopy Height
height.prom<-subset(prom.coefs,Cov=="ch.med")
height.prom2<-height.prom[order(height.prom$spp_select),]
spp_sub_height<-xspp_sub[xspp_sub$spp_sel %in% height.prom2$spp_select,]
height.prom2$for_dep<-spp_sub_height$forest_dependence
height.prom2$endem<-spp_sub_height$endemismo
height.prom2$spp_end<-spp_sub_height$spp_end
height.prom2<-height.prom2[order(height.prom2$coef),]
#height.prom2<- subset(height.prom2, height.prom2$coef < 1) # efecto negativo

#jpeg("canopy_height_sep.jpg",width = 1600, height = 2000, units = "px", 
#   pointsize = 12, quality = 100, res = 288)

#separated by forest dependency

bos <- subset(height.prom2, height.prom2$for_dep=="yes")
nobos <- subset(height.prom2, height.prom2$for_dep=="none")
#ggplot(height.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
ch_bos<-ggplot(bos, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +  
  ylim(-3,3)+
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_point() +
  #facet_wrap(~for_dep, ncol=1)+
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of canopy height on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8))

ch_nobos<-ggplot(nobos, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +  
  ylim(-3,3)+
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_point() +
  #facet_wrap(~for_dep, ncol=1)+
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of canopy height on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8))

ggarrange(ch_bos, ch_nobos, ncol=1, nrow=2, align = "hv")

# not separated by forest dependency
#jpeg("ch_vert_2.jpg", width = 2000, height = 2800, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
ggplot(height.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Canopy Height on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),axis.text.y = element_text(face = "italic", size = 8))
#dev.off()

#another one with only the spp that have a negative reaction to coffee
# at the bottom of the code

#dev.off()

# Graph foliage height diversity
fhd.prom<-subset(prom.coefs,Cov=="fhdpai")
fhd.prom2<-fhd.prom[order(fhd.prom$spp_select),]
spp_sub_fhd<-xspp_sub[xspp_sub$spp_sel %in% fhd.prom2$spp_select,]
fhd.prom2$for_dep<-spp_sub_fhd$forest_dependence
fhd.prom2$endem<-spp_sub_fhd$endemismo
fhd.prom2$spp_end<-spp_sub_fhd$spp_end
fhd.prom2<-fhd.prom2[order(fhd.prom2$coef),]
#fhd.prom2<-subset(fhd.prom2, fhd.prom2$coef < 1) #spp con efecto negativo
#jpeg("fhd_vert_2.jpg", width = 2000, height = 2800, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
ggplot(fhd.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Foliage Height Diversity on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),axis.text.y = element_text(face = "italic", size = 8))
#dev.off()

# Elevation with SE
elev.prom<-subset(prom.coefs,Cov=="elev.mean.1")
elev.prom2<-elev.prom[order(elev.prom$spp_select),]
spp_sub_elev<-xspp_sub[xspp_sub$spp_sel %in% elev.prom2$spp_select,]
elev.prom2$for_dep<-spp_sub_elev$forest_dependence  
elev.prom2$spp_end<-spp_sub_elev$spp_end
elev.prom2<-elev.prom2[order(elev.prom2$coef),]
#jpeg("elev_vert_2.jpg", width = 2000, height = 2800, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
ggplot(elev.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Elevation on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),axis.text.y = element_text(face = "italic", size = 8))
#dev.off()

# Graph human footprint
lhfi.prom<-subset(prom.coefs,Cov=="lhfi.med.3")
lhfi.prom2<-lhfi.prom[order(lhfi.prom$spp_select),]
spp_sub_lhfi<-xspp_sub[xspp_sub$spp_sel %in% lhfi.prom2$spp_select,]
lhfi.prom2$for_dep<-spp_sub_lhfi$forest_dependence  
lhfi.prom2$spp_end<-spp_sub_lhfi$spp_end
lhfi.prom2<-lhfi.prom2[order(lhfi.prom2$coef),]
#jpeg("lhfi_vert_2.jpg", width = 2000, height = 2800, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
ggplot(lhfi.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "darkgray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Human Footprint on average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8),axis.text.y = element_text(face = "italic", size = 8))
#dev.off()

# Graph coffee
cafe.prom<-subset(prom.coefs,Cov=="cafe")
cafe.prom2<-cafe.prom[order(cafe.prom$spp_select),]
spp_sub_cafe<-xspp_sub[xspp_sub$spp_sel %in% cafe.prom2$spp_select,]
cafe.prom2$for_dep<-spp_sub_cafe$forest_dependence
cafe.prom2$spp_end<-spp_sub_cafe$spp_end

cafe.prom3<-subset(cafe.prom2, cafe.prom2$coef > 0.75 | cafe.prom2$coef < -0.75) #spp con efecto negativo. >0.8 para las spp opuestas
cafe.prom4<-subset(cafe.prom2, (CI_5 > 0 & CI_95 > 0) | (CI_5 < 0 & CI_95 < 0)) #spp con efecto negativo. >0.8 para las spp opuestas

cafe.prom2<-cafe.prom2[order(cafe.prom2$coef),]

cafe_vert <- ggplot(cafe.prom3, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_end, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Coffee plantations on average occupancy", 
       y = "Occupancy", x= "Species")+
  theme_classic()+
  theme(text = element_text(size = 12),axis.text.y = element_text(face = "italic", size = 13))+
  coord_flip()

cafe_tot <- ggplot(cafe.prom2, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_end, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of Coffee plantations on average occupancy", 
       y = "Occupancy", x= "Species")+
  theme_classic()+
  theme(text = element_text(size = 8),axis.text.y = element_text(face = "italic", size = 8))+
  coord_flip()


#jpeg("cafe_vert_2.jpg", width = 2200, height = 2000, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
cafe_vert + geom_point(data = cafe.prom4, aes(x = spp_end, y = coef), 
                       size = 3, shape = 23)
#cafe_tot
#dev.off()

#Predicted occupancy CH and FHD

cafe.spp <- cafe.prom3$spp_select

fhd.filter <- fhd.prom2 %>% filter(spp_select %in%  cafe.spp)

height.filter <- height.prom2 %>% filter(spp_select %in%  cafe.spp)

coef.merge <- merge(fhd.filter, height.filter, by= 'spp_select')

#plot(coef.merge$coef.x, coef.merge$coef.y, main = 'Predicted Occupancy in spp where coffee has a negative effect', 
#     xlab= "Predicted occupancy to Foliage height diversity", 
#     ylab="Predicted occupancy to Canopy height")
#abline(lm(coef.merge$coef.x~coef.merge$coef.y))

library(ggrepel)

#jpeg("occ_covs_6.jpg", width = 2200, height = 1400, units = "px", 
#     pointsize = 12, quality = 100, res = 288)
cafe.merge<- ggplot(coef.merge, aes(x=coef.x, y=coef.y, col=for_dep.x, label = spp_select))+
  geom_hline(yintercept = 0, col = "darkgray")+
  geom_vline(xintercept = 0, col = "darkgray")+
  geom_point(size = 2)+
  #geom_label_repel(data= subset(coef.merge3, spp_select=="Zonotrichia capensis"), size = 3, max.overlaps = 35) +
  #geom_linerange(aes(ymin=coef.y-SE.y, ymax=coef.y+SE.y))+
  #geom_linerange(aes(xmin=coef.x-SE.x, xmax=coef.x+SE.x))+
  geom_linerange(aes(ymin=CI_5.y, ymax=CI_95.y))+
  geom_linerange(aes(xmin=CI_5.x, xmax=CI_95.x))+
  #geom_text_repel(aes(label=ifelse(coef.x>0 & coef.y>0, as.character(spp_select),'')), 
   #         vjust=4, col="black", size=3)+
  #geom_text_repel(aes(label=spp_select),vjust=2, col="black", size=4)+
  scale_color_manual(values=group.colors,name = "Forest dependency")+
  labs(x="Foliage height diversity (FHD) Coefficient", 
       y="Canopy height (CH) Coefficient")+
  theme_classic()

#dev.off()

# Canopy height only with spp that have a negative reaction to coffee

height.filter  #only spp with coffee negative values
#jpeg("heig_filt.jpg",width = 2500, height = 2000, units = "px", 
 #    pointsize = 12, quality = 200, res = 288)
ggplot(height.filter, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of canopy height on birds' average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8)
        ,axis.text.y = element_text(face = "italic", size = 8)) #argument for italics 
#dev.off()

# FHD only with spp that have a negative reaction to coffee

fhd.filter #only spp with coffee negative values
#jpeg("fhd_filt.jpg",width = 2500, height = 2000, units = "px", 
 #    pointsize = 12, quality = 200, res = 288)
ggplot(fhd.filter, aes(x = reorder(spp_end, coef), y = coef, col = for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point() +
  #geom_linerange(aes(x = spp_select, ymin = CI_5, ymax = CI_95)) +
  geom_linerange(aes(x = spp_end, ymin = coef-SE, ymax = coef+SE)) +
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  labs(title = "Effect of FHD on birds' average occupancy", y = "Occupancy",
       x="Species")+
  theme_classic()+
  theme(text = element_text(size = 8)
        ,axis.text.y = element_text(face = "italic", size = 8))
#dev.off()

# #argument for italics 

#Predicted occupancy EL and LHFI

cafe.spp <- cafe.prom3$spp_select

elev.filter <- elev.prom2 %>% filter(spp_select %in%  cafe.spp)

lhfi.filter <- lhfi.prom2 %>% filter(spp_select %in%  cafe.spp)

coef.merge2 <- merge(elev.filter, lhfi.filter, by= 'spp_select') #coffee affected spp
coef.merge3 <- merge(elev.prom2, lhfi.prom2, by= 'spp_select')

#jpeg("lhfi_el_update_3.jpg", width = 2200, height = 1400, units = "px", 
 #   pointsize = 12, quality = 100, res = 288)
ggplot(coef.merge3, aes(x=coef.x, y=coef.y, col=for_dep.x, label = spp_select))+
  geom_hline(yintercept = 0, col = "darkgray")+
  geom_vline(xintercept = 0, col = "darkgray")+
  geom_point(size = 1.5)+ 
  #geom_label_repel(data= subset(coef.merge3, spp_select=="Ortalis columbiana"), size = 3, max.overlaps = 35) +
  geom_linerange(aes(ymin=CI_5.y, ymax=CI_95.y))+
  geom_linerange(aes(xmin=CI_5.x, xmax=CI_95.x))+
  #geom_linerange(aes(ymin=coef.y-SE.y, ymax=coef.y+SE.y))+
  #geom_linerange(aes(xmin=coef.x-SE.x, xmax=coef.x+SE.x))+
  #geom_text_repel(aes(label=ifelse(coef.x>0 & coef.y>0, as.character(spp_select),'')), 
  #         vjust=4, col="black", size=3)+
  #geom_text_repel(aes(label=spp_select),vjust=2, col="black", size=4)+
  scale_color_manual(values=group.colors,name = "Forest dependency")+
  labs(x="Elevation (EL) Coefficient", 
       y="Human Footprint (LHFI) Coefficient")+
  theme_classic()

#dev.off()

###### Density plots #####

cafe_den <- ggplot(cafe.prom2, aes(coef, fill=for_dep))+geom_density(alpha=0.5)+
  labs(x="Coffee Occupancy coefficients", y="Density") +
  geom_vline(xintercept = 0, color = "black")+
  theme_classic()
cafe_den <- cafe_den +  scale_fill_discrete(name = "Forest dependency")
ch_den <- ggplot(height.prom2, aes(coef, fill=for_dep))+ geom_density(alpha=0.5)+
  labs(x="Canopy height (CH) Occupancy coefficients", y="Density") +
  geom_vline(xintercept = 0, color = "black")+
  theme_classic()
ch_den <- ch_den +  scale_fill_discrete(name = "Forest dependency")
fhd_den <- ggplot(fhd.prom2, aes(coef, fill=for_dep))+geom_density(alpha=0.5) +
  labs(x="Foliage height diversity (FHD) Occupancy coefficients", y="Density") +
  geom_vline(xintercept = 0, color = "black")+
  theme_classic()
fhd_den <- fhd_den +  scale_fill_discrete(name = "Forest dependency")
lhfi_den <- ggplot(lhfi.prom2, aes(coef, fill=for_dep))+geom_density(alpha=0.5)+
  labs(x="Human footprint (LHFI) Occupancy coefficients", y="Density") +
  geom_vline(xintercept = 0, color = "black")+
  theme_classic()
lhfi_den <- lhfi_den +  scale_fill_discrete(name = "Forest dependency")
elev_den <- ggplot(elev.prom2, aes(coef, fill=for_dep))+geom_density(alpha=0.5)+
  labs(x="Elevation Occupancy coefficients", y="Density") +
  geom_vline(xintercept = 0, color = "black")+
  theme_classic()
elev_den <- elev_den +  scale_fill_discrete(name = "Forest dependency")

#leg_vacio <- ggplot(elev.prom2, aes(coef, fill=for_dep))+
#  geom_density(alpha=0.5)+
#  lims(y = c(0,0))+
#  theme_void()+
#  theme(legend.position = c(0.5,0.5),
#        legend.key.size = unit(1, "cm"),
#        legend.text = element_text(size =  12),
#        legend.title = element_text(size = 15, face = "bold"))+
#  guides(colour = guide_legend(override.aes = list(size=8)))

#leg_vacio$labels$fill="Forest Dependency"

library(patchwork)
dens_graph <- cafe_den +
  ch_den +
  fhd_den +
  lhfi_den +
  elev_den +
  guide_area() +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(theme = theme(plot.title = element_text(size = 26)))

#leg <- get_legend(elev_den)

#dens <- ggarrange(cafe_den, ch_den, fhd_den, lhfi_den, elev_den,
 #                 ncol=2, nrow = 3, common.legend = T, legend="bottom")

#jpeg("density2.jpg", width = 2200, height = 2800, units = "px", 
 #    pointsize = 12, quality = 100, res = 288)
dens_graph
#dev.off()

##### Not necessary anymore #####

#### Three graphs (Coffee, FHD and CH) on the same panel 

#ggarrange(coffee.g, ch.g, fhd.g, ncol=3, nrow=1, align = "hv") #this does not work properly

all.data <- rbind(cafe.prom3[,1:7], fhd.filter[,1:7], height.filter[,1:7])

lab.names <-c(cafe="a. Coffee",ch.med="b. Canopy Height",fhdpai="c. FHD")

ordeng <- all.data[all.data$Cov == "cafe",3]
ordeng1 <- order(ordeng)
all.data$sppF <- factor(all.data$spp_select, levels=all.data$spp_select[ordeng1])

#jpeg("three_covs.jpg",width = 2500, height = 2000, units = "px", 
 #   pointsize = 12, quality = 200, res = 288)
ggplot(all.data, aes(sppF, coef, col=for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(size=2.5)+
  facet_grid(~Cov, labeller=as_labeller(lab.names))+
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  geom_linerange(aes(x = spp_select, ymin = coef-SE, ymax = coef+SE)) +
  theme_bw()+
  labs(title = "Effect of the covariates on birds' average occupancy", y = "Occupancy",
       x="Species")+
  theme(plot.title = element_text(size = 14,hjust=0.5),
        axis.text.y = element_text(face = "italic", size = 12), #argument for italics 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank()) 
#dev.off()

#without the coffee data, just in case

nocafe.data <- rbind(fhd.filter[,1:7], height.filter[,1:7])

lab1.names <-c(ch.med="a. Canopy Height",fhdpai="b. FHD")

#jpeg("three_covs.jpg",width = 2500, height = 2000, units = "px", 
#   pointsize = 12, quality = 200, res = 288)
ggplot(nocafe.data, aes(reorder(spp_select, coef), coef, col=for_dep)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point()+
  facet_grid(~Cov, labeller=as_labeller(lab1.names))+
  coord_flip()+
  scale_color_manual(values=group.colors, name = "Forest dependency")+
  theme_bw()+
  labs(title = "Effect of the covariates on birds' average occupancy", y = "Occupancy",
       x="Species")+
  theme(plot.title = element_text(size = 12,hjust=0.5),
        axis.text.y = element_text(face = "italic", size = 8)) #argument for italics 
