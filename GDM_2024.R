
#Aproximaci?n al Generalized Dissimilarity Model

library(plyr)
library(dplyr)     # manejo de datos
library(reshape2)
library(gdm)       # modelos de disimilaridad
library(stringr)
library(raster)
library(ggplot2)
library(ggfortify)
library(ggpubr)

#setwd("D:/Dropbox/tesis/")
#setwd("C:/Users/Atila/Dropbox/tesis/")
setwd("C:/Users/user/Dropbox/tesis/")
#setwd("C:/Users/USUARIO/Dropbox/tesis/")
#setwd("/Users/juanluisparra/Dropbox/TESIS/FernandoCediel/Datos_2022")
#archivo con varias columnas extra creadas con anticipaci?n para poder organizar las replicas
datos.1<-read.csv("datos.1.csv", header = TRUE, stringsAsFactors = FALSE)

#todas las localidades muestreadas
local <- unique(datos.1$Location)
local <- local[order(local)]
local_coor <- df
coor <- match(local, datos.1$Location)
local.sp <- data.frame(local, long = datos.1$Longitude[coor], lat = datos.1$Latitude[coor])

proj <- read.csv("local.proj.csv") # Proyectadas a planas
proj.sites <- proj[,2]
local.proj <- cbind(proj.sites, proj[,5:6])

#Not necessary in this version
#convierte la matriz de formato wide a formato long con estas tres columnas 
#trash <- melt(datos.1, id.vars = c("Location", "repeticion","Scientific.Name"), measure.vars="Count")
#convierte el formato long en una tabla organizada siteXspp 
#tabla <- acast(trash,Location~Scientific.Name,fill = 0)

#Cargo la matriz de occ predicha para hacer matriz de dist.

pred.occ<-read.csv("pred_occ_2.csv") ### <-- LOS DATOS SE ESTÁN REPITIENDO A LO LARGO DE LAS FILAS PARA CADA SP

# un solo df para las covariables de ocupacion ya estandarizadas
covs.occ2 <- read.csv("covs.fin.csv")

#categorias de cafe

cats.cafe<-read.csv("cafe_binario.csv", sep = ";")

cats.cafe$categ <- ifelse(cats.cafe$cafe==1,1,0)

covs.occ2$categories <- cats.cafe$categ

# site X spp + covs
sitecov <- cbind(proj.sites, covs.occ2[,2:6])
sitespp <- cbind(pred.occ[,2:74], local.proj) #este num paso 75 a 74, hay una especie menos

####### boxplots de las covariables para ver diferencias entre cafe y nocafe

plot(x=1, y=1, xlim=c(0,9), ylim=c(-5,5), xaxt="n", type="n")
boxplot(elev.mean.1 ~ categories, sitecov, main="Elevation", add=TRUE, at=1:2)
boxplot(ch.med ~ categories, sitecov, main="Canopy height", add=TRUE, at=3:4)
boxplot(lhfi.med.3 ~ categories, sitecov, main="LHFI", add=TRUE, at=5:6)
boxplot(fhdpai ~ categories, sitecov, main="FHD", add=TRUE, at=7:8)
abline(h=0, lty = "dashed")

# Matriz de distancias

dist.loc <- pointDistance(local.proj[,2:3], longlat=FALSE)

distancias <- dist.loc[lower.tri(dist.loc, diag = FALSE)]

#tabla en formato de columnas para las distancias
pairs <- combn(seq(1,153), 2)
dist.eu.loc <- data.frame(punto1 = pairs[1,], punto2= pairs[2,])

# GDM

# Preparaci?n de los datos para el GDM

gdmtab <- formatsitepair(sitespp, bioFormat = 1, dist = "bray", abundance = T, 
                         siteColumn = "proj.sites", XColumn = "long.1", 
                         YColumn = "lat.1", predData = sitecov)

# OCC + covs + cafe
gdm.1 <- gdm(gdmtab, geo = T)
str(gdm.1)
summary(gdm.1)

filas<-rownames(gdmtab)

dist.eu.loc$cat.1<-cats.cafe$categ[dist.eu.loc[,1]]
dist.eu.loc$cat.2<-cats.cafe$categ[dist.eu.loc[,2]]

dist.eu.loc$observed<-gdm.1$observed
dist.eu.loc$predicted<-gdm.1$predicted
dist.eu.loc$ecological<-gdm.1$ecological
dist.eu.loc$residuals<-(gdm.1$predicted - gdm.1$observed)

# graficos del GDM

#comparaciones de categorias

#plot(x=1, y=1, xlim=c(0,3), ylim=c(0,1), type="n")
#boxplot(subset(dist.eu.loc, cat.1==0 & cat.2==0, select=observed), add=TRUE, at=1) # coffee
#boxplot(subset(dist.eu.loc, cat.1==1 & cat.2==1, select=observed), add=TRUE, at=2) # not coffee

gdmtab.1 <- formatsitepair(sitespp, bioFormat = 1, dist = "bray", abundance = T, 
                           siteColumn = "proj.sites", XColumn = "long.1",
                           YColumn = "lat.1", predData = sitecov)
# OCC + covs + cafe
gdm.2 <- gdm(gdmtab.1, geo = T)
str(gdm.2)
summary(gdm.2)

filas<-rownames(gdmtab.1)

dist.eu.loc$cat.1<-cats.cafe$categ[dist.eu.loc[,1]]
dist.eu.loc$cat.2<-cats.cafe$categ[dist.eu.loc[,2]]

dist.eu.loc.2<-dist.eu.loc

dist.eu.loc.2$observed<-gdm.2$observed
dist.eu.loc.2$predicted<-gdm.2$predicted
dist.eu.loc.2$ecological<-gdm.2$ecological
dist.eu.loc.2$residuals<-(gdm.2$predicted - gdm.2$observed)

# con los residuales

#plot(x=1, y=1, xlim=c(0,3), ylim=c(-0.3,0.3), type="n",
 #    xlab="No_cafe - Cafe")
#boxplot(subset(dist.eu.loc.2, cat.1==0 & cat.2==0, select=residuals), add=TRUE, at=1) #no cafe
#boxplot(subset(dist.eu.loc.2, cat.1==1 & cat.2==1, select=residuals), add=TRUE, at=2) #cafe
#abline(h=0, lty = "dashed")

#predicted X observed
#plot(dist.eu.loc$predicted, dist.eu.loc$observed, col="darkgrey",
 #    abline(lm(dist.eu.loc$observed ~ dist.eu.loc$predicted), col = "black"))
#points(subset(dist.eu.loc, cat.1==1 & cat.2==1, select=c(predicted, observed)), col ="brown") # coffee
#points(subset(dist.eu.loc, cat.1==0 & cat.2==0, select=c(predicted, observed)), col ="green") # not coffee

#ecological X observed
#png("gdm_fin_1.png", width = 1000, height = 600)
#par(bg ="antiquewhite")
#plot(dist.eu.loc$ecological, dist.eu.loc$observed, col = "black", pch=5,
 #    xlab = "Predicted Ecological Distance", ylab = "Observed Ecological Distance")
#points(subset(dist.eu.loc, cat.1==1 & cat.2==1, select=c(ecological, observed)), col ="brown", pch=5)  #coffee
#points(subset(dist.eu.loc, cat.1==0 & cat.2==0, select=c(ecological, observed)), col ="green", pch=5)  #not coffee
#abline(lm(dist.eu.loc$observed ~ dist.eu.loc$ecological), col = "orange", lwd=2)
#legend("bottomright", legend = c("Coffee","Not Coffee", "Both"),
 #      col = c("brown","green", "black"), cex = 1, title = "Categories", bty = "n", pch = 5)
#dev.off()

length(gdm.2$predictors)
#plot(gdm.2)
#dev.off()

gdm.1.splineDat <- isplineExtract(gdm.1)
str(gdm.1.splineDat)

#plot(gdm.1.splineDat$x[,"Geographic"], gdm.1.splineDat$y[,"Geographic"], lwd=3,
 #    type="l", xlab="Geographic distance", ylab="Partial ecological distance")

gdm.2.splineDat <- isplineExtract(gdm.2)
str(gdm.2.splineDat)
elev.scale <- gdm.2.splineDat$x[,"elev.mean.1"]
elev.unscaled <- unscale(elev.scale, center = 1778.595, scale = 158.1719)
elev.unscaled <- as.vector(elev.unscaled$V1)

fhd.unscaled <- unscale(gdm.2.splineDat$x[,"fhdpai"],center = 2.7420663,scale = 0.13980579)
fhd.unscaled <- as.vector(fhd.unscaled$V1)

lhfi.unscaled <- unscale(gdm.2.splineDat$x[,"lhfi.med.3"], center = 59.39542, scale = 5.083401)
lhfi.unscaled <- as.vector(lhfi.unscaled$V1)

ch.unscaled <- unscale(gdm.2.splineDat$x[,"ch.med"], center = 19.6980209, scale = 3.55824937)
ch.unscaled <- as.vector(ch.unscaled$V1)

# 
#jpeg("gdm_occu.jpg", width = 2300, height = 1800, units = "px", 
 #       pointsize = 12, quality = 100, res = 288)
par(mfrow=c(2,3), mar=c(5,5,4,2)+0.1)
# graph_names <- c("EL","FHD","CH","LHFI","Geo Dist")
# #Spline Elevation
plot(elev.unscaled, gdm.2.splineDat$y[,"elev.mean.1"], lwd=3,
      type="l", xlab="Elevation distance", ylab="Partial Elevation distance", 
      ylim=c(0,0.4),cex.lab=1.5)
# 
# #Spline Foliage height diversity
plot(fhd.unscaled, gdm.2.splineDat$y[,"fhdpai"], lwd=3,
      type="l", xlab="Foliage height diversity distance", ylab="Partial fhdpai", 
      ylim=c(0,0.4),cex.lab=1.5)
# 
# #Spline Canopy height
plot(ch.unscaled, gdm.2.splineDat$y[,"ch.med"], lwd=3,
      type="l", xlab="Canopy height distance", ylab="Partial Canopy height distance", 
      ylim=c(0,0.4),cex.lab=1.5)
# 
# #Spline Human Footprint
plot(lhfi.unscaled, gdm.2.splineDat$y[,"lhfi.med.3"], lwd=3,
      type="l", xlab="Human Footprint Distance", ylab="Partial Human Footprint Distance", 
      ylim=c(0,0.4),cex.lab=1.5)
# 
# #spline categories
plot(gdm.2.splineDat$x[,"categories"], gdm.2.splineDat$y[,"categories"], lwd=3,
     type="l", xlab="Categories Distance", ylab="Partial Categories Distance",
     ylim=c(0,0.4),cex.lab=1.5)
# #Spline geographic
plot(gdm.2.splineDat$x[,"Geographic"], gdm.2.splineDat$y[,"Geographic"], lwd=3,
      type="l", xlab="Geographic distance", ylab="Partial geographical distance", 
      ylim=c(0,0.4),cex.lab=1.5)

dev.off()

### Volviendo a hacer el GDM con las categor?as ordenadas
### cafe/cafe = 1-1, no-cafe/no-cafe = 1-2, cafe/no-cafe = 1-3
# valor diferente a las tres comparaciones
######## este modelo apoya la homogeneizacion biotica ###########
#################################################################
gdmtab.2 <- gdmtab.1

donde_00<-which(gdmtab.2$s1.categories==0 & gdmtab.2$s2.categories==0, arr.ind = T)
donde_01<-which(gdmtab.2$s1.categories==0 & gdmtab.2$s2.categories==1, arr.ind = T)
donde_10<-which(gdmtab.2$s1.categories==1 & gdmtab.2$s2.categories==0, arr.ind = T)

gdmtab.2$s1.categories[donde_01] <- 1
gdmtab.2$s2.categories[donde_01] <- 3

gdmtab.2$s1.categories[donde_10] <- 1
gdmtab.2$s2.categories[donde_10] <- 3

gdmtab.2$s1.categories[donde_00] <- 1
gdmtab.2$s2.categories[donde_00] <- 2


# OCC + covs + cafe ordenado (cafe/cafe menor)
gdm.3 <- gdm(gdmtab.2, geo = T)
str(gdm.3)
summary(gdm.3)

filas<-rownames(gdmtab.2)

dist.eu.loc.4<-dist.eu.loc

dist.eu.loc.4$cat.1<-gdmtab.2$s1.categories
dist.eu.loc.4$cat.2<-gdmtab.2$s2.categories

dist.eu.loc.4$observed<-gdm.3$observed
dist.eu.loc.4$predicted<-gdm.3$predicted
dist.eu.loc.4$ecological<-gdm.3$ecological
dist.eu.loc.4$residuals<-(gdm.3$predicted - gdm.3$observed)

#jpeg("box_fin.jpg",width = 2000, height = 1500, units = "px", 
 #       pointsize = 12, quality = 100, res = 288)
#plot(x=1, y=1, xlim=c(0,3), ylim=c(-0.1,0.1), type="n",
 #    xlab="No_cafe - Cafe")
#boxplot(subset(dist.eu.loc.4, cat.1==1 & cat.2==2, select=residuals), add=TRUE, at=1) #no cafe
#boxplot(subset(dist.eu.loc.4, cat.1==1 & cat.2==1, select=residuals), add=TRUE, at=2) #cafe
#abline(h=0, lty = "dashed")
#dev.off()
#ecological X observed
#png("gdm_fin_2.png", width = 800, height = 600)

#jpeg("gdm_occ_1.jpg",width = 2500, height = 1500, units = "px", 
 #        pointsize = 12, quality = 100, res = 288)
ord4 <- order(dist.eu.loc.4$predicted)
plot(dist.eu.loc.4$ecological, dist.eu.loc.4$observed, col = "#00000050", bg = "#00000050", pch=21,
     xlab = "Predicted Ecological Distance", ylab = "Observed Dissimilarity", cex=1.3) 
points(subset(dist.eu.loc.4, cat.1==1 & cat.2==1, select=c(ecological, observed)), 
       col= "#d6d53650", bg= "#d6d53650", pch=21, cex = 1.3)  #coffee  
points(subset(dist.eu.loc.4, cat.1==1 & cat.2==2, select=c(ecological, observed)), 
       col = "#bd62d150", bg ="#bd62d150", pch=21, cex = 1.3)  #not coffee
points(x=dist.eu.loc.4$ecological[ord4], y= dist.eu.loc.4$predicted[ord4], col = "#000000", type="l", lwd=2)
#abline(lm(dist.eu.loc.4$observed ~ dist.eu.loc.4$ecological), col = "black", lwd=2)
legend("bottomright", legend = c("Coffee","Not Coffee", "Both"), 
       col= c("#d6d536","#bd62d1", "#000000"), bg=c("#d6d536","#bd62d1", "#000000"), cex = 1.3, 
       title = "Categories", bty = "n", pch = 16)
#dev.off()

#updating the graph with the hist

points_tot <- dist.eu.loc.4$ecological
hist_poi_tot <- density(points_tot)
points_cof <- subset(dist.eu.loc.4, cat.1==1 & cat.2==1, select=c(ecological, observed))
hist_cof_eco <- density(points_cof[,"ecological"])
points_nocof <- subset(dist.eu.loc.4, cat.1==1 & cat.2==2, select=c(ecological, observed))
hist_nocof_eco <- density(points_nocof[,"ecological"])

#jpeg("gdm_occ_hist.jpg",width = 2500, height = 800, units = "px", 
 #   pointsize = 12, quality = 100, res = 288)
plot(hist_poi_tot, main="", ylim= c(0,8), xlab="", ylab="", axes=F)
polygon(hist_poi_tot, col="#000000")
polygon(hist_cof_eco, col="#d6d53690")
polygon(hist_nocof_eco, col="#bd62d190")
#legend("right", legend = c("Coffee","Not Coffee", "Both"), 
 #      fill= c("#d6d536","#bd62d1", "#000000"), title = "Categories", bty = "n")
#dev.off()

#density ecological dissimilarity

points_occy <- dist.eu.loc.4$observed
hist_poi_occy <- density(points_occy)
points_cof_occ2 <- subset(dist.eu.loc.4, cat.1==1 & cat.2==1, select=c(ecological, observed))
hist_cof_eco_occ2 <- density(points_cof_occ2[,"observed"])
points_nocof_occ2 <- subset(dist.eu.loc.4, cat.1==1 & cat.2==2, select=c(ecological, observed))
hist_nocof_eco_occ2 <- density(points_nocof_occ2[,"observed"])

#jpeg("gdm_occ_hist_obs1.jpg",width = 1500, height = 1500, units = "px", 
 #  pointsize = 12, quality = 100, res = 288)
plot(hist_poi_occy, main="", ylim= c(0,12), xlab="", ylab="", axes=F)
polygon(hist_poi_occy, col="#000000")
polygon(hist_cof_eco_occ2, col="#d6d53690")
polygon(hist_nocof_eco_occ2, col="#bd62d190")
#dev.off()

#nf <- layout(matrix(c(2,2,1,1),2,2,byrow = TRUE), c(10,4), c(8,4), TRUE)
#layout.show(nf)

# extract spline data
gdm.3.splineDat <- isplineExtract(gdm.3)
str(gdm.3.splineDat)

jpeg("gdm_all.jpg",width = 2300, height = 1800, units = "px", 
     pointsize = 12, quality = 100, res = 288)
par(mfrow=c(2,3))
par(mar = c(5, 5, 4, 2) + 0.1)


#Spline Elevation
plot(gdm.3.splineDat$x[,"elev.mean.1"], gdm.3.splineDat$y[,"elev.mean.1"], lwd=3,
     type="l", xlab="Elevation distance", ylab="Partial elevation distance", 
     ylim=c(0,0.5), cex.axis= 1.5, cex.lab=1.5)

#Spline Foliage height diversity
plot(gdm.3.splineDat$x[,"fhdpai"], gdm.3.splineDat$y[,"fhdpai"], lwd=3,
     type="l", xlab="Foliage height diversity distance", ylab="Partial fhdpai",
     ylim=c(0,0.5), cex.axis= 1.5, cex.lab=1.5)

#Spline Canopy height
plot(gdm.3.splineDat$x[,"ch.med"], gdm.3.splineDat$y[,"ch.med"], lwd=3,
     type="l", xlab="Canopy height distance", ylab="Partial Canopy height distance", 
     ylim=c(0,0.5), cex.axis= 1.5, cex.lab=1.5)

#Spline Human Footprint
plot(gdm.3.splineDat$x[,"lhfi.med.3"], gdm.3.splineDat$y[,"lhfi.med.3"], lwd=3,
     type="l", xlab="Human Footprint Distance", ylab="Partial Human Footprint Distance", 
     ylim=c(0,0.5), cex.axis= 1.5, cex.lab=1.5)
#spline categories
plot(gdm.3.splineDat$x[,"categories"], gdm.3.splineDat$y[,"categories"], lwd=3,
     type="l", xlab="Categories distance", ylab="Partial Categories", ylim=c(0,0.5)
     , cex.axis= 1.5, cex.lab=1.5)
#Spline geographic
plot(gdm.3.splineDat$x[,"Geographic"], gdm.3.splineDat$y[,"Geographic"], lwd=3,
     type="l", xlab="Geographic distance", ylab="Partial geographical distance", 
     ylim=c(0,0.5), cex.axis= 1.5, cex.lab=1.5)

#mtext("I-splines showing the contribution of each variable", side = 3, line = -2, outer = T)
dev.off()

###### Calculate R2

res_2 <- sum(dist.eu.loc.4$residuals^2) #residuales al cuadrado
mean_2 <- sum((sapply(mean(dist.eu.loc.4$observed), "-", dist.eu.loc.4$observed))^2) #media de obs restado a cada valor de obs
R2 <- 1 - (res_2/mean_2)
# 0.9453235

#######GDM con las categorias de nocafe ######

tipos <- read.csv("cobertura.csv")
dist.eu.loc.5 <- dist.eu.loc.4

donde <- match(dist.eu.loc.5$punto1,tipos$X.)
dist.eu.loc.5$punto1_cob <- tipos$Cobertura[donde]

donde2 <- match(dist.eu.loc.5$punto2,tipos$X.)
dist.eu.loc.5$punto2_cob <- tipos$Cobertura[donde2]

##### ahora a hacer unas cajas!!! 

x_bp <- c("Cafe","Bosque","Cultivo","Potrero")

plot(x=1, y=1, xlim=c(0.5,4.5), ylim=c(-0.1,0.55), type="n",main="comparaciones coberturas (observado)", xlab="Cafe - Bosque - Cultivo - Potrero" )
boxplot(subset(dist.eu.loc.5, punto1_cob=='cafe'& punto2_cob=='cafe', select=observed), add=TRUE, at=1, names="cafe") #cafe/cafe
boxplot(subset(dist.eu.loc.5, punto1_cob=='Bosque'& punto2_cob=='Bosque', select=observed), add=TRUE, at=2) #bosque/bosque
boxplot(subset(dist.eu.loc.5, punto1_cob=='Cultivo'& punto2_cob=='Cultivo', select=observed), add=TRUE, at=3) #Cultivo/Cultivo
boxplot(subset(dist.eu.loc.5, punto1_cob=='Potrero'& punto2_cob=='Potrero', select=observed), add=TRUE, at=4) #Potrero/Potrero
abline(h=0, lty = "dashed")


plot(x=1, y=1, xlim=c(0.5,4.5), ylim=c(-0.1,0.55), type="n",,main="comparaciones coberturas (predicho)", xlab="Cafe - Bosque - Cultivo - Potrero" )
boxplot(subset(dist.eu.loc.5, punto1_cob=='cafe'& punto2_cob=='cafe', select=predicted), add=TRUE, at=1) #cafe/cafe
boxplot(subset(dist.eu.loc.5, punto1_cob=='Bosque'& punto2_cob=='Bosque', select=predicted), add=TRUE, at=2) #bosque/bosque
boxplot(subset(dist.eu.loc.5, punto1_cob=='Cultivo'& punto2_cob=='Cultivo', select=predicted), add=TRUE, at=3) #Cultivo/Cultivo
boxplot(subset(dist.eu.loc.5, punto1_cob=='Potrero'& punto2_cob=='Potrero', select=predicted), add=TRUE, at=4) #Potrero/Potrero
abline(h=0, lty = "dashed")

### Volviendo a hacer el GDM con las categorias ordenadas
### cafe/cafe = 1-1, no-cafe/no-cafe = 1-1, cafe/no-cafe = 1-3
#Equal effects

######### TENERLO A LA MANO!

gdmtab.3 <- gdmtab.1

donde_00<-which(gdmtab.3$s1.categories==0 & gdmtab.3$s2.categories==0, arr.ind = T)
donde_01<-which(gdmtab.3$s1.categories==0 & gdmtab.3$s2.categories==1, arr.ind = T)
donde_10<-which(gdmtab.3$s1.categories==1 & gdmtab.3$s2.categories==0, arr.ind = T)

gdmtab.3$s1.categories[donde_01] <- 1
gdmtab.3$s2.categories[donde_01] <- 2

gdmtab.3$s1.categories[donde_10] <- 1
gdmtab.3$s2.categories[donde_10] <- 2

gdmtab.3$s1.categories[donde_00] <- 1
gdmtab.3$s2.categories[donde_00] <- 3

# OCC + covs + cafe ordenado (cafe/nocafe diferente)
gdm.4 <- gdm(gdmtab.3, geo = T)
str(gdm.4)
summary(gdm.4)

filas<-rownames(gdmtab.3)

dist.eu.loc.5<-dist.eu.loc

dist.eu.loc.5$cat.1<-gdmtab.3$s1.categories
dist.eu.loc.5$cat.2<-gdmtab.3$s2.categories

dist.eu.loc.5$observed<-gdm.4$observed
dist.eu.loc.5$predicted<-gdm.4$predicted
dist.eu.loc.5$ecological<-gdm.4$ecological
dist.eu.loc.5$residuals<-(gdm.4$predicted - gdm.4$observed)

###### Calculate R2

res_2 <- sum(dist.eu.loc.5$residuals^2) #residuales al cuadrado
mean_2 <- sum((sapply(mean(dist.eu.loc.5$observed), "-", dist.eu.loc.5$observed))^2) #media de obs restado a cada valor de obs
R2 <- 1 - (res_2/mean_2)
#[1]  0.9453975

###### GDM without the coffee categories covariates
####### MODELO NULO ##########

# un solo df para las covariables de ocupacion ya estandarizadas
covs.occ3 <- read.csv("covs.fin.csv")

# site X spp + covs
sitecov2 <- cbind(proj.sites, covs.occ3[,2:5])
sitespp2 <- cbind(pred.occ[,2:75], local.proj)

# Matriz de distancias

dist.loc <- pointDistance(local.proj[,2:3], longlat=FALSE)

distancias <- dist.loc[lower.tri(dist.loc, diag = FALSE)]

#tabla en formato de columnas para las distancias
pairs <- combn(seq(1,153), 2)
dist.eu.loc_z <- data.frame(punto1 = pairs[1,], punto2= pairs[2,])

# GDM

# Preparaci?n de los datos para el GDM

gdmtab <- formatsitepair(sitespp2, bioFormat = 1, dist = "bray", abundance = T, 
                         siteColumn = "proj.sites", XColumn = "long.1", 
                         YColumn = "lat.1", predData = sitecov2)
#OCC + covs

gdm.5 <- gdm(gdmtab, geo = T)
str(gdm.5)
summary(gdm.5)

filas<-rownames(gdmtab)

dist.eu.loc$observed<-gdm.1$observed
dist.eu.loc$predicted<-gdm.1$predicted
dist.eu.loc$ecological<-gdm.1$ecological
dist.eu.loc$residuals<-(gdm.1$predicted - gdm.1$observed)

# tabla de devianzas

devi <- c(gdm.1$explained,gdm.3$explained,gdm.4$explained,gdm.5$explained)

### analisis extra para confirmar

library(vegan)

# con la comunidad de aves que tiene modelo de ocupacion

coffee <- replace(covs.occ2$categories, covs.occ2$categories>=1, "coffee")
coffee <- replace(coffee, coffee<1, "notcoffee")
occ_simper <- simper(pred.occ[1:153,2:74], coffee)
summary(occ_simper)
#write.csv(occ_simper)

#simper for coffee points only

coffee_df <- as.data.frame(coffee)
only_coffee <- cbind(pred.occ[,2:74], coffee_df)

coffee_birds <- subset(only_coffee, coffee == "coffee")

coffee_birds_simper <- simper(coffee_birds[,1:73])
summary(coffee_birds_simper)

prueba$total$aporte<-prueba$total$average/sum(prueba$total$average)

##########

pred.imp_occ <- gdm.varImp(gdmtab.2, geo = T, nPerm=50, parallel=T, cores=10, predSelect=T)
pred.importance <- data.frame(value=pred.imp_occ$`Predictor Importance`[,1])
pred.importance <- cbind(covariates=c("Geo Dist","Elevation","CH","LHFI","FHD","Categories"), pred.importance)
#barplot(sort(pred.imp_occ$`Predictor Importance`[,1], decreasing=T), names.arg=c("Elev","FHD","CH","LHFI","Coffee","Geo dist"))

#Make the splines graph once again with the barplot
#
# elevation
elev.data <- data.frame(elev.x=elev.unscaled, 
                        elev.y=as.vector(gdm.3.splineDat$y[,"elev.mean.1"]))

elev.spline <- ggplot(data=elev.data, aes(x=elev.x, y=elev.y)) +
                    geom_line() + 
                    ylim(0, 0.3) +
                    labs(x = "Elevation", y = "Partial Elevation") +
                    theme_classic()
# FHD
fhd.data <- data.frame(fhd.x=fhd.unscaled, 
                        fhd.y=as.vector(gdm.3.splineDat$y[,"fhdpai"]))

fhd.spline <- ggplot(data=fhd.data, aes(x=fhd.x, y=fhd.y)) +
                    geom_line() +
                    ylim(0, 0.3) +
                    labs(x= "Foliage Height Diversity (FHD)", y= "Partial FHD") +
                    theme_classic()
# CH
ch.med.data <- data.frame(ch.med.x=ch.unscaled, 
                          ch.med.y=as.vector(gdm.3.splineDat$y[,"ch.med"]))

ch.med.spline <- ggplot(data=ch.med.data, aes(x=ch.med.x, y=ch.med.y)) +
                        geom_line() +
                        ylim(0, 0.3) +
                        labs(x= "Canopy Height (CH)", y= "Partial Canopy Height") +
                        theme_classic()
# LHFI
lhfi.data <- data.frame(lhfi.x=lhfi.unscaled, 
                        lhfi.y=as.vector(gdm.3.splineDat$y[,"lhfi.med.3"]))

lhfi.spline <- ggplot(data=lhfi.data, aes(x=lhfi.x, y=lhfi.y)) +
  geom_line() +
  ylim(0, 0.3) +
  labs(x= "Human Footprint (LHFI)", y= "Partial LHFI") +
  theme_classic()

# COFFEE
coffee.data <- data.frame(coffee.x=as.vector(gdm.3.splineDat$x[,"categories"]), 
                          coffee.y=as.vector(gdm.3.splineDat$y[,"categories"]))

coffee.spline <- ggplot(data=coffee.data, aes(x=coffee.x, y=coffee.y)) +
  geom_line() +
  ylim(0, 0.3) +
  labs(x= "Coffee Categories", y= "Partial Coffee Categories") +
  theme_classic()

# Geo
geo.data <- data.frame(geo.x=as.vector(gdm.3.splineDat$x[,"Geographic"]), 
                       geo.y=as.vector(gdm.3.splineDat$y[,"Geographic"]))

geo.spline <- ggplot(data=geo.data, aes(x=geo.x, y=geo.y)) +
  geom_line() +
  ylim(0, 0.3) +
  labs(x= "Geographic distance", y= "Partial Geographic Distance") +
  theme_classic()

# Barplot varImp
barplot(sort(pred.imp_occ$`Predictor Importance`[,1], decreasing=T), names.arg=c("Elev","FHD","CH","LHFI","Coffee","Geo dist"))

barras <- ggplot(pred.importance, aes(x=reorder(covariates, -value), y=value))+
                geom_bar(stat = "identity") +
                theme_classic() +
                theme(text = element_text(size=20),
                  axis.text.x = element_text(angle=90, hjust=1))  +
                labs(x= "covariates", y="Frequency")

vp <- grid::viewport(width = 0.7, height = 0.6, x = 0.5, y = 0.65) #crear el viewport

print(geo.spline)+print(barras, vp = vp)

geo.spline_barras <- function(){
  +   print(geo.spline) 
  +   print(barras, vp = vp)
  }

### Complete graph

splines <- ggarrange(elev.spline, fhd.spline, ch.med.spline, lhfi.spline, 
                     coffee.spline, geo.spline,
                     ncol=3, nrow = 2) #NOT WORKING PROPERLY! AHHHH!

jpeg("splines_update.jpg",width = 2300, height = 1800, units = "px", 
    pointsize = 12, quality = 100, res = 288)
ggarrange(elev.spline, fhd.spline, ch.med.spline, lhfi.spline, 
          coffee.spline, geo.spline,
          ncol=3, nrow = 2)
dev.off()

jpeg("barras_update.jpg",width = 2000, height = 2300, units = "px", 
     pointsize = 12, quality = 100, res = 288)
ggplot(pred.importance, aes(x=reorder(covariates, -value), y=value))+
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(text = element_text(size=40),
        axis.text.x = element_text(angle=65, hjust=1))  +
  labs(x= "covariates", y="Importance")
dev.off()
