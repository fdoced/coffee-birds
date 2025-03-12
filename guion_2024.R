#Guíon actualizado a 2024
#paquetes necesarios 

library(lubridate) # manejo de variables de tiempo
library(plyr)
library(dplyr)     # manejo de datos
library(reshape2)  # melt y acast
library(unmarked)  # modelos de ocupacion
library(AICcmodavg)# seleccion de modelos
library(MuMIn)     # automated model selection
library(ggplot2)
library(ggfortify)
library(stringr)
library(corrplot)
library(sp)
library(raster)
library(sf)
library(ggthemes)
library(plot3D)

#setwd("C:/Users/Atila/Dropbox/tesis/")
setwd("C:/Users/user/Dropbox/tesis/")
#setwd("D:/Dropbox/tesis/")
#setwd("/Users/juanluisparra/Dropbox/TESIS/FernandoCediel/Datos_2022")
#archivo con varias columnas extra creadas con anticipacion para poder organizar las replicas (historia de deteccion)
datos.1<-read.csv("datos.1.csv", header = TRUE, stringsAsFactors = FALSE)

#todas las localidades muestreadas
local <- unique(datos.1$Location)
local <- local[order(local)]
local_coor <- df
coor <- match(local, datos.1$Location)
local.sp <- data.frame(local, long = datos.1$Longitude[coor], lat = datos.1$Latitude[coor])

#este paso es para obtener un array donde las filas son las localidades, las columnas las repeticiones,
# y la tercera dimension son las especies. 
#convierte la matriz de formato wide a formato long con estas tres columnas 
trash <- melt(datos.1, id.vars = c("Location", "repeticion","Scientific.Name"), measure.vars="Count")
#convierte el formato long en un array organizado (eje z) por la especie y pone 0s donde no esta la spp
trial <- acast(trash,Location~repeticion~Scientific.Name,fill = 0)
#por culpa de algo esta creando una columna 21, entonces se elimina
trial2 <- trial[,-21,]
dim(trial2) #153 (localidades) 20 (repeticiones) 234 (especies)
#para las temp 3 y 5 necesito dejar solo esas repeticiones

sppnames<- as.vector(dimnames(trial2)[[3]])
rows<- as.vector(dimnames(trial2)[[1]])
rownames1<- c(rows,paste(rows,"1", sep="_"))

trialy <- array(data=NA, dim = c(306,4,225))
dimnames(trialy) <- list(rownames1,1:4,sppnames)

for(i in 1:225){
trialy[,,i] <- rbind(trial2[,9:12,i], trial2[,17:20,i])
}

#orden alfabetico de las spp para saber el # de cada una
spp <- unique(trash$Scientific.Name[order(trash$Scientific.Name)]) 
spp
#write.csv(spp, "spp_totales.csv")
#225 spp en fincas AAA hasta T5

# hasta aqu? est? listo para realizar los modelos

###### Covariables de ocupacion #####

#covariables categoricas
cats.cafe<-read.csv("cafe_binario_1.csv", sep = ";")
vec<-ifelse(cats.cafe$cafe==1,"orange","green")

# un solo df para las covariables de ocupacion ya estandarizadas
#covs.occ<-read.csv("covs.pca.med.csv")
covs.occ<-read.csv("covs.total_1.csv")
covs.occ2<-subset(covs.occ, select= c("elev.mean.1","ch.med","lhfi.med.3"))

sfv.covs.mean<-read.csv("sfv_covs_mean_only_1.csv")
sfv.covs<-scale(sfv.covs.mean[4:8])

covs.occ2<-cbind(covs.occ2, subset(sfv.covs, select=(fhdPai_mean_val)))
names(covs.occ2)[4]<-"fhdpai"
#write.csv(covs.occ2, "covs.fin.csv")

#PCA de covariables
#pca.covs<-prcomp(covs.occ2, center=F, scale. = F)
#summary(pca.covs)
#autoplot(pca.covs, data=covs.occ2, loadings=TRUE, loadings.label=T, 
#         label=T,  center=F, scale=F, col=vec)
#pca.covs$rotation
#pca.covs$x[,1:2]
#local.pca<-cbind(local.sp, pca.covs$x[,1:2])
#coordinates(local.pca)<-local.pca[,2:3]

### Add categorical variables to one file
covs.occ3<-cbind(covs.occ2, cats.cafe[,2])
names(covs.occ3)[5]<-"cafe"

#jpeg("hist_covs.jpg",width = 480, height = 300, units = "px", 
#     pointsize = 12, quality = 200)
#hist(covs.occ3$ch.med, col=rgb(0,0,1,0.2), xlab= NULL, main=NULL)
#hist(covs.occ3$fhdpai, col=rgb(1,0,0,0.2), add=T)
#legend("topright", c("Canopy Height", "FHD"), fill=c(rgb(0,0,1,0.2),rgb(1,0,0,0.2)))
#dev.off()

#jpeg("hist_covs_1.jpg",width = 480, height = 300, units = "px", 
#     pointsize = 12, quality = 200)
#hist(subset(covs.occ3$ch.med, covs.occ3$cafe == 1), col=rgb(0,0,1,0.3), main = NULL, xlab = NULL)
#hist(subset(covs.occ3$fhdpai, covs.occ3$cafe == 1), col=rgb(1,0,0,0.3), add=T  )
#hist(subset(covs.occ3$ch.med, covs.occ3$cafe == 0), col=rgb(0,0,1,0.6), main=NULL, xlab= NULL, add=T)
#hist(subset(covs.occ3$fhdpai, covs.occ3$cafe == 0), col=rgb(1,0,0,0.6), add=T  )
#legend("topright", c("CH-coffee", "FHD-coffee","CH-no coffee","FHD-no coffee"), fill=c(rgb(0,0,1,0.3),rgb(1,0,0,0.3),rgb(0,0,1,0.6),rgb(1,0,0.6)))
#dev.off()

ch<-subset(covs.occ3, covs.occ3$cafe==1, select= (ch.med))
fhd<-subset(covs.occ3, covs.occ3$cafe==1, select= (fhdpai))

#ch.cut<- cut(ch$ch.med, 20)
#fhd.cut<- cut(fhd$fhdpai, 20)

#zeta <- table(ch.cut, fhd.cut)
#jpeg("hist_covs3d.jpg",width = 960, height = 600, units = "px", 
#     pointsize = 12, quality = 400)
#hist3D(z=zeta, border="black", xlab="CH",ylab="FHD", zlab="Frequency" )
#dev.off()
#image2D(z=zeta, border="black",xlab="CH",ylab="FHD" )

#boxplot(ch.med~cafe, data=covs.occ3)
#boxplot(fhdpai~cafe, data=covs.occ3)
#boxplot(elev.mean.1~cafe, data=covs.occ3)
#boxplot(lhfi.med.3~cafe, data=covs.occ3)

####Covariables de deteccion####
#decibeles
tab.db<-read.csv("tab.db.summ_1.csv", sep = ";")
#hora
tab.time<-read.csv("tab.time.summ_1.csv", sep = ";")
#temporada
tab.temp<-read.csv("tab.temp.summ_1.csv", sep = ";")

#Organizacion de covariables de deteccion
covs.det.summ<-list(time=tab.time[,2:5], DB=tab.db[,2:5], temp=tab.temp[,2:5])  

#Extraer los datos para correr los modelos
#2020
datos.2020<-datos.1[datos.1[,"year"]==2020,]
unique(datos.2020$Scientific.Name)
#187 spp
unique(datos.2020$month)
#Temporada 3 - 2020
datos.T3<-datos.2020[datos.2020[,"month"]==8 | datos.2020[,"month"]==9,]
spp.t3<-unique(datos.T3$Scientific.Name)
#169 spp
#2021
datos.2021<-datos.1[datos.1[,"year"]==2021,]
length(unique(datos.2021$Scientific.Name))
#Temporada 5 - 2021
datos.T5<-datos.2021[datos.2021[,"month"]==7 | datos.2021[,"month"]==8 |
                       datos.2021[,"month"]==9,]
spp.T5<-unique(datos.T5$Scientific.Name)

#152 spp

#Especies seleccionadas #### solo summer (T3 y T5)
datos_summ <- rbind(datos.T3, datos.T5) #datos de los conteos de verano (t3 y t5)
#write.csv(datos_summ, "datos.summ.csv")
spp_abun <- ddply(datos_summ,~Scientific.Name,summarise,species_count=length(unique(Submission.ID)))
spp_abun <- spp_abun[order(spp_abun$species_count),]
#spp_abun_sel <- spp_abun[spp_abun$species_count >=30 & spp_abun$species_count <= 110,]
spp_sel<-spp_abun$Scientific.Name
#truncar los valores a 0 y 1
trialy[trialy>1]<-1

#loop para determinar especies con varias observaciones
spp_abun_sel<-data.frame(spp_sel,num_sitios=NA,prom_det=NA)
for(i in 1:length(spp_sel)) {   
  sp_name<-spp_sel[i]           #especie
  spt<-trialy[,,sp_name]   #historia de deteccion 
  spp_inter<-apply(spt,1, mean)
  spp_abun_sel$num_sitios[i]<-length(spp_inter[spp_inter>0])
  spp_abun_sel$prom_det[i]<- mean(spp_inter[spp_inter>0])
}

spp_abun_sel[order(spp_abun_sel$num_sitios),]
spp_abun_sel[order(spp_abun_sel$prom_det),]

spp_top<-subset(spp_abun_sel, spp_abun_sel$prom_det>=0.15 & spp_abun_sel$num_sitios>=6 
                & spp_abun_sel$num_sitios<10)
spp_top[order(spp_top$prom_det),]

xspp_sub<-subset(spp_abun_sel, spp_abun_sel$num_sitios>=10)  ### CAMBIAR NOMBRE

xspp_sub <- xspp_sub[-(grep("Bubulcus", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Colaptes rubiginosus", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Cyanocorax affinis", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Ognorhynchus icterotis", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Synallaxis albescens", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Streptoprocne zonaris", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Psittacara wagleri", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Pionus chalcopterus", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Coragyps atratus", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Vireo leucophrys", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Rupornis magnirostris", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Stelgidopteryx ruficollis", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Patagioenas cayennensis", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Vanellus chilensis", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Pygochelidon cyanoleuca", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Streptoprocne rutila", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Herpetotheres cachinnans", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Patagioenas fasciata", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Molothrus oryzivorus", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Cathartes aura", xspp_sub$spp_sel)),]
xspp_sub <- xspp_sub[-(grep("Aulacorhynchus haematopygus", xspp_sub$spp_sel)),]

xspp_sub <- rbind(xspp_sub, spp_top) #con las spp que son las mejores segun los criterios
xspp_sub <- xspp_sub[order(xspp_sub$prom_det),]
dim(xspp_sub)

### selected species with the forest dependance classification by birdlife

spp_important<-read.csv("spp_important_act.csv", sep = ";")
spp_fordep<-spp_important[,2:8]

xspp_sub<- merge(xspp_sub, spp_fordep, by="spp_sel")

######Bucle para correr los modelos######
#siempre correr antes del bucle
sp_occ<-list() #lista para almacenar los valores de occ/sp 
occ_tab<-data.frame(matrix(NA,length(xspp_sub$spp_sel),5))
colnames(occ_tab)<-c("spp","Predicted","SE","lower","upper") 
fit.sp<-list()
fit.sp.occ<-list()
mod.sel<-list()
mod.sel.occ<-list()
mod.sel.bic<-list()
best.mod<-c()
best.mod.occ<-c()
best.mod.aic<-c()
best.mod.bic<-c()
best.occ.bic<-c()
borrar<-list()
ci_tab<-list()
coefis<-list()
confins<-list()
problem<-c()
mod_list<- c("height","fhd","elev","lhfi","height.fhd","fhd.elev",
             "height.elev","height.lhfi","full","null","cat","height.cat",
             "fhd.cat","elev.cat","lhfi.cat")

for(i in 1:length(xspp_sub$spp_sel)) {  
  sp_name<-xspp_sub$spp_sel[i]           #especie
  sp<-trialy[,,sp_name]         #historia de deteccion completa
  sp_unm<-unmarkedFrameOccu(sp,      #obj unm de la T4
                            siteCovs = covs.occ3[,1:5], #covariables ocupacion
                            obsCovs = covs.det.summ) #covariables deteccion
  sp_occ[[i]]<-list()        #segunda lista para almacenar occ/sp (modelo sencillo)
  sp_occ[[i]][[1]]<-occu(~1 ~1, sp_unm) #mod nulo
  sp_occ[[i]][[2]]<-occu(~DB ~1, sp_unm)
  sp_occ[[i]][[3]]<-occu(~time ~1, sp_unm)
  sp_occ[[i]][[4]]<-occu(~temp ~1, sp_unm)
  sp_occ[[i]][[5]]<-occu(~DB+time+temp ~1, sp_unm)
  
  fit.sp[[i]]<-fitList(null=sp_occ[[i]][[1]],
                       db=sp_occ[[i]][[2]],
                       time=sp_occ[[i]][[3]],
                       temp=sp_occ[[i]][[4]],
                       deteccion=sp_occ[[i]][[5]]
  )
  mod.sel[[i]]<-modSel(fit.sp[[i]])
  best.mod[i]<-mod.sel[[i]]@Full[1,1]
  
  if(best.mod[i]=="null"){
    sp_occ[[i]][[6]]<-occu(~1 ~ch.med, sp_unm)
    sp_occ[[i]][[7]]<-occu(~1 ~fhdpai, sp_unm)
    sp_occ[[i]][[8]]<-occu(~1 ~elev.mean.1, sp_unm)
    sp_occ[[i]][[9]]<-occu(~1 ~lhfi.med.3, sp_unm)
    sp_occ[[i]][[10]]<-occu(~1 ~ch.med+fhdpai, sp_unm)
    sp_occ[[i]][[11]]<-occu(~1 ~fhdpai+elev.mean.1, sp_unm)
    sp_occ[[i]][[12]]<-occu(~1 ~ch.med+elev.mean.1, sp_unm)
    sp_occ[[i]][[13]]<-occu(~1 ~ch.med+lhfi.med.3, sp_unm)
    sp_occ[[i]][[14]]<-occu(~1 ~ch.med+fhdpai+elev.mean.1+lhfi.med.3+cafe, sp_unm)
    sp_occ[[i]][[15]]<-occu(~1 ~1, sp_unm)
    sp_occ[[i]][[16]]<-occu(~1 ~cafe, sp_unm)
    sp_occ[[i]][[17]]<-occu(~1 ~ch.med+cafe, sp_unm)
    sp_occ[[i]][[18]]<-occu(~1 ~fhdpai+cafe, sp_unm)
    sp_occ[[i]][[19]]<-occu(~1 ~elev.mean.1+cafe, sp_unm)
    sp_occ[[i]][[20]]<-occu(~1 ~lhfi.med.3+cafe, sp_unm)
  }
  if(best.mod[i]=="db"){
    sp_occ[[i]][[6]]<-occu(~DB ~ch.med, sp_unm)
    sp_occ[[i]][[7]]<-occu(~DB ~fhdpai, sp_unm)
    sp_occ[[i]][[8]]<-occu(~DB ~elev.mean.1, sp_unm)
    sp_occ[[i]][[9]]<-occu(~DB ~lhfi.med.3, sp_unm)
    sp_occ[[i]][[10]]<-occu(~DB ~ch.med+fhdpai, sp_unm)
    sp_occ[[i]][[11]]<-occu(~DB ~fhdpai+elev.mean.1, sp_unm)
    sp_occ[[i]][[12]]<-occu(~DB ~ch.med+elev.mean.1, sp_unm)
    sp_occ[[i]][[13]]<-occu(~DB ~ch.med+lhfi.med.3, sp_unm)
    sp_occ[[i]][[14]]<-occu(~DB ~ch.med+fhdpai+elev.mean.1+lhfi.med.3+cafe, sp_unm)
    sp_occ[[i]][[15]]<-occu(~DB ~1, sp_unm)
    sp_occ[[i]][[16]]<-occu(~DB ~cafe, sp_unm)
    sp_occ[[i]][[17]]<-occu(~DB ~ch.med+cafe, sp_unm)
    sp_occ[[i]][[18]]<-occu(~DB ~fhdpai+cafe, sp_unm)
    sp_occ[[i]][[19]]<-occu(~DB ~elev.mean.1+cafe, sp_unm)
    sp_occ[[i]][[20]]<-occu(~DB ~lhfi.med.3+cafe, sp_unm)
  }
  if(best.mod[i]=="time"){
    sp_occ[[i]][[6]]<-occu(~time ~ch.med, sp_unm)
    sp_occ[[i]][[7]]<-occu(~time ~fhdpai, sp_unm)
    sp_occ[[i]][[8]]<-occu(~time ~elev.mean.1, sp_unm)
    sp_occ[[i]][[9]]<-occu(~time ~lhfi.med.3, sp_unm)
    sp_occ[[i]][[10]]<-occu(~time ~ch.med+fhdpai, sp_unm)
    sp_occ[[i]][[11]]<-occu(~time ~fhdpai+elev.mean.1, sp_unm)
    sp_occ[[i]][[12]]<-occu(~time ~ch.med+elev.mean.1, sp_unm)
    sp_occ[[i]][[13]]<-occu(~time ~ch.med+lhfi.med.3, sp_unm)
    sp_occ[[i]][[14]]<-occu(~time ~ch.med+fhdpai+elev.mean.1+lhfi.med.3+cafe, sp_unm)
    sp_occ[[i]][[15]]<-occu(~time ~1, sp_unm)
    sp_occ[[i]][[16]]<-occu(~time ~cafe, sp_unm)
    sp_occ[[i]][[17]]<-occu(~time ~ch.med+cafe, sp_unm)
    sp_occ[[i]][[18]]<-occu(~time ~fhdpai+cafe, sp_unm)
    sp_occ[[i]][[19]]<-occu(~time ~elev.mean.1+cafe, sp_unm)
    sp_occ[[i]][[20]]<-occu(~time ~lhfi.med.3+cafe, sp_unm)
  }
  if(best.mod[i]=="temp"){
    sp_occ[[i]][[6]]<-occu(~temp ~ch.med, sp_unm)
    sp_occ[[i]][[7]]<-occu(~temp ~fhdpai, sp_unm)
    sp_occ[[i]][[8]]<-occu(~temp ~elev.mean.1, sp_unm)
    sp_occ[[i]][[9]]<-occu(~temp ~lhfi.med.3, sp_unm)
    sp_occ[[i]][[10]]<-occu(~temp ~ch.med+fhdpai, sp_unm)
    sp_occ[[i]][[11]]<-occu(~temp ~fhdpai+elev.mean.1, sp_unm)
    sp_occ[[i]][[12]]<-occu(~temp ~ch.med+elev.mean.1, sp_unm)
    sp_occ[[i]][[13]]<-occu(~temp ~ch.med+lhfi.med.3, sp_unm)
    sp_occ[[i]][[14]]<-occu(~temp ~ch.med+fhdpai+elev.mean.1+lhfi.med.3+cafe, sp_unm)
    sp_occ[[i]][[15]]<-occu(~temp ~1, sp_unm)
    sp_occ[[i]][[16]]<-occu(~temp ~cafe, sp_unm)
    sp_occ[[i]][[17]]<-occu(~temp ~ch.med+cafe, sp_unm)
    sp_occ[[i]][[18]]<-occu(~temp ~fhdpai+cafe, sp_unm)
    sp_occ[[i]][[19]]<-occu(~temp ~elev.mean.1+cafe, sp_unm)
    sp_occ[[i]][[20]]<-occu(~temp ~lhfi.med.3+cafe, sp_unm)
  }
  if(best.mod[i]=="deteccion"){ 
    sp_occ[[i]][[6]]<-occu(~DB+time+temp ~ch.med, sp_unm)
    sp_occ[[i]][[7]]<-occu(~DB+time+temp ~fhdpai, sp_unm)
    sp_occ[[i]][[8]]<-occu(~DB+time+temp ~elev.mean.1, sp_unm)
    sp_occ[[i]][[9]]<-occu(~DB+time+temp ~lhfi.med.3, sp_unm)
    sp_occ[[i]][[10]]<-occu(~DB+time+temp ~ch.med+fhdpai, sp_unm)
    sp_occ[[i]][[11]]<-occu(~DB+time+temp ~fhdpai+elev.mean.1, sp_unm)
    sp_occ[[i]][[12]]<-occu(~DB+time+temp ~ch.med+elev.mean.1, sp_unm)
    sp_occ[[i]][[13]]<-occu(~DB+time+temp ~ch.med+lhfi.med.3, sp_unm)
    sp_occ[[i]][[14]]<-occu(~DB+time+temp ~ch.med+fhdpai+elev.mean.1+lhfi.med.3+cafe, sp_unm)
    sp_occ[[i]][[15]]<-occu(~DB+time+temp ~1, sp_unm)
    sp_occ[[i]][[16]]<-occu(~DB+time+temp ~cafe, sp_unm)
    sp_occ[[i]][[17]]<-occu(~DB+time+temp ~ch.med+cafe, sp_unm)
    sp_occ[[i]][[18]]<-occu(~DB+time+temp ~fhdpai+cafe, sp_unm)
    sp_occ[[i]][[19]]<-occu(~DB+time+temp ~elev.mean.1+cafe, sp_unm)
    sp_occ[[i]][[20]]<-occu(~DB+time+temp ~lhfi.med.3+cafe, sp_unm)
 }
  fit.sp.occ[[i]] <- fitList( 
    height=sp_occ[[i]][[6]], 
    fhd=sp_occ[[i]][[7]], 
    elev=sp_occ[[i]][[8]],
    lhfi=sp_occ[[i]][[9]],
    height.fhd=sp_occ[[i]][[10]],
    fhd.elev=sp_occ[[i]][[11]],
    height.elev=sp_occ[[i]][[12]],
    height.lhfi=sp_occ[[i]][[13]],
    full=sp_occ[[i]][[14]],
    null=sp_occ[[i]][[15]],
    cat=sp_occ[[i]][[16]],
    height.cat=sp_occ[[i]][[17]],
    fhd.cat=sp_occ[[i]][[18]],
    elev.cat=sp_occ[[i]][[19]],
    lhfi.cat=sp_occ[[i]][[20]]
  )
  
  uno <- which(as.numeric(lapply(sp_occ[[i]], function(x) x@opt$convergence ))[6:20]==1) #borra los que no convergen
  dos <- lapply(sp_occ[[i]][6:20], function (x) (slot(x@estimates,"estimates"))$state)
  tres <- which(as.numeric(lapply(dos, function (x) length(which(SE(x)>1)==TRUE))) > 0)
  cuatro <- which(as.numeric(lapply(dos, function (x) length(which(is.na(SE(x)==TRUE))))) > 0)
  borrar[[i]]<- unique(c(uno,tres,cuatro))
  if(length(borrar[[i]])==15) next
  
  if(length(borrar[[i]])>0 & length(borrar[[i]])<20){                              
    fit.sp.occ[[i]]<-fit.sp.occ[[i]]@fits[-borrar[[i]]]
    mod.sel.occ[[i]]<-modSel(fitList(fits=fit.sp.occ[[i]]))
    mod.sel.bic[[i]]<-bictab(fit.sp.occ[[i]])
  } else {
    mod.sel.occ[[i]]<-modSel(fit.sp.occ[[i]])
    mod.sel.bic[[i]]<-bictab(fit.sp.occ[[i]]@fits)
  }
  best.mod.occ[i]<-mod.sel.occ[[i]]@Full$model
  best.mod.aic[i]<-mod.sel.occ[[i]]@Full$AIC
  best.occ.bic[i]<-mod.sel.bic[[i]]$Modnames
  best.mod.bic[i]<-mod.sel.bic[[i]]$BIC
  match.best<-match(best.mod.occ[[i]], mod_list)+5
  coefis[[i]]<-coef(sp_occ[[i]][[match.best]])
  confins[[i]]<-confint(sp_occ[[i]][[match.best]],type = "state",  level= 0.9)
}

#visualization of best model
unique(best.mod)
barplot(prop.table(sort(table(best.mod), decreasing = T)))

unique(best.mod.occ) #con AIC
#png("best.mod.png")
par(mar = c(6, 4, 4, 2))
barplot(prop.table(sort(table(best.mod.occ), decreasing=T)), las =2)
#dev.off()
#unique(best.occ.bic) #con BIC
#barplot(prop.table(table(best.occ.bic)), las =2)

##### Graficas de respuesta por especies por covariable

#datos para hacer las predicciones

cov.data.1<-data.frame(ch.med = seq(-3.5,2.77, length.out = 100), ##REVISAR
                       fhdpai = 0, 
                       elev.mean.1 = 0,
                       lhfi.med.3 = 0,
                       cafe = 1
)
cov.data.2<-data.frame(ch.med = 0,
                       fhdpai = seq (-3.6,2.4, length.out = 100),
                       elev.mean.1 = 0,
                       lhfi.med.3 = 0,
                       cafe = 1
)
cov.data.3<-data.frame(ch.med = 0,
                       fhdpai = 0,
                       elev.mean.1 = seq (-2,2.3, length.out = 100),
                       lhfi.med.3 = 0,
                       cafe = 1
)  
cov.data.4<-data.frame(ch.med = 0,
                       fhdpai = 0,
                       elev.mean.1 = 0,
                       lhfi.med.3 = seq (-3.4,2, length.out = 100),
                       cafe = 1
)
cov.data.5<-data.frame(ch.med = 0,
                       fhdpai = 0,
                       elev.mean.1 = 0,
                       lhfi.med.3 = 0,
                       cafe = seq(1,1,length.out = 100)
)
pred_covs<-list(height=cov.data.1, fhd=cov.data.2, elev=cov.data.3, 
                lhfi=cov.data.4, cafe=cov.data.5)

mod_list<- c("height","fhd","elev","lhfi","height.fhd","fhd.elev",
             "height.elev","height.lhfi","full","null","cat","height.cat",
             "fhd.cat","elev.cat","lhfi.cat") #repetido antes del bucle
cov_list<- c("ch.med","fhdpai","elev.mean.1","lhfi.med.3","cafe","ch.med:cafe", "fhdpai:cafe", 
             "elev.mean.1:cafe", "lhfi.med.3:cafe")

###JUAN###
#Grafico con todos los coeficientes para grupo de especies donde esa covariable quedo en el mejor modelo
#primero genero tabla con todos los coeficientes y su intervalo de confianza
tab_coef<-data.frame(spp=rep(NA,400), 
                     covs=NA, 
                     coef=NA,
                     CI_5=NA,
                     CI_95=NA)
u=1
for(i in 1:length(xspp_sub$spp_sel)) {
  mods_nam<-names(coef(sp_occ[[i]][[match(best.mod.occ[i], mod_list)+5]]))#+5 para el num del modelo
  donde <- lapply(cov_list, grep , x=mods_nam)
  cuales<-which(as.numeric(lapply(donde, FUN=function(x) length(x)))>0)
  if (length(cuales) == 0) {
    next }
  else {
    for (j in 1: length(cuales)) {
      tab_coef$spp[u]   <- xspp_sub$spp_sel[i]
      tab_coef$covs[u]  <- cov_list[cuales[j]]
      tab_coef$coef[u]  <- coefis[[i]][j+1] #mas uno porque el primer coeficiente siempre es el intercepto
      tab_coef$CI_5[u]  <- confins[[i]][j+1,1]
      tab_coef$CI_95[u] <- confins[[i]][j+1,2]
      u=u+1
    }
  }
}

tab_coef <- tab_coef[1:(u-1),]

# Graph Canopy Height
height_1<-subset(tab_coef,covs=="ch.med")
height_2<-height_1[order(height_1$coef),]
height_3<-subset(tab_coef,covs=="ch.med:cafe")

#jpeg("canopy_height_occ_3.jpg", quality = 75)
ggplot(height_2, aes(x = reorder(spp, coef), y = coef)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(color='green') +
  geom_linerange(aes(x = spp, ymin = CI_5, ymax = CI_95), color = 'green') +
  coord_flip() +
  labs(y = "Effect of canopy height on occupancy")#+
  #geom_point(aes(x=height_2$spp, y=height_2$coef), color="brown")+
  #geom_linerange(aes(x=height_2$spp, ymin = height_2$CI_5, ymax = height_2$CI_95), color = "brown")

#dev.off()

### Graph Total Cover
fhd_1<-subset(tab_coef,covs=="fhdpai")
fhd_2<-fhd_1[order(fhd_1$coef),]
fhd_3<-subset(tab_coef,covs=="fhdpai:cafe") #no hay spp
#jpeg("total_cover_occ_2.jpg")
ggplot(fhd_2, aes(x = reorder(spp, coef), y = coef)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(color='green') +
  geom_linerange(aes(x = spp, ymin = CI_5, ymax = CI_95), color = 'green') +
  coord_flip() +
  labs(y = "Effect of Foliage Height Diversity on Occupancy")
#dev.off()    

# Graph Elevation
elev_1<-subset(tab_coef, covs=="elev.mean.1")
elev_2<-elev_1[order(elev_1$coef),]
elev_3<-subset(tab_coef,covs=="elev.mean.1:cafe")
#jpeg("elevation_occ_2.jpg")
ggplot(elev_2, aes(x = reorder(spp, coef), y = coef)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(color='green') +
  geom_linerange(aes(x = spp, ymin = CI_5, ymax = CI_95), color = 'green') +
  coord_flip() +
  labs(y = "Effect of Elevation on occupancy")#+
  #geom_point(aes(x=elev_3[1,]$spp, y=elev_3[1,]$coef), color="brown")+
  #geom_linerange(aes(x=elev_3[1,]$spp, ymin=elev_3[1,]$CI_5, ymax=elev_3[1,]$CI_95), color = "brown")+
  #geom_point(aes(x=elev_3[2,]$spp, y=elev_3[2,]$coef), color="brown")+
  #geom_linerange(aes(x=elev_3[2,]$spp, ymin=elev_3[2,]$CI_5, ymax=elev_3[2,]$CI_95), color = "brown")
#dev.off()

# Graph Human Footprint
lhfi_1<-subset(tab_coef, covs=="lhfi.med.3")
lhfi_2<-lhfi_1[order(lhfi_1$coef),]
lhfi_3<-subset(tab_coef,covs=="lhfi.med.3:cafe")
#jpeg("lhfi_occ_2.jpg")
ggplot(lhfi_2, aes(x = reorder(spp, coef), y = coef)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_point(color='green') +
  geom_linerange(aes(x = spp, ymin = CI_5, ymax = CI_95), color = 'green') +
  coord_flip() +
  labs(y = "Effect of Human Footprint on occupancy")#+
  #geom_point(aes(x=lhfi_3[1,]$spp, y=lhfi_3[1,]$coef), color="brown")+
  #geom_linerange(aes(x=lhfi_3[1,]$spp, ymin=lhfi_3[1,]$CI_5, ymax=lhfi_3[1,]$CI_95), color = "brown")+
  #geom_point(aes(x=lhfi_3[2,]$spp, y=lhfi_3[2,]$coef), color="brown")+
  #geom_linerange(aes(x=lhfi_3[2,]$spp, ymin=lhfi_3[2,]$CI_5, ymax=lhfi_3[2,]$CI_95), color = "brown")
#dev.off()

######################## Graficas de cafe
#especies con cafe en mejor modelo
#cualcafe <- match(unique(tab_coef[grep("cafe",x=tab_coef$covs),"spp"]),spp_sub$spp_sel)
#sppCafe  <- unique(tab_coef[grep("cafe",x=tab_coef$covs),"spp"])
cualcafe <- which(best.mod.occ=='cat')
sppCafe <- xspp_sub$spp_sel[which(best.mod.occ=='cat')]

#interceptos de esos modelos
intercepto <- rep(NA, length(cualcafe))
ses        <- rep(NA, length(cualcafe))
for (i in 1:length(cualcafe)) {
  match.best<-match(best.mod.occ[[cualcafe[i]]], mod_list)+5
  intercepto[i] <- (coef(sp_occ[[cualcafe[i]]][[match.best]]))[1]
  ses[i]        <- (SE(sp_occ[[cualcafe[i]]][[match.best]]))[1]
}

#tabla para figura
ncat=2 #numero de categorias
dFcafe <- data.frame(matrix(NA, nrow=length(cualcafe)*ncat, ncol=5, 
                            dimnames=list(NULL,c("Especie","cat","odd","lower","upper"))))
dFcafe$Especie <- rep(sppCafe, each=ncat)

sec <- seq(1,dim(dFcafe)[1]+1, 2)
sec
dFcafe$cat <- c("noCafe", "Cafe")
dFcafe$odd[sec[1:length(sppCafe)]] <-intercepto

for (i in 1: length(cualcafe)) {
  x <- subset(tab_coef,spp==sppCafe[i])
  dFcafe[(sec[i]+1):(sec[i+1]-1),3:5] <- x[1,3:5]
  dFcafe[sec[i],4:5] <- confins[[cualcafe[i]]][1,]
}
#posible figura
ggplot(dFcafe, aes(x = odd, y = cat)) +
  geom_point(size = 3.5, color = "orange") + 
  geom_vline(aes(xintercept = 0), linewidth = 0.25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = upper, xmin = lower), linewidth = 0.5, height = 0.2, color = "gray50") +
  xlab("log Odds ratio") +
  ylab("Categoria Cafe") +
  facet_wrap(~Especie)

######  Graficas de resultados para cada especie #####

#pdf("occu_graphs_final.pdf")

for(i in 1:length(xspp_sub$spp_sel)) { 
  mods_nam<-names(coef(sp_occ[[i]][[match(best.mod.occ[i], mod_list)+5]])) #+5 para el num del modelo
  cuales<-which(as.numeric(lapply(cov_list, grep, x=mods_nam))>0)
  
  if (is.na(best.mod.occ[i])== TRUE) next
  if (length(cuales)==0) next 
  
  for (j in 1:length(cuales)) { #no funciona del todo
    figura<-data.frame(pred_covs[[cuales[j]]],predict(sp_occ[[i]][[match(best.mod.occ[i], 
                              mod_list)+5]],type='state', newdata=pred_covs[[cuales[j]]])) 
    print(
      ggplot(data = figura, aes(x=figura[,cuales[j]], y=Predicted))+
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1)+
        geom_line()+
        labs(x=names(figura)[cuales[j]], y= "occupancy", title = xspp_sub$spp_sel[i]))+
      theme_solarized()+scale_colour_solarized()
  }
}

#### Models averaging! ######

#dev.off()

#mod.sel.occ <- mod.sel.occ[-3] #why am i doing this?

#sacar especies que ningun modelo cumplio criterios
elim <- which(as.numeric(lapply(mod.sel.occ,length))==0)
mod.sel.occ2 <- mod.sel.occ[-elim]

#cuales especies el mejor modelo es el nulo
nonul <- as.numeric(lapply(mod.sel.occ2,function(x) x@Full$model[1] != "null"))

#para las que el mejor modelo NO es el nulo
mejores <- function(x) {
  if (dim(x@Full)[1] == 1) {
    return(x@Full$model[1])
  } else {
  cual <- which(x@Full$delta > 2)
  return(x@Full$model[1:(cual[1]-1)])
  }
  }

  nonul1<- which(nonul==1)
#   lapply(mod.sel.occ[],mejores)

#Bucle para determinar cuales modelos tienen AIC menor que 2 en el listado de modelos

cand.mod <- list()
length(cand.mod) <- 73

for (i in 1:length(nonul)) {
  if (nonul[i]==0) 
    {cand.mod[[i]] = "null"}
  #else {
   # if (dim(mod.sel.occ[[i]]@Full)[1] == 1) {
   # cand.mod[[i]] <- mod.sel.occ[[i]]@Full$model[1]}
    else {
    cual <- which(mod.sel.occ2[[i]]@Full$delta <= 2)
    cand.mod[[i]] <- mod.sel.occ2[[i]]@Full$model[1:max(cual)]
  }
  }
  #}
cand.mod

# probar usando todos los modelos o probar usando solo los mejores

#modavg

# use the  candidate models for running modavg
sel.mod.avg <- list()
sel.mod.avg[[i]] <- mod.sel.occ2[[i]]@Full[1:length(cand.mod[[i]]),] #it worked, for nothing!

#tryouts with the complete set of models for every species

fit.sp.occ2 <- fit.sp.occ[-elim]

# Crear bucle para convertir fit.sp.occ2[[i]] en lista para todas las especies

for (i in 1:length(xspp_sub$spp_sel[-elim])) {
  if ((length((borrar[-elim])[[i]])==0)) {
  fit.sp.occ2[[i]] <- as.list(fit.sp.occ2[[i]]@fits)
  } else
  fit.sp.occ2[[i]] <- fit.sp.occ2[[i]]  
}

modprom <- list()
prom.coefs <- data.frame(spp_select=NA, Cov=NA, coef= NA, CI_5 = NA, 
                         CI_95 = NA, SE=NA)
species <- as.vector(xspp_sub$spp_sel[-elim])

for (i in 1:length(xspp_sub$spp_sel[-elim])) {
  mods_nam2 <- unique(do.call("c",lapply(fit.sp.occ2[[i]],FUN=function(x) names(coef(x)))))
  donde <- lapply(cov_list[1:5], grep, x=mods_nam2) ##AQUI TODAVIA HAY ALGO RARO!
  cuales <- which(as.numeric(lapply(donde, FUN=function(x) length(x)))>0)
  modprom[[i]] <- list()
  if (length(grep(pattern = ".cat", x=names(fit.sp.occ2[[i]]))) == 0) {
    for (j in 1:length(cuales)) {
      modprom[[i]][[j]] <- modavg(fit.sp.occ2[[i]], parm = cov_list[cuales[j]] ,parm.type="psi")
    }
    
  } else {
    fit.sp.occ2[[i]] <- fit.sp.occ2[[i]][-(grep(pattern = ".cat", x=names(fit.sp.occ2[[i]])))]
    mods_nam2 <- unique(do.call("c",lapply(fit.sp.occ2[[i]],FUN=function(x) names(coef(x)))))
    donde <- lapply(cov_list[1:5], grep, x=mods_nam2) ##AQUI TODAVIA HAY ALGO RARO!
    cuales <- which(as.numeric(lapply(donde, FUN=function(x) length(x)))>0)
    for (j in 1:length(cuales)) {
  modprom[[i]][[j]] <- modavg(fit.sp.occ2[[i]], parm= cov_list[cuales[j]], parm.type="psi")
    }
  }

  prom <- do.call("c",lapply(modprom[[i]], function(x) x$Mod.avg.beta))
  low.prom <- do.call("c",lapply(modprom[[i]], function(x) x$Lower.CL))
  upp.prom <- do.call("c",lapply(modprom[[i]], function(x) x$Upper.CL))
  se.prom <- do.call("c",lapply(modprom[[i]], function(x) x$Uncond.SE ))
  prom.coefs<- data.frame(rbind(prom.coefs, data.frame(spp_select = species[i], 
                                            Cov=cov_list[cuales], coef=prom, 
                                            CI_5=low.prom, CI_95=upp.prom, SE=se.prom)))

}

#write.csv(prom.coefs, "coefficients_2024.csv")

# most frequent covariate after averaging models

barplot(prop.table(sort(table(prom.coefs$Cov), decreasing=T)), las =2)

### tabla con los coeficientes

coefs.tab <- prom.coefs

species <- xspp_sub[xspp_sub$spp_sel %in% coefs.tab$spp_select,]

coefs.tab <- cbind(xspp_sub)

coefs.tab$for_dep<-xspp_sub$forest_dependence
coefs.tab$endem<-xspp_sub$endemismo
coefs.tab$spp_end<-xspp_sub$spp_end

##### Extraer los valores de occ predicha para cada spp/punto para hacer el GDM nuevamente 

#### SOLO HACER UNA VEZ

fit.sp.occ2

prom.nam <- c("psi(ch.med)","psi(fhdpai)","psi(elev.mean.1)",
              "psi(lhfi.med.3)","psi(cafe)")

modavgPred(fit.sp.occ2[[i]],parm.type="psi", newdata = covs.occ3)

pred.spp <- list()

for (i in 1:length(xspp_sub$spp_sel[-elim])) {
  #if(is.null(fit.sp.occ2[[i]][["cat"]])==T) next 
  pred.spp[[i]]<-modavgPred(fit.sp.occ2[[i]],parm.type="psi", newdata = covs.occ3)
}

p<-list()

for (i in 1:length(pred.spp)){
  Ped<-pred.spp[[i]]$mod.avg.pred
  p[[i]]<-Ped
}
pred2 <- as.data.frame(do.call(cbind, p))
names(pred2) <- xspp_sub$spp_sel[-elim]
#write.csv(pred2,"pred_occ_2.csv")
