# Data organization based on a dataframe from an eBird account not the  total
# data for an specific area or similar

library(lubridate) # manejo de variables de tiempo
library(plyr)
library(dplyr)     # manejo de datos
library(reshape2)  # melt y acast
library(unmarked)  # modelos de ocupacion
library(AICcmodavg)# seleccion de modelos
library(MuMIn)     # automated model selection
library(ggplot2)
library(stringr)

setwd("C:/Users/Atila/Dropbox/tesis/") #win

##### Cargar y organizar los datos #####
# cuatro temporadas # standardize species names
datos.raw <- read.csv("C:/Users/Atila/Dropbox/tesis/MyEBirdData_21122021.csv", header=T, stringsAsFactors = FALSE) #Win
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Zimmerius chrysops chrysops")]<- "Zimmerius chrysops"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Troglodytes aedon [musculus Group]")]<- "Troglodytes aedon"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Ramphocelus flammigerus flammigerus")]<- "Ramphocelus flammigerus"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Patagioenas fasciata [albilinea Group]")]<- "Patagioenas fasciata"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Mimus gilvus [gilvus Group]")]<- "Mimus gilvus"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Colibri cyanotus cyanotus/crissalis")]<- "Colibri cyanotus"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Caracara plancus [cheriway Group]")]<- "Caracara plancus"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Bubulcus ibis ibis")]<- "Bubulcus ibis"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Aulacorhynchus albivitta albivitta/phaeolaemus")]<- "Aulacorhynchus albivitta"
datos.raw$Scientific.Name[which(datos.raw$Scientific.Name == "Atlapetes albinucha [gutturalis Group]" )]<- "Atlapetes albinucha"

taxones<-unique(datos.raw$Scientific.Name)
# 259 taxones en total
#limpiar los datos eliminando redundancias y posibles errores
datos.4<- datos.raw[!grepl("sp.", datos.raw[["Scientific.Name"]]), ] #eliminar lo que tiene "sp."
datos.3<- datos.4[!grepl("sp,", datos.4[["Scientific.Name"]]), ] #eliminar lo que tiene "sp,"
datos.2<- datos.3[!grepl("sp ", datos.3[["Scientific.Name"]]), ] #eliminar lo que tiene "sp"
spp.clean<-unique(datos.2$Scientific.Name)
# 241 spp total
datos.1 <- datos.2[grep("BPI-",datos.2$Location),] #dejar unicamente los puntos de muestreo
spp.bpi<-unique(datos.1$Scientific.Name) #especies en puntos BPI
# 234 especies
#separar la columna Date en tres columnas: year, month, day.
datos.1 <- datos.1 %>% 
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date),
                rec_1 = 1)
#creo columna con formato de tiempo para organizar replicas cronologicamente. 
datePOSIXct <- ymd(datos.1$Date) #para que entienda esa columna como year, month y day
X <- paste(datos.1$Date,datos.1$Time) #pegar fecha y hora en una col
TimePOSIXct <- parse_date_time(X,'%y/%m/%d %I:%M %p') #Para que entienda en que lugar esta cada segmento
datos.1$TimePOSIXct <- TimePOSIXct #pegar esta columna al conjunto de datos

#todas las localidades muestreadas
local <- unique(datos.1$Location)
local <- local[order(local)]
local_coor <- df
#todas las replicas
datos.1$replicas <- paste(datos.1$Location,datos.1$Date,datos.1$Time)
repl  <- unique(datos.1$replicas)
str(repl)
#1616 replicas hasta el fin de la T3
#2198 replicas hasta el fin de la T4
#2749 al fin de la T5
##### Matriz de repeticiones con NA #####
#Crear una matriz con las localidades y cuantas veces ha sido muestreada esa localidad
#para corroborar cuales han sido muestreadas, cuales no, y cuales son NA y/o tienen errores
####correr las primeras 4 lineas del bucle siguiente antes de hacer esto, luego si correr 
#el bucle completo
#rep.1<-matrix(NA,153,20)
#rep.1<-do.call("rbind", repeticiones)
#dim(rep.1)
#head(rep.1)
#tail(rep.1)
#repeticiones[[153]]
#for (i in 1:153){
#  rep.1[i,1:length(repeticiones[[i]])]<-repeticiones[[i]]  
#}
#write.csv(rep.1, "matriz_NA5.csv") 
#Cargar la matriz arreglada a mano
#con la organizacion de las unidades de muestreo con NAs y listas vacias
rep.na<-read.csv("matriz_NA_T5.csv", sep = ";") #correr antes del bucle
#rep.na<-rep.na[,-14] #ya elimine la ultima columna
rep.na<-rep.na[,-1] #eliminar la primera columna
#BUCLE
#columna (ahora con NAs) para incluir dato nuevo de repeticion
repeticiones<-list()
datos.1$repeticion <- NA
for (i in 1:length(local)) {
  sub <- datos.1[datos.1$Location == local[i],]   #saco registros solo para localidad 1 hasta el 153
  sub <- arrange(sub,TimePOSIXct)                 #ordeno estos registros cronologicamente
  reps <- unique(sub$replicas)                    #obtengo lista unica de replicas para loc i pero organizada cronologicamente
  repeticiones[[i]]=reps                          #Muestra las unidades de muestreo que tiene cada sp en el array
  #donde1 <- match(datos.1$replicas,reps)         #averiguo el numero de repeticion que debo asignar a cada replica para la loc i en la tabla original
  donde2 <- match(datos.1$replicas,rep.na[i,])    #encuentro cuales datos son NA segun la matriz rep.na
  cuales <- which(is.na(donde2)==FALSE)           #averiguo en que posicion de la tabla original debo poner los numeros de las repeticiones
  datos.1$repeticion[cuales] <- na.omit(donde2)   #asigno el numero de la repeticion en la tabla original
}

#exportar archivo hasta ahora
#write.csv(datos.1, file = "datos.1.csv") #### DATOS HASTA LAS T5
datos.1<-read.csv("datos.1.csv")

###
#este paso es para obtener un array donde las filas son las localidades, las columnas las repeticiones,
# y la tercera dimension son las especies. 
#convierte la matriz de formato wide a formato long con estas tres columnas 
trash <- melt(datos.1, id.vars = c("Location", "repeticion","Scientific.Name"), measure.vars="Count")
#convierte el formato long en un array organizado (eje z) por la especie y pone 0s donde no esta la spp
trial <- acast(trash,Location~repeticion~Scientific.Name,fill = 0)
#por culpa de algo esta creando una 17 columna, entonces se elimina
trial2 <- trial[,-17,]
dim(trial2) #153 (localidades) 16 (repeticiones) 213 (especies)
#orden alfabetico de las spp para saber el # de cada una
spp <- unique(trash$Scientific.Name[order(trash$Scientific.Name)]) 
spp
#213 spp en fincas AAA hasta T4
# numero de registros/spp
spp_reg <- datos.1 %>%
  group_by(Scientific.Name) %>%
  tally(Count)
#ggplot(spp_reg) +
#  geom_bar(aes(x= reorder(Scientific.Name, -n), n), stat = "identity") +
#  labs(x = "Specie", y = "Number of records")
#Saber donde estan los NA en esta matriz
donde.na <- which(is.na(rep.na) == TRUE, arr.ind = TRUE)
#ponerle los NA en el array trial2 donde pertenecen
#buscar en que localidad esta 
for (i in 1:153) {
  trial2[donde.na[i,1],donde.na[i,2],] <- NA
}
#cuantas repeticiones tiene cada unidad de muestreo y corroborar
lapply(repeticiones,length) 
#si quiero ver la tabla para una especie:
head(trial2[,,213])
tail(trial2[,,213])

####Covariables de deteccion####
#decibeles
db.total<- grep("db", datos.1$Checklist.Comments, ignore.case = TRUE) #encontrar en que filas esta "db"
db.ext <-as.numeric(str_extract(
  str_extract(datos.1$Checklist.Comments, "[Dd][Bb]\\s[0-9][0-9]"), #extraer todo lo que tenga "db ##"
  "[0-9][0-9]"))  #extraer s?lo el valor
datos.1$db.ext<- db.ext #agregar esta columna al df principal
datos.0<- datos.1[datos.1$year!=2019,] # quitar los datos del 2019
#datos.0<- datos.0[-(datos.0$year==2020 & datos.0$month<4),] #quitar lo que es T2
#Dejar todos los datos del 2020 y 2021
#datos.db<- datos.0[which(((datos.0$year==2020) == (datos.0$month<4))==FALSE),] #quitar los datos de antes del mes 4
datos.db<-datos.0 #para todos los a?os

#scale(datos.db$db.ext)
tab.db<-tapply(scale(datos.db$db.ext), list(datos.db$Location,datos.db$repeticion), mean)
tab.time<-tapply(datos.db$TimePOSIXct, list(datos.db$Location,datos.db$repeticion), "first")

db.mean<-apply(tab.db, 1, mean, na.rm=TRUE)
#hist(db.mean)
db.mean.std<-scale(db.mean)
donde.na<-which(is.na(tab.db)==TRUE,arr.ind = TRUE)

tab.db[donde.na]<-db.mean.std[donde.na[,1]] #tabla sin NAs
tab.db<-tab.db[,c(-1)]
tab.db.t3<-tab.db[,5:8]
tab.db.t5<-tab.db[,13:16]
tab.db.summ<-cbind(tab.db.t3, tab.db.t5)
#write.csv(tab.db.summ, file = "tab.db.summ.csv")

#hora
hora<-str_split(datos.db$Time, " ")
hora.1<-do.call("rbind",hora)
time.1<-str_split(hora.1[,1], ":")
time<-do.call("rbind",time.1)
time<-apply(time, 2, as.numeric)

time.std<-scale((time[,1]+(time[,2]/60)))
#hist(time.std)
tab.time<-tapply(time.std, list(datos.db$Location, datos.db$repeticion), mean)
tab.time[is.na(tab.time)]<-0
tab.time<-tab.time[,c(-1)]
tab.time.t3<-tab.time[,5:8]
tab.time.t5<-tab.time[,13:16]
tab.time.summ<-cbind(tab.time.t3, tab.time.t5)

write.csv(tab.time.summ, file = "tab.time.summ.csv")

### 1 & 0 
tab.temp<-matrix(153, 16, data=NA)
tab.temp[,1:8] <- 1 
tab.temp[,9:16] <- 0
tab.temp.t2<-tab.temp[,1:4]
tab.temp.t3<-tab.temp[,5:8]
tab.temp.t4<-tab.temp[,9:12]
tab.temp.t5<-tab.temp[,13:16]
tab.temp.wint<- cbind(tab.temp.t3, tab.temp.t5)
write.csv(tab.temp.wint, "tab.temp.wint.csv")
tab.temp.summ<-cbind(tab.temp.t2, tab.temp.t4)
write.csv(tab.temp.summ, "tab.temp.summ.csv")
