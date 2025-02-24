library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library('Hmisc')
library(spdep)
library(sp)
library(tmap)
library(raster)
library(geoR)
library(spdep)
library(gstat)
library(wesanderson)
library(extrafont)
library(LS2Wstat)

loadfonts(device = "win")
library(car)

# Primero leemos los archivos a utilizar
departamentos <- st_read("Datos/Codgeo_Pais_x_dpto_con_datos/pxdptodatosok.shp")
smn <- read.csv(file = 'Datos/estaciones_smn.csv', sep = ";")
temperaturas <- read.csv(file = 'Datos/temperaturas.csv')
datoshorarios <- read.csv(file = 'Datos/datoshorarios.csv')
humedad_historico <- read.csv(file = 'Datos/humedad_historico.csv')
temperatura_historico <- read.csv(file = 'Datos/temperatura_historico.csv')
preciocampos <- read_excel("Datos/precio campos.xlsx")
campos <- read_excel("Datos/Campos.xlsx")
estimaciones <- read.csv("/cloud/project/Datos/Estimaciones.csv", sep=";")


########################################################################################
################################  ANALISIS EXPLORATORIO  ###############################
########################################################################################

#Miro un poco el de departamentos que utilizamos en clase.
summary(departamentos)
grafico1 <-ggplot() + geom_sf(data =departamentos)
grafico1
summary(departamentos$provincia)

#La Antartida es parte de T. del Fuego
departamentos_tf <- departamentos %>% 
  filter(provincia == "Tierra del Fuego") %>% mutate(personas = personas%>% as.numeric())
grafico2 <- ggplot() + geom_sf(data =departamentos_tf, aes(fill = personas))  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())
grafico2

#Ahora voy a ver la localizaci�n de las estaciones
summary(smn)
colnames(smn)
#Cambio este nombre para que se lea mejor
smn <- smn %>% rename(Estacion = NOMBRE) 
summary(smn$Provincia)
describe(smn)

# Vemos c�mo se distribuyen las estaciones por provincia.
# La provincia con mayores estaciones es, por lejos, Buenos Aires. La siguen C�rdoba y Santa Fe.
#View(smn %>% group_by(Provincia) %>% tally(sort = TRUE))

# Ac� tengo que sumarle los minutos a la latitud porque vienen separados

smn$minutos_latitud <- smn$Latitud.min./60
smn$minutos_longitud <- smn$Longitud.min./60
head(smn)
smn$x <- smn$Longitud.gr.-smn$minutos_longitud
smn$y <- smn$Latitud.gr.-smn$minutos_latitud
head(smn)

#Veo las estaciones en el mapa

ggplot() + geom_sf(data = departamentos,fill = c('seashell'), color = "slategray",size = 0.50) + geom_point(data = smn, aes(x=x, y=y),
                                                      colour = "royalblue1", size = 1)+ theme_classic()

departamentos_ba <- departamentos %>% filter(provincia == "Buenos Aires")
smn_ba <- smn %>% filter(Provincia == "BUENOS AIRES")
#Veo las estaciones en el mapa de la provincia de Buenos Aires

ggplot() + geom_sf(data = departamentos_ba,fill = c('seashell'), color = "slategray") + 
  geom_point(data = smn_ba, aes(x=x, y=y),colour = "royalblue1", size = 2)+ 
  theme_classic() +geom_text(data= smn_ba, aes(x,y, label = Estacion), size = 2, fontface = "bold",vjust = -0.5)+
  ggtitle("Estaciones Meteorol�gicas", subtitle = "Provincia de Buenos Aires") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())

 
#Los de los partidos del Conurbano + CABA no se leen muy bien as� que hago un mapa s�lo con ellos.
departamentos_amba <- departamentos %>% filter(provincia == "Buenos Aires" | provincia =="Ciudad Aut�noma de Buenos Aires")
smn_amba <- smn %>% filter(Provincia == "BUENOS AIRES" | Provincia == "CAPITAL FEDERAL")

bbox_new <- st_bbox(departamentos_ba)
bbox_new[1] <- -59
bbox_new[2] <- -35
bbox_new[3] <- -57.8
bbox_new[4] <- -34.4
bbox_new  
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()

ggplot() + geom_sf(data = departamentos_amba,fill = c('seashell'), color = "slategray") + 
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1],ylim = st_coordinates(bbox_new)[c(2,3),2])+ 
  geom_point(data = smn_amba, aes(x=x, y=y),colour = "dodgerblue1", size = 3)+ theme_classic() +
  geom_text(data= smn_amba, aes(x,y, label = Estacion), size = 2, fontface = "bold", vjust = -0.5)+
  ggtitle("Estaciones Meteorol�gicas", subtitle = "Area Metropolitana") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())



#####################################
##### temperatura historicos #######
#####################################

temperatura_historico <- temperatura_historico %>% gather(Mes, temp, Enero:Diciembre)
smn_temperatura_h <- merge(x = temperatura_historico, y = smn, by = "Estacion", all.x = TRUE)

smn_temperatura_h <- smn_temperatura_h %>% filter(!is.na(x) & !is.na(y) & !is.na(temp)) #filtro NA


colnames(smn_temperatura_h)
smn_temperatura_h <- smn_temperatura_h[,c(14,15,3,1)]

#Transformo los datos a geodata
temp_geodata <- as.geodata(smn_temperatura_h)
class(temp_geodata)

#An�lisis descriptivo
#Aca vemos que en una tendencia en las coordenadas y.
#Vemos que tenemos m�s observaciones con temperaturas maximas entre 20 y 30 grados centigrados
# Hay una clara tendencia mirando la latitud, lo que hace sentido en Argentina por tener 
# mayor variaci�n clim�tica de norte a sur que de este a oeste.

plot(temp_geodata)

#Ac� cambios los datos de data frame a sf
temp_sf_h <- (smn_temperatura_h)%>% dplyr::select(x,y,temp) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
class(temp_sf_h)

#En detalle vemos la distribuci�n de la temperatura. En concordancia, vemos mayores temperaturas al norte que al sur.
options(sf_max.plot=1)
plot(temp_sf_h, main = "Temperatura media hist�rica")

#Analizo normalidad 
#QQplot
qqPlot(smn_temperatura_h$temp, ylab="Temperatura", main = "QQPlot Temperatura",col.lines = "indianred", grid= FALSE)


#Test de Kolmogorov-Smirnov.
normal1 <- rnorm(length(smn_temperatura_h$temp), mean(smn_temperatura_h$temp), sd(smn_temperatura_h$temp))
ks.test(smn_temperatura_h$temp, normal1)

#test de shapiro
shapiro.test(smn_temperatura_h$temp)


#Pruebo dos transformaciones

#normalizar datos
transformacion1 <- smn_temperatura_h
transformacion1$temp <- (transformacion1$temp- mean(transformacion1$temp))/var(transformacion1$temp)


normal2<- rnorm(length(transformacion1$temp), mean(transformacion1$temp), sd(transformacion1$temp))
ks.test(transformacion1$temp, normal2)
shapiro.test(transformacion1$temp)

#log transformacion
transformacion2 <- smn_temperatura_h
#desplazo para no tener indefinido del log de 0
transformacion2$temp <- log(transformacion2$temp +min(transformacion2$temp)*-1 + 0.1)

normal3 <- rnorm(length(transformacion2$temp), mean(transformacion2$temp), sd(transformacion2$temp))
ks.test(transformacion2$temp, normal3)
shapiro.test(transformacion2$temp)


########################################################################################
###################################  DATOS HISTORICOS HUMEDAD #########################
########################################################################################



humedad_historico <- humedad_historico %>% gather(Mes, HUM, Enero:Diciembre)
smn_humedad_h <- merge(x = humedad_historico, y = smn, by = "Estacion", all.x = TRUE)
smn_humedad_h <- smn_humedad_h %>% filter(!is.na(x) & !is.na(y) & !is.na(HUM))


colnames(smn_humedad_h)
smn_humedad_h <- smn_humedad_h[,c(14,15,3,1)]
#Transformo a geodata
hum_geodata <- as.geodata(smn_humedad_h)
class(hum_geodata)
#Aca vemos que en la humedad no hay diferencias tan marcadas en regiones de pa�s como en el caso de temperaturas.
#No se ve una tendencia en las coordenadas y.

plot(hum_geodata)
hum_sf_h <- (smn_humedad_h)%>% dplyr::select(x,y,HUM) %>% st_as_sf(coords =c("x", "y"), crs= 4326)


#En detalle vemos la distribuci�n de humedad. La mayor parte del pa�s corresponde a puntos de humedad entre un 60% y 70%.
#Algunas regiones destacan por tener valores muy altos de humedad, como es Iguaz�. Los valores m�s bajos son algunas zonas de la Patagonia y de Cuyo.
options(sf_max.plot=1)
plot(hum_sf_h,breaks = c(0,10,20,30,40,50,60,70,80,90,100),pch =15 ,cex = 1, main = "Humedad hist�rica")


#Analisis de normalidad 
qqPlot(smn_humedad_h$HUM, ylab = "Humedad", main = "QQPlot Histograma",col.lines="indianred", grid= FALSE)
#Test de Kolmogorov-Smirnov.
normal1 <- rnorm(length(smn_humedad_h$HUM), mean(smn_humedad_h$HUM), sd(smn_humedad_h$HUM))
ks.test(smn_humedad_h$HUM, normal1)

#Test de Shapiro 
shapiro.test(smn_humedad_h$HUM)
normal3 <- rnorm(length(smn_humedad_h$HUM), mean(smn_humedad_h$HUM), sd(smn_humedad_h$HUM))
ks.test(smn_humedad_h$HUM, normal3)


#transformaciones 
#normalizar datos 
transformacion1 <- smn_humedad_h
transformacion1$HUM <- (transformacion1$HUM - mean(transformacion1$HUM))/var(tranformacion1$HUM)
normal4 <- rnorm(length(transformacion1$HUM), mean(transformacion1$HUM), sd(smn_humedad_h$HUM))
ks.test(transformacion1$HUM, normal4)
shapiro.test(transformacion1$HUM)

#log transformaci�n 
transformacion2 <- smn_humedad_h
transformacion2$HUM  <- log(transformacion2$HUM) 
normal5 <- rnorm(length(transformacion2$HUM), mean(transformacion2$HUM), sd(transformacion2$HUM))
ks.test(transformacion2$HUM, normal5)
shapiro.test(transformacion2$HUM)


#preparo para presentaci�n
par(mfrow=c(2,2))

qqPlot(smn_temperatura_h$temp, ylab="Temperatura", main = "QQPlot Temperatura",col.lines = "indianred", grid= FALSE)
qqPlot(smn_humedad_h$HUM, ylab = "Humedad", main = "QQPlot Histograma",col.lines="indianred", grid= FALSE)

#Vemos que ac� hay colas menos pesadas que en el caso de humedad y con una distribuci�n m�s normal.
hist(smn_temperatura_h$temp, xlab = "Temperatura ",col="gray20", main="Histograma temperatura",prob=TRUE)
lines(density(smn_temperatura_h$temp), # density plot
      lwd = 4, # thickness of line
      col = "indianred")
hist(smn_humedad_h$HUM, xlab = "Temperatura m�xima",col="gray20", main="Histograma humedad",prob=TRUE) 
lines(density(smn_humedad_h$HUM),lwd=4, col="indianred")




par(mfrow=c(1,1))
# AUTOCORRELACION ESPACIAL

# Agrupo por mayor facilidad los datos
smn_hum_h_group <- smn_humedad_h %>% 
  arrange(x, y, HUM) %>% 
  group_by(Estacion) %>%
  mutate(HUM.med = median(HUM)) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(x,y,HUM.med, Estacion) 

#PUNTOS

pares <- smn_hum_h_group %>% dplyr::select(x, y) %>% filter(!is.na(x), !is.na(y))
class(pares)
coordinates(pares) <- ~x+y

#Ahora voy a buscar los vecinos. Este objeto tiene para cada uno con cuantos se relaciona
# Ac� habr�a que analizar bien que pasa con este �ltimo par�metro del vecindario que me dice
# que los vecinos est�n a una distancia menor a 5,10,etc. Esto cambia mucho los vecindarios

k1 <- knn2nb(knearneigh(pares))
all.linked <- max(unlist(nbdists(k1, pares)))
#Miro entonces qu� distancia maxima tiene que haber para que est�n todos conectados.
print(all.linked)

pares_grilla <- dnearneigh(pares, 0, all.linked)
class(pares_grilla)
#Voy a ver todos los puntos en el mapa y sus relaciones
plot(pares_grilla, pares)

#Hace pesos para los vecinos dividiendo 1 por la cantidad de vecinos que ese punto tiene.
pesos_grilla <- nb2listw(pares_grilla, style = "W", zero.policy=TRUE )

#Vemos que s� estan autocorrelacionados espacialmente.
options(scipen=2)
moran.test(smn_hum_h_group$HUM.med, pesos_grilla)

par(mfrow= c(1,1))
par(mar=c(4,4,1.5,0.5))

#Graficamos
mp <- moran.plot(smn_hum_h_group$HUM.med, listw=pesos_grilla, zero.policy=TRUE, 
           pch=16, col="black",cex=.5, quiet=TRUE,
           xlim=c(50,80),ylim=c(60,75),
           labels=as.character(smn_hum_h_group$Estacion))
moran.plot(as.vector(scale(smn_hum_h_group$HUM.med)), pesos_grilla,
           labels=as.character(smn_hum_h_group$Estacion), xlim=c(-2, 4), ylim=c(-2,4), pch=19)
if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") +  
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    ggtitle("Moran I Scatter Plot" ,round(moran.test(smn_hum_h_group$HUM.med, pesos_grilla)[3]$estimate[1],3))+
    xlab("Humedad") + ylab(paste0("Spatially lagged ", "Humedad"))
}

# VARIOGRAMA

h_var <- smn_hum_h_group %>% dplyr::select(HUM.med, x,y)
coordinates(h_var) <-~y+x
variograma <-variogram(HUM.med ~x, h_var, width = 1.2, cutoff = 13)

ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 40) +
  labs(title = expression("Variograma emp�rico de Humedad"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))

#vemos toda la nube

variograma_nube <- variogram(HUM.med~x, h_var, cloud =TRUE)
ggplot(variograma_nube, aes(x = dist, y = gamma)) +
  geom_point(colour = tayloRswift::swift_palettes$lover[4], size = 2) + ylim(0, 3) +
  labs(title = expression("Variograma nube de Humedad"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 15, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 10, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 15, face = "bold"),
                        axis.title = element_text(size = 15, face = "bold"))

### An�lisis de isotrop�a

#Que no sea todo del mismo color me indica que habr�a isotrop�a en la data.
v1 <- variogram(HUM.med ~x, h_var,cutoff=30, width=1.2,  map = T)
plot(v1)
v1_sin_tendencia<- variogram(HUM.med~1, h_var,cutoff=30, width=1.2,  map = T)
plot(v1_sin_tendencia)
par(mfrow=c(2,2))
#Lo miro ahora en las cuatro direcciones posibles
v.dir <-variogram(HUM.med ~x, h_var,alpha = (0:3) * 45,width=1.2)
v.anis <- vgm(psill = 30, "Sph", 12, anis = c(0, 0.9),nugget=4)
plot(v.dir, v.anis, main = "Variogramas - Te�ricos Humedad")

v.dir$direction <- factor(v.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))
par(mfrow=c(1,1))
#Vemos que dentro de todo los variogramas emp�ricos no son tan distintos entre s�. El m�s distinto, tanto en comportamiento como en direcci�n es el de la direcci�n Norte-Sur.
ggplot(v.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Humedad"), 
       x = "distance", y = "semivariance", colour = "direcci�n") + geom_line(size=1.5)+
  scale_color_manual(values=wes_palette("Cavalcanti1"))+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 20),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 20, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 20, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 25, face = "bold"),
                        axis.title = element_text(size = 20, face = "bold"))

variograma <- variogram(HUM.med ~x, h_var, alpha = c(45,45),width=1.2,cutoff = 13)
dat_fit_hum <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, 
                             vgm(psill = 30, "Lin", 15, nugget=10))#, anis = c(0, 0.9)))
plot(variograma, dat_fit_hum)

# KRIGGING

class(departamentos)
departamentos_sina <- departamentos %>% filter(departamen != "Ant�rtida Argentina", departamen != "Islas del Atl�ntico Sur")
class(departamentos_sina)
dep_grilla <- as_Spatial(departamentos_sina)
class(dep_grilla)

grilla <- as.data.frame(spsample(dep_grilla, "regular", n = 5000))
names(grilla) <- c("x", "y")
coordinates(grilla) <- c("x", "y")
plot(grilla)

gridded(grilla) <- TRUE
fullgrid(grilla) <- TRUE

proj4string(grilla) <- proj4string(dep_grilla)
proj4string(h_var) <- proj4string(dep_grilla)

dar_krg <- krige(HUM.med ~ x,h_var,grilla, model = dat_fit_hum, nmax = 19)
summary(dar_krg$var1.pred)

r <- raster(dar_krg, layer = "var1.pred" )
r.m <- mask(r, dep_grilla)

tm_shape(r.m) +
  tm_raster(n = 15, palette = "magma") +
  tm_shape(h_var) + tm_dots(size = .1) +
  tm_legend(legend.outside = TRUE)+ tm_layout(title = "Kriging de Humedad")


ver <- as.data.frame(r)
ver[is.na(ver),]<- 0
#Analizo cuantos centros ver
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}
#sale �ptimo 3 o 4 

wssplot(ver, nc = 20)
cluster.temp <- kmeans(ver, 3) ### kmeans, with 4 clusters

clusters <- raster(r)

clusters <- setValues(clusters, cluster.temp$cluster) 
clusters
cluster.m1 <- mask(clusters, dep_grilla)
plot(cluster.m1)

cluster.temp

humedad.r<-r

## ******* TEMPERATURA ******* ##


smn_temp_h <- merge(x = temperatura_historico, y = smn, by = "Estacion", all.x = TRUE)
colnames(smn_temp_h)
smn_temp_h <- smn_temp_h %>% filter(!is.na(x) & !is.na(y) & !is.na(temp))





colnames(smn_temp_h)
smn_temp_h <- smn_temp_h[,c(14,15,3,1)]
temp_geodata <- as.geodata(smn_temp_h)
class(temp_geodata)

#Aca vemos que, si bien las temperaturas medias de entre 10�C y 20�C se repiten en varias regiones del pa�s, las temperaturas m�s fr�as y c�lidas registradas se diferencian fuertemente entre norte y sur.

temp_sf_h <- (smn_temp_h)%>% dplyr::select(x,y,temp) %>% st_as_sf(coords =c("x", "y"), crs= 4326)


# AUTOCORRELACION ESPACIAL

#PUNTOS

smn_temp_h_group <- smn_temp_h %>% 
  arrange(x, y, temp) %>% 
  group_by(Estacion) %>%
  mutate(temp.med = median(temp)) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(x,y,temp.med, Estacion) 

pares <- smn_temp_h_group %>% dplyr::select(x, y) %>% filter(!is.na(x), !is.na(y))
class(pares)
coordinates(pares) <- ~x+y

#Ahora voy a buscar los vecinos. Este objeto tiene para cada uno con cuantos se relaciona
# Ac� habr�a que analizar bien que pasa con este �ltimo par�metro del vecindario que me dice
# que los vecinos est�n a una distancia menor a 10,15,20,25. Esto cambia mucho los vecindarios
#Miro entonces qu� distancia maxima tiene que haber para que est�n todos conectados.
k1 <- knn2nb(knearneigh(pares))
all.linked <- max(unlist(nbdists(k1, pares)))
print(all.linked)

pares_grilla <- dnearneigh(pares, 0, all.linked)
class(pares_grilla)
#Voy a ver todos los puntos en el mapa y sus relaciones
plot(pares_grilla, pares)

#Hace pesos para los vecinos dividiendo 1 por la cantidad de vecinos que ese punto tiene.
pesos_grilla <- nb2listw(pares_grilla, style = "W", zero.policy=TRUE )

#Vemos que estan autocorrelacionados espacialmente.
options(scipen=2)
moran.test(smn_temp_h_group$temp.med, pesos_grilla)
#Graficamos
par(mfrow = c(1,1))
par(mar=c(4,4,1.5,0.5))
mp <- moran.plot(smn_temp_h_group$temp.med, listw=pesos_grilla, zero.policy=TRUE, 
           pch=16, col="black",cex=.5, quiet=TRUE,
           xlim=c(5,25),ylim=c(5,25),
           labels=as.character(smn_temp_h_group$Estacion))
moran.plot(as.vector(scale(smn_temp_h_group$temp.med)), pesos_grilla,
           labels=as.character(smn_temp_h_group$Estacion), xlim=c(-2, 4), ylim=c(-2,4), pch=19)
if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") +  
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    ggtitle("Moran I Scatter Plot" ,round(moran.test(smn_temp_h_group$temp.med, pesos_grilla)[3]$estimate[1],3))+
    xlab("Temperatura") + ylab(paste0("Spatially lagged ", "Temperatura"))
}
# VARIOGRAMA 

t_var <- smn_temp_h_group %>% dplyr::select(temp.med, x,y)
coordinates(t_var) <-~y+x
variograma <-variogram(temp.med ~ y, t_var, width  = 1.2)

ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 12) +
  labs(title = expression("Variograma emp�rico de Temperaturas"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        axis.title.x =   element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))

#vemos toda la nube
variograma_nube <- variogram(temp.med~y, t_var, cloud =TRUE)
ggplot(variograma_nube, aes(x = dist, y = gamma)) +
  geom_point(colour = tayloRswift::swift_palettes$evermore[4], size = 2) + ylim(0, 3) +
  labs(title = expression("Variograma nube de Temperatura"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 15, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 15, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 15, face = "bold"),
                        axis.title = element_text(size = 15, face = "bold"))

### An�lisis de isotrop�a

v2 <- variogram(temp.med ~y, t_var,cutoff=30, width=1.2,  map = T)
v2_sintendencia <- variogram(temp.med ~1, t_var,cutoff=30, width=1.2  ,map = T)
# Que sea todo del mismo color me dice que el proceso es isotr�pico porque solo mira la magnitud,
# no si es algo de norte a sur, etc Porque el mapa me dice que me muevo de norte a sur y no hay cambios debido a eso.
plot(v2)
plot(v2_sintendencia)

v2.dir <-variogram(temp.med~y, t_var,alpha = (0:3) * 45)
v2.anis <- vgm(psill = 10, "Lin", 13, anis = c(90, 0.9),nugget=0)
plot(v2.dir, v2.anis, main = "Variogramas - Te�ricos Temperatura")

v2.dir$direction <- factor(v2.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))

#Vemos aca que en el caso de la temperatura, la diferencia de Norte a Sur es la que sigue teniendo mayor correlaci�n espacial a�n a distancias grandes.
#En humedad esta diferencia se ve a�n m�s entre Este y Oeste, que se refleja por una zona cuyana y cordillerana muy seca en comparaci�n al litoral.
ggplot(v2.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Temperatura"), 
       x = "distancia", y = "semivariance", colour = "direcci�n") + geom_line(size=1.5)+
  scale_color_manual(values=tayloRswift::swift_palettes$taylor1989)+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        legend.title = element_text(size = 10),
                        legend.text = element_text(size = 10),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 15, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 10, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 15, face = "bold"),
                        axis.title = element_text(size = 13, face = "bold"))


variograma <- variogram(temp.med~y, t_var, width = 1.2)
dat_fit_tem <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, 
                             vgm(psill = 5.5, "Exp", 9,nugget=0.2))#, anis = c(0, 0.9)))
plot(variograma, dat_fit_tem)

# KRIGGING

proj4string(t_var) <- proj4string(dep_grilla)

dar_krg <- krige(temp.med ~ y,t_var,grilla, model = dat_fit_tem, nmax = 25)
summary(dar_krg$var1.pred)

r <- raster(dar_krg, layer = "var1.pred" )
r.m <- mask(r, dep_grilla)

tm_shape(r.m) +
  tm_raster(n = 15, palette = wes_palette("Zissou1")) +
  tm_shape(t_var) + tm_dots(size = .1) +
  tm_legend(legend.outside = TRUE)+ tm_layout(title = "Kriging de Temperatura")

ver <- as.data.frame(r)
ver[is.na(ver),]<- 0

wssplot(ver, nc = 20)
#vemos que el �ptimo tambi�n est� en el 3 3 aprox
cluster.hum <- kmeans(ver, 4) ### kmeans, with 3 clusters

clusters <- raster(r)

clusters <- setValues(clusters, cluster.hum$cluster) 
clusters
cluster.m2 <- mask(clusters, dep_grilla)
plot(cluster.m2)
cluster.hum
temperatura.r<- r
####################################
####### Precios de campo ##########
###################################

#Ahora voy a ver la localizaci?n de los precios de tierra
summary(campos)
colnames(campos) <- c("y", "x", "precio")
describe(campos)
#etiquetas <- campos
#etiquetas<- etiquetas[order(etiquetas$y),]
#etiquetas <- etiquetas[-c(111,108,103,94,93,85,18,90, 89, 87,49,29,53,56),]

`%notin%` <- Negate(`%in%`)
departamentos_sin_tf <- departamentos %>% filter(departamentos$departamen %notin%  departamentos_tf$departamen)
ggplot() + geom_sf(data = departamentos_sin_tf,fill=c("white"), color = "slategray",size = 0.10) + geom_point(data = campos, aes(x=x, y=y,fill=precio,color=precio))+ theme_classic()
par(mfrow=c(2,1))
qqPlot(campos$precio,ylab="Precios", main = "QQPlot Precios",col.lines = "indianred", grid= FALSE)
hist(campos$precio, xlab = "Precio minimo",col="gray20", xlim=c(100,20000), main="Histograma precio minimo",prob=TRUE)
lines(density(campos$precio), # density plot
  lwd = 2, # thickness of line
      col = "indianred")
#transformo a geodata

campo.geodata <- as.geodata(campos)
plot(campo.geodata)
campo.coord <- preciocampos %>% st_as_sf(coords =c("x", "y"), crs= 4326)



par(mfrow=c(1,1))

#PUNTOS
pares <- campos %>% filter(!is.na(x), !is.na(y))
class(pares)
coordinates(pares) <- ~x+y



#Ahora voy a buscar los vecinos. Este objeto tiene para cada uno con cuantos se relaciona
# Ac� habr�a que analizar bien que pasa con este �ltimo par�metro del vecindario que me dice
# que los vecinos est�n a una distancia menor a 10,15,20,25. Esto cambia mucho los vecindarios
#Miro entonces qu� distancia maxima tiene que haber para que est�n todos conectados.
k1 <- knn2nb(knearneigh(pares))
all.linked <- max(unlist(nbdists(k1, pares)))
print(all.linked)


pares_grilla <- dnearneigh(pares, 0, all.linked)
class(pares_grilla)
#Voy a ver todos los puntos en el mapa y sus relaciones
plot(pares_grilla, pares)



#Hace pesos para los vecinos dividiendo 1 por la cantidad de vecinos que ese punto tiene.
pesos_grilla <- nb2listw(pares_grilla, style = "W", zero.policy=TRUE )
#Vemos que estan autocorrelacionados espacialmente.
options(scipen=20)
moran.test(campos$precio, pesos_grilla)

#Graficamos
par(mfrow=c(1,1))
mp <- moran.plot(campos$precio, listw=pesos_grilla, zero.policy=TRUE, 
                 pch=16, col="black",cex=.5, quiet=TRUE,
                 xlim=c(50,80),ylim=c(60,75))
moran.plot(as.vector(scale(campos$precio)), pesos_grilla, pch=19, xlab ="Precios", ylab="Spatially lagged precios scaled")
if (require(ggplot2, quietly=TRUE)) {
  xname <- attr(mp, "xname")
  ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") +  
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    ggtitle("Moran I Scatter Plot" ,round(moran.test(campos$precio, pesos_grilla)[3]$estimate[1],3))+
    xlab("Precio minimo") + ylab(paste0("Spatially lagged ", "Precio minimo"))
}

# VARIOGRAMA 

c_var <- campos %>% filter(!is.na(x), !is.na(y))
coordinates(c_var) <-~x+y
variograma <-variogram( precio~1, c_var)

loadfonts(device = "win")
windowsFonts()
ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 30000000) +
  labs(title = expression("Variograma emp�rico de Precios"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        axis.title.x =   element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))

set.seed(99)
kmncluster <- kmeans(na.omit(campos$precio), centers = 3, iter.max = 500, nstart = 5, algorithm="Lloyd")
kmncluster$cluster

precio_cluster <- campos
precio_cluster$cluster <- kmncluster$cluster
precio_cluster
class(precio_cluster)

ggplot() + 
  geom_sf(data = departamentos_sin_tf,fill = c('seashell'), color = "slategray",size = 0.50) + 
  geom_point(data = precio_cluster, aes(x=x, y=y, colour = cluster), size = 4 )+ 
  scale_color_gradientn(colours = wes_palette(n=5, name="Zissou1")) + theme_classic()

variograma_nube <- variogram(precio~1, c_var, cloud =TRUE)
ggplot(variograma_nube, aes(x = dist, y = gamma)) +
  geom_point(colour = tayloRswift::swift_palettes$lover[4], size = 2) +
  labs(title = expression("Variograma nube de Humedad"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 15, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 10, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 15, face = "bold"),
                        axis.title = element_text(size = 15, face = "bold"))

### An�lisis de isotrop�a

#Que no sea todo del mismo color me indica que habr�a isotrop�a en la data.
v1 <- variogram( precio~1, c_var,cutoff=30, width=1.2,  map = T)
plot(v1)

par(mfrow=c(2,2))
#Lo miro ahora en las cuatro direcciones posibles
v.dir <-variogram(precio ~x, c_var,alpha = (0:3) * 45,width=1.2)
v.anis <- vgm(psill = 30, "Sph", 12, anis = c(0, 0.9),nugget=4)
plot(v.dir, v.anis, main = "Variogramas - Te�ricos Humedad")

v.dir$direction <- factor(v.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))
par(mfrow=c(1,1))
#Vemos que dentro de todo los variogramas emp�ricos no son tan distintos entre s�. El m�s distinto, tanto en comportamiento como en direcci�n es el de la direcci�n Norte-Sur.
ggplot(v.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Precios de campo"), 
       x = "distance", y = "semivariance", colour = "direcci�n") + geom_line(size=1.5)+
  scale_color_manual(values=wes_palette("Cavalcanti1"))+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 20),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 20, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 20, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 25, face = "bold"),
                        axis.title = element_text(size = 20, face = "bold"))

variograma <- variogram(precio ~1, c_var, alpha = c(45,45),width=1.2,cutoff = 13)
dat_fit_hum <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, 
                             vgm(psill = 1000000, "Sph", 15, nugget=4000000))#, anis = c(0, 0.9)))
plot(variograma, dat_fit_hum)

# KRIGGING

class(departamentos)
departamentos_sina <- departamentos %>% filter(departamen != "Ant�rtida Argentina", departamen != "Islas del Atl�ntico Sur")
#deptos <- departamentos_sina %>% filter(provincia %notin% c("Tierra del Fuego", "Santa Cruz","Neuqu�n", "R�o Negro","Chubut"))
deptos <- departamentos_sina
class(deptos)
dep_grilla <- as_Spatial(deptos)
class(dep_grilla)

grilla <- as.data.frame(spsample(dep_grilla, "regular", n = 5000))
names(grilla) <- c("x", "y")
coordinates(grilla) <- c("x", "y")
plot(grilla)

gridded(grilla) <- TRUE
fullgrid(grilla) <- TRUE

proj4string(grilla) <- proj4string(dep_grilla)
proj4string(c_var) <- proj4string(dep_grilla)

dar_krg <- krige(precio ~ 1,c_var,grilla, model = dat_fit_hum, nmax = 19)
summary(dar_krg$var1.pred)

r <- raster(dar_krg, layer = "var1.pred" )
r.m <- mask(r, dep_grilla)

tm_shape(r.m) +
  tm_raster(n = 20, palette = "magma") +
  tm_shape(c_var) + tm_dots(size = .1) +
  tm_legend(legend.outside = TRUE)+ tm_layout(title = "Kriging de Precios")






ver <- as.data.frame(r)
ver[is.na(ver),]<- 0

wssplot(ver,nc=20)
cluster.precio <- kmeans(ver, 3) ### kmeans, with 4 clusters

clusters <- raster(r)
## create an empty raster with same extent than ICE
clusters <- setValues(clusters, cluster.precio$cluster) 
clusters
cluster.m <- mask(clusters, dep_grilla)
plot(cluster.m)

par(mfrow=c(1,3))
plot(cluster.m1, main="cluster humedad")
plot(cluster.m2, main="cluster temperatura")
plot(cluster.m, main="cluster precios")

cluster.precio$centers
cluster.hum$centers
cluster.temp$centers
precio.r <- r

par(mfrow=c(1,1))

m = glm(precio.r[]~temperatura.r[]+humedad.r[])
summary(m)



#new_data <- raster::merge(r1, r2,r3, tolerance = 0.5)

#head(new_data)
#ver <- as.data.frame(new_data)
#ver[is.na(ver),]<- 0

#wssplot(ver,nc=20)
#cluster.todos <- kmeans(ver, 8) ### kmeans, with 4 clusters

#clusters <- raster(new_data)
## create an empty raster with same extent than ICE
#clusters <- setValues(clusters, cluster.todos$cluster) 
#clusters
#cluster.m4 <- mask(clusters, dep_grilla)
#plot(cluster.m4)



####Producciones de cultivo

unique(estimaciones$Cultivo)
produccion <- estimaciones %>%  filter(Campana == "2019/20") %>% 
  mutate(Produccion = Produccion%>% as.numeric()) %>% 
  group_by(Cultivo) %>% 
  summarise(prod = sum(Produccion))
head(produccion)

ggplot(aes(x = reorder(Cultivo, prod), y = prod), data = produccion) +
  geom_bar(stat = 'identity',  col = "black", fill = wes_palette("Zissou1")[4]) +coord_flip()+
ggtitle('Producci�n por cultivo campa�a 2019/20') + theme_classic()+
  theme(axis.title.x =   element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
        axis.line = element_line(colour = "black"),plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))

soja <- estimaciones %>%  filter(Cultivo == "Soja total" | Cultivo == "Soja 1era" | Cultivo== "Soja 2da"& Campana == "2019/20") %>% 
  mutate(Produccion = Produccion%>% as.numeric()) %>% 
  group_by(Provincia) %>% 
  summarise(prod = sum(Produccion))
head(soja)
ggplot(aes(x = reorder(Provincia, prod), y = prod), data = soja) +
  geom_bar(stat = 'identity',  col = "black", fill = wes_palette("Zissou1")[4]) +coord_flip()+
  geom_text(aes(label = prettyNum(prod,big.mark=".",scientific=FALSE)),stat = 'identity', size =5, vjust = 0.5, hjust = 1, fontface = "bold")+
  theme(text = element_text(family = "Raleway"))+
  ggtitle('Producci�n de soja campa�a 2019/20') + theme_classic()+
  theme(text = element_text(family = "Raleway Medium"),
        axis.title.x =   element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(), axis.text.y = element_text(size = 18),
        panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
        axis.line = element_line(colour = "black"),plot.title = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"))

maiz <- estimaciones %>%  filter(Cultivo == "Maiz"& Campana == "2019/20") %>% 
  mutate(Produccion = Produccion%>% as.numeric()) %>% 
  group_by(Provincia) %>% 
  summarise(prod = sum(Produccion))
head(maiz)

ggplot(aes(x = reorder(Provincia, prod), y = prod), data = maiz) +
  geom_bar(stat = 'identity',  col = "black", fill = wes_palette("GrandBudapest1")[1]) +coord_flip()+
  theme(text = element_text(family = "Raleway"))+
  ggtitle('Producci�n de ma�z campa�a 2019/20') + theme_classic()+
  theme(text = element_text(family = "Raleway Medium"),
        axis.title.x =   element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text.x = element_blank(), 
        axis.line = element_line(colour = "black"),plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))



