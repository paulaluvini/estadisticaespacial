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
campos <- read_excel("Datos/precio campos.xlsx")

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

#Ahora voy a ver la localización de las estaciones
summary(smn)
colnames(smn)
#Cambio este nombre para que se lea mejor
smn <- smn %>% rename(Estacion = NOMBRE) 
summary(smn$Provincia)
describe(smn)

# Vemos cómo se distribuyen las estaciones por provincia.
# La provincia con mayores estaciones es, por lejos, Buenos Aires. La siguen Córdoba y Santa Fe.
#View(smn %>% group_by(Provincia) %>% tally(sort = TRUE))

# Acá tengo que sumarle los minutos a la latitud porque vienen separados

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
  ggtitle("Estaciones Meteorológicas", subtitle = "Provincia de Buenos Aires") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())

 
#Los de los partidos del Conurbano + CABA no se leen muy bien así que hago un mapa sólo con ellos.
departamentos_amba <- departamentos %>% filter(provincia == "Buenos Aires" | provincia =="Ciudad Autónoma de Buenos Aires")
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
  ggtitle("Estaciones Meteorológicas", subtitle = "Area Metropolitana") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())



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

#Análisis descriptivo
#Aca vemos que en una tendencia en las coordenadas y.
#Vemos que tenemos más observaciones con temperaturas maximas entre 20 y 30 grados centigrados
# Hay una clara tendencia mirando la latitud, lo que hace sentido en Argentina por tener 
# mayor variación climática de norte a sur que de este a oeste.

plot(temp_geodata)

#Acá cambios los datos de data frame a sf
temp_sf_h <- (smn_temperatura_h)%>% dplyr::select(x,y,temp) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
class(temp_sf_h)

#En detalle vemos la distribución de la temperatura. En concordancia, vemos mayores temperaturas al norte que al sur.
options(sf_max.plot=1)
plot(temp_sf_h, main = "Temperatura media histórica")

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
#Aca vemos que en la humedad no hay diferencias tan marcadas en regiones de país como en el caso de temperaturas.
#No se ve una tendencia en las coordenadas y.

plot(hum_geodata)
hum_sf_h <- (smn_humedad_h)%>% dplyr::select(x,y,HUM) %>% st_as_sf(coords =c("x", "y"), crs= 4326)


#En detalle vemos la distribución de humedad. La mayor parte del país corresponde a puntos de humedad entre un 60% y 70%.
#Algunas regiones destacan por tener valores muy altos de humedad, como es Iguazú. Los valores más bajos son algunas zonas de la Patagonia y de Cuyo.
options(sf_max.plot=1)
plot(hum_sf_h,breaks = c(0,10,20,30,40,50,60,70,80,90,100),pch =15 ,cex = 1, main = "Humedad histórica")


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

#log transformación 
transformacion2 <- smn_humedad_h
transformacion2$HUM  <- log(transformacion2$HUM) 
normal5 <- rnorm(length(transformacion2$HUM), mean(transformacion2$HUM), sd(transformacion2$HUM))
ks.test(transformacion2$HUM, normal5)
shapiro.test(transformacion2$HUM)


#preparo para presentación
par(mfrow=c(2,2))

qqPlot(smn_temperatura_h$temp, ylab="Temperatura", main = "QQPlot Temperatura",col.lines = "indianred", grid= FALSE)
qqPlot(smn_humedad_h$HUM, ylab = "Humedad", main = "QQPlot Histograma",col.lines="indianred", grid= FALSE)

#Vemos que acá hay colas menos pesadas que en el caso de humedad y con una distribución más normal.
hist(smn_temperatura_h$temp, xlab = "Temperatura ",col="gray20", main="Histograma temperatura",prob=TRUE)
lines(density(smn_temperatura_h$temp), # density plot
      lwd = 4, # thickness of line
      col = "indianred")
hist(smn_humedad_h$HUM, xlab = "Temperatura máxima",col="gray20", main="Histograma humedad",prob=TRUE) 
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
# Acá habría que analizar bien que pasa con este último parámetro del vecindario que me dice
# que los vecinos están a una distancia menor a 5,10,etc. Esto cambia mucho los vecindarios

k1 <- knn2nb(knearneigh(pares))
all.linked <- max(unlist(nbdists(k1, pares)))
#Miro entonces qué distancia maxima tiene que haber para que estén todos conectados.
print(all.linked)

pares_grilla <- dnearneigh(pares, 0, all.linked)
class(pares_grilla)
#Voy a ver todos los puntos en el mapa y sus relaciones
plot(pares_grilla, pares)

#Hace pesos para los vecinos dividiendo 1 por la cantidad de vecinos que ese punto tiene.
pesos_grilla <- nb2listw(pares_grilla, style = "W", zero.policy=TRUE )

#Vemos que sí estan autocorrelacionados espacialmente.
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
  labs(title = expression("Variograma empírico de Humedad"), 
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

### Análisis de isotropía

#Que no sea todo del mismo color me indica que habría isotropía en la data.
v1 <- variogram(HUM.med ~x, h_var,cutoff=30, width=1.2,  map = T)
plot(v1)
v1_sin_tendencia<- variogram(HUM.med~1, h_var,cutoff=30, width=1.2,  map = T)
plot(v1_sin_tendencia)
par(mfrow=c(2,2))
#Lo miro ahora en las cuatro direcciones posibles
v.dir <-variogram(HUM.med ~x, h_var,alpha = (0:3) * 45,width=1.2)
v.anis <- vgm(psill = 30, "Sph", 12, anis = c(0, 0.9),nugget=4)
plot(v.dir, v.anis, main = "Variogramas - Teóricos Humedad")

v.dir$direction <- factor(v.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))
par(mfrow=c(1,1))
#Vemos que dentro de todo los variogramas empíricos no son tan distintos entre sí. El más distinto, tanto en comportamiento como en dirección es el de la dirección Norte-Sur.
ggplot(v.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Humedad"), 
       x = "distance", y = "semivariance", colour = "dirección") + geom_line(size=1.5)+
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
departamentos_sina <- departamentos %>% filter(departamen != "Antártida Argentina", departamen != "Islas del Atlántico Sur")
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


## ******* TEMPERATURA ******* ##


smn_temp_h <- merge(x = temperatura_historico, y = smn, by = "Estacion", all.x = TRUE)
colnames(smn_temp_h)
smn_temp_h <- smn_temp_h %>% filter(!is.na(x) & !is.na(y) & !is.na(temp))





colnames(smn_temp_h)
smn_temp_h <- smn_temp_h[,c(14,15,3,1)]
temp_geodata <- as.geodata(smn_temp_h)
class(temp_geodata)

#Aca vemos que, si bien las temperaturas medias de entre 10°C y 20°C se repiten en varias regiones del país, las temperaturas más frías y cálidas registradas se diferencian fuertemente entre norte y sur.

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
# Acá habría que analizar bien que pasa con este último parámetro del vecindario que me dice
# que los vecinos están a una distancia menor a 10,15,20,25. Esto cambia mucho los vecindarios
#Miro entonces qué distancia maxima tiene que haber para que estén todos conectados.
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
  labs(title = expression("Variograma empírico de Temperaturas"), 
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

### Análisis de isotropía

v2 <- variogram(temp.med ~y, t_var,cutoff=30, width=1.2,  map = T)
v2_sintendencia <- variogram(temp.med ~1, t_var,cutoff=30, width=1.2  ,map = T)
# Que sea todo del mismo color me dice que el proceso es isotrópico porque solo mira la magnitud,
# no si es algo de norte a sur, etc Porque el mapa me dice que me muevo de norte a sur y no hay cambios debido a eso.
plot(v2)
plot(v2_sintendencia)

v2.dir <-variogram(temp.med~y, t_var,alpha = (0:3) * 45)
v2.anis <- vgm(psill = 10, "Lin", 13, anis = c(90, 0.9),nugget=0)
plot(v2.dir, v2.anis, main = "Variogramas - Teóricos Temperatura")

v2.dir$direction <- factor(v2.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))

#Vemos aca que en el caso de la temperatura, la diferencia de Norte a Sur es la que sigue teniendo mayor correlación espacial aún a distancias grandes.
#En humedad esta diferencia se ve aún más entre Este y Oeste, que se refleja por una zona cuyana y cordillerana muy seca en comparación al litoral.
ggplot(v2.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Temperatura"), 
       x = "distancia", y = "semivariance", colour = "dirección") + geom_line(size=1.5)+
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

#######Precios de campo
colnames(campos) <- c("names", "descripcion","x","y", "price1", "price2")
#camposb<- (campos)%>% dplyr::select(x,y,price1) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
ggplot() + geom_sf(data = departamentos,fill = c('seashell'), color = "slategray",size = 0.50) + geom_point(data = campos, aes(x=x, y=y),
                                                                                                            colour = "royalblue1", size = 1)+ theme_classic()

