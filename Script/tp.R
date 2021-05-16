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
loadfonts(device = "win")
windowsFonts()

# Primero leemos los archivos a utilizar
departamentos <- st_read("Datos/Codgeo_Pais_x_dpto_con_datos/pxdptodatosok.shp")
smn <- read.csv(file = 'Datos/estaciones_smn.csv', sep = ";")
temperaturas <- read.csv(file = 'Datos/temperaturas.csv')
datoshorarios <- read.csv(file = 'Datos/datoshorarios.csv')
humedad_historico <- read.csv(file = 'Datos/humedad_historico.csv')
temperatura_historico <- read.csv(file = 'Datos/temperatura_historico.csv')

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
smn <- smn %>% rename(Estacion =ï..NOMBRE) 
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

ggplot() + geom_sf(data = departamentos,fill = c('seashell'), color = "slategray",size = 0.50) + geom_point(data = smn, aes(x=x, y=y),
                                                      colour = "royalblue1", size = 1)+ theme_classic()

departamentos_ba <- departamentos %>% filter(provincia == "Buenos Aires")
smn_ba <- smn %>% filter(Provincia == "BUENOS AIRES")

ggplot() + geom_sf(data = departamentos_ba,fill = c('seashell'), color = "slategray") + 
  geom_point(data = smn_ba, aes(x=x, y=y),colour = "royalblue1", size = 2)+ 
  theme_classic() +geom_text(data= smn_ba, aes(x,y, label = Estacion), size = 2, fontface = "bold",vjust = -0.5)+
  ggtitle("Estaciones Meteorológicas", subtitle = "Provincia de Buenos Aires") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())

 # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                       # size = 0.5), panel.background = element_rect(fill = "aliceblue"))

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
grafico2
  #theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),
    #    panel.background = element_rect(fill = "aliceblue"))

########################################################################################
#DATOS DIARIOS

# TEMPERATURAS
# Miro el dataset de temperaturas datos diarios

head(temperaturas)
temperaturas <- temperaturas %>% rename(Estacion = NOMBRE) 
dim(temperaturas)[1]
colnames(temperaturas)
sort(unique(temperaturas$FECHA))

#Me quedo solo con 2020. Para no tener problemas de estacionalidad, si me quedo con 
#2 meses de enero dado que tengo 2020 y 2021 estaria teniendo temperaturas sesgadas.

for (i in 1:dim(temperaturas)[1]){
  if (nchar(temperaturas$FECHA[i]) == 8){
    temperaturas$YEAR[i] = substr(temperaturas$FECHA[i],5,nchar(temperaturas$FECHA[i]))
  }
  if (nchar(temperaturas$FECHA[i]) == 7){
    temperaturas$YEAR[i] = substr(temperaturas$FECHA[i],4,nchar(temperaturas$FECHA[i]))
  }
}

head(temperaturas)
temperaturas_2020 <- temperaturas[temperaturas$YEAR == 2020,]
dim(temperaturas_2020)

#Lo mergeo con el otro dataset
smn_temp <- merge(x = temperaturas_2020, y = smn, by = "Estacion", all.x = TRUE)
#agrego este dataset general que incluye todos los años
smn_temp_gral <- merge(x= temperaturas, y = smn, by="Estacion", all.x=TRUE)
#Filtro los na
smn_temp <- smn_temp %>% filter(!is.na(x) & !is.na(y) & !is.na(TMAX) & !is.na(TMIN))

smn_temp_gral <- smn_temp_gral %>% filter(!is.na(x) & !is.na(y) & !is.na(TMAX) & !is.na(TMIN))
class(smn_temp)
dim(smn_temp)
head(smn_temp)
colnames(smn_temp)

#Ahora agrupo.

temp_max_group <- smn_temp %>% 
  arrange(Estacion, x, y, TMAX) %>% 
  group_by(Estacion) %>%
  mutate(TMAX.med = median(TMAX)) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(x,y,TMAX.med) 

temp_min_group <- smn_temp %>% 
  arrange(Estacion, x, y, TMIN) %>% 
  group_by(Estacion) %>%
  mutate(TMIN = median(TMIN)) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(x,y,TMIN ) 

smn_temp_gral <- smn_temp_gral %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
class(smn_temp_gral)
temp_max <- smn_temp_gral[,c(16,3)]
#temp_min <- temp_max_group[,c(16,17,4)]
temp_min <- smn_temp_gral[,c(16,4)]

#filtro dataset para sacar antartida
smn_temp_sina <- smn_temp %>% filter(Provincia != "ANTARTIDA")

par(mfrow=c(2,2))
qqnorm(temp_max$TMAX, ylim=c(-30,50))
qqnorm(smn_temp_sina$TMAX, ylim =c(-30,50))

hist(temp_max$TMAX, xlab = "Temperatura máxima",col="slategray1", xlim=c(-30,60), main="Histograma temperatura",prob=TRUE)
lines(density(temp_max$TMAX), # density plot
      lwd = 2, # thickness of line
      col = "gray20")
hist(smn_temp_sina$TMAX, xlab = "Temperatura máxima",col="slategray1", xlim =c(-30,60), main="Histograma temperatura sin Antártida",prob=TRUE) 
lines(density(smn_temp_sina$TMAX),lwd=2, col="gray20")


##Para testear normalidad 
pvalue1 <- vector(length=5000)
pvalue2 <- vector(length=5000)
wvalue1 <- vector(length=5000)
wvalue2 <- vector(length = 5000)
set.seed(100)
#Test de Shapiro
for (i in 1:5000){
  pvalue1[i] <- shapiro.test(sample(temp_max$TMAX, 5000))$p.value
  wvalue1[i]<- shapiro.test(sample(temp_max$TMAX, 5000))$statistic
  pvalue2[i] <- shapiro.test(sample(smn_temp_sina$TMAX,5000))$p.value
  wvalue2[i] <- shapiro.test(sample(smn_temp_sina$TMAX,5000))$statistic
    }


mean(pvalue1)
mean(pvalue2)
mean(wvalue1)
mean(wvalue2)

#Test de Kolmogorov-Smirnov.
normal1 <- rnorm(length(temp_max$TMAX), mean(temp_max$TMAX), sd(temp_max$TMAX))
normal2 <- rnorm(length(smn_temp_sina$TMAX), mean(smn_temp_sina$TMAX), sd(smn_temp_sina$TMAX))
ks.test(temp_max$TMAX, normal1)
ks.test(smn_temp_sina$TMAX, normal2)

par(mfrow=c(1,1))
plot(temp_max,pch = 15 ,cex = 1)

#hago un analisis de la temperatura minima
ggplot() + geom_histogram(data=temp_min, aes(x= TMIN)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
  ggtitle("Histograma de temperatura mediana") + labs(x= "temperatura máxima")
qqnorm(temp_min$TMIN)


#Vemos que tenemos más observaciones con temperaturas maximas entre 20 y 30 grados centigrados
# Hay una clara tendencia mirando la latitud, lo que hace sentido en Argentina por tener 
# mayor variación climática de norte a sur que de este a oeste.
temp_max_geodata <- as.geodata(temp_max_group)
class(temp_max_geodata)
plot(temp_max_geodata)

# AUTOCORRELACION ESPACIAL

#PUNTOS
pares <- temp_max_group %>% dplyr::select(x, y) %>% filter(!is.na(x), !is.na(y))
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
options(scipen=20)
moran.test(temp_max_group$TMAX.med, pesos_grilla)


########################################################################################
###################################  DATOS HISTORICOS  #################################
########################################################################################

############################### ******* HUMEDAD ******* ################################

humedad_historico <- humedad_historico %>% gather(Mes, HUM, Enero:Diciembre)
smn_humedad_h <- merge(x = humedad_historico, y = smn, by = "Estacion", all.x = TRUE)
smn_humedad_h <- smn_humedad_h %>% filter(!is.na(x) & !is.na(y) & !is.na(HUM))

#hago un analisis de la humedad historica. Veo que tiene una cola bastante pronunciada hacia la derecha, hay mas valores de humedad por encima del 60%.
ggplot() + geom_histogram(data=smn_humedad_h, aes(x= HUM)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
  ggtitle("Histograma de humedad histórico") + labs(x= "Humedad") +theme(text = element_text(family = "JP1"),
                                                                         panel.grid.major = element_blank(),
                                                                         panel.grid.minor = element_blank(), axis.text.x = element_text(size = 10, family ="JP1", face = 'bold'), 
                                                                         axis.text.y = element_text(size = 10, family = "JP1", face = 'bold'), 
                                                                         axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                                                                         axis.title = element_text(size = 11, face = "bold"))

#Los datos no son normales, estan bastante sesgados a la cola derecha
qqnorm(smn_humedad_h$HUM)

#Test de Kolmogorov-Smirnov.
normal1 <- rnorm(length(smn_humedad_h$HUM), mean(smn_humedad_h$HUM), sd(smn_humedad_h$HUM))
ks.test(smn_humedad_h$HUM, normal1)


par(mfrow=c(1,1))
plot(temp_max,pch = 15 ,cex = 1)

colnames(smn_humedad_h)
smn_humedad_h <- smn_humedad_h[,c(14,15,3,1)]
hum_geodata <- as.geodata(smn_humedad_h)
class(hum_geodata)
#Aca vemos que en la humedad no hay diferencias tan marcadas en regiones de país como en el caso de temperaturas.
#No se ve una tendencia en las coordenadas y.
plot(hum_geodata)
hum_sf_h <- (smn_humedad_h)%>% dplyr::select(x,y,HUM) %>% st_as_sf(coords =c("x", "y"), crs= 4326)

#En detalle vemos la distribución de humedad. La mayor parte del país corresponde a puntos de humedad entre un 60% y 70%.
#Algunas regiones destacan por tener valores muy altos de humedad, como es Iguazú. Los valores más bajos son algunas zonas de la Patagonia y de Cuyo.
options(sf_max.plot=1)
plot(hum_sf_h,breaks = c(0,10,20,30,40,50,60,70,80,90,100),pch =15 ,cex = 1)

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
    xlab("Humedad") + ylab(paste0("Spatially lagged ", "Humedad"))
}

# VARIOGRAMA

h_var <- smn_hum_h_group %>% dplyr::select(HUM.med, x,y)
coordinates(h_var) <-~y+x
variograma <-variogram(HUM.med ~1, h_var)

ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 180) +
  labs(title = expression("Variograma empírico de Humedad"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))

#vemos toda la nube
variograma_nube <- variogram(HUM.med~1, h_var, cloud =TRUE)
plot(variograma_nube, main= "Variograma nube de Humedad")

dat_fit_hum <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, vgm(psill=200,model ="Lin", nugget =1,range=20))
plot(variograma, dat_fit_hum)

### Análisis de isotropía

#Que no sea todo del mismo color me indica que no habría isotropía en la data.
v1 <- variogram(HUM.med ~1, h_var,cutoff=50, width=5,  map = T)
plot(v1)

#Lo miro ahora en las cuatro direcciones posibles
v.dir <-variogram(HUM.med ~1, h_var,alpha = (0:3) * 45)
v.anis <- vgm(psill = 140, "Lin", 20, anis = c(45, 0.3),nugget=50)
plot(v.dir, v.anis)

v.dir$direction <- factor(v.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))

#Vemos que dentro de todo los variogramas empíricos no son tan distintos entre sí. El más distinto, tanto en comportamiento como en dirección es el de la dirección Norte-Sur.
ggplot(v.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Humedad"), 
       x = "distancia", y = "semivariance", colour = "dirección") + geom_line(size=1.5)+
  scale_color_manual(values=wes_palette("Cavalcanti1"))+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))


# KRIGGING

class(departamentos)
departamentos_sina <- departamentos %>% filter(departamen != "Antártida Argentina", departamen != "Islas del Atlántico Sur")
class(departamentos_sina)
dep_grilla <- as_Spatial(departamentos_sina)
class(dep_grilla)

grilla <- as.data.frame(spsample(dep_grilla, "regular", n = 5000))
names(grilla) <- c("X", "Y")
coordinates(grilla) <- c("X", "Y")
plot(grilla)

gridded(grilla) <- TRUE
fullgrid(grilla) <- TRUE

proj4string(grilla) <- proj4string(dep_grilla)
proj4string(h_var) <- proj4string(dep_grilla)

dar_krg <- krige(HUM.med ~ 1,h_var,grilla, model = dat_fit_hum, nmax = 20)
summary(dar_krg$var1.pred)

r <- raster(dar_krg, layer = "var1.pred" )
r.m <- mask(r, dep_grilla)

plot(r.m)

tm_shape(r.m) +
  tm_raster(n = 10, palette = wes_palette("Cavalcanti1")) +
  tm_shape(d_var) + tm_dots(size = .1) +
  tm_legend(legend.outside = TRUE)



                            ## ******* TEMPERATURA ******* ##

head(temperatura_historico)
temperatura_historico <- temperatura_historico %>% gather(Mes, TEMP, Enero:Diciembre)

smn_temp_h <- merge(x = temperatura_historico, y = smn, by = "Estacion", all.x = TRUE)
colnames(smn_temp_h)
smn_temp_h <- smn_temp_h %>% filter(!is.na(x) & !is.na(y) & !is.na(TEMP))

#hago un analisis de la temperatura historica
#Vemos que acá hay colas menos pesadas que en el caso de humedad y con una distribución más normal.
ggplot() + geom_histogram(data=smn_temp_h, aes(x= TEMP), binwidth = 1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
  ggtitle("Histograma de temperatura historico") + labs(x= "temperatura")

#COMPARO CON EL DE 2020
ggplot() + geom_histogram(data=temp_max, aes(x= TMAX), binwidth = 1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
  ggtitle("Histograma de temperatura maxima 2020") + labs(x= "temperatura maxima")

par(mfrow = c(1,2))
qqnorm(smn_temp_h$TEMP)
qqnorm(temp_max$TMAX)

#Test de Kolmogorov-Smirnov.
normal1 <- rnorm(length(smn_temp_h$TEMP), mean(smn_temp_h$TEMP), sd(smn_temp_h$TEMP))
ks.test(smn_temp_h$TEMP, normal1)

colnames(smn_temp_h)
smn_temp_h <- smn_temp_h[,c(14,15,3,1)]
temp_geodata <- as.geodata(smn_temp_h)
class(temp_geodata)

#Aca vemos que, si bien las temperaturas medias de entre 10°C y 20°C se repiten en varias regiones del país, las temperaturas más frías y cálidas registradas se diferencian fuertemente entre norte y sur.
plot(temp_geodata)
temp_sf_h <- (smn_temp_h)%>% dplyr::select(x,y,TEMP) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
options(sf_max.plot=1)
plot(temp_sf_h, pch =15 ,breaks = c(0,10,15,20,30),cex = 1)

# AUTOCORRELACION ESPACIAL

#PUNTOS

smn_temp_h_group <- smn_temp_h %>% 
  arrange(x, y, TEMP) %>% 
  group_by(Estacion) %>%
  mutate(TEMP.med = median(TEMP)) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(x,y,TEMP.med, Estacion) 

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
moran.test(smn_temp_h_group$TEMP.med, pesos_grilla)
#Graficamos
par(mfrow = c(1,1))
par(mar=c(4,4,1.5,0.5))
mp <- moran.plot(smn_temp_h_group$TEMP.med, listw=pesos_grilla, zero.policy=TRUE, 
           pch=16, col="black",cex=.5, quiet=TRUE,
           xlim=c(5,25),ylim=c(5,25),
           labels=as.character(smn_temp_h_group$Estacion))
moran.plot(as.vector(scale(smn_temp_h_group$TEMP.med)), pesos_grilla,
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
    xlab("Humedad") + ylab(paste0("Spatially lagged ", "Humedad"))
}
# VARIOGRAMA 

t_var <- smn_temp_h_group %>% dplyr::select(TEMP.med, x,y)
coordinates(t_var) <-~y+x
variograma <-variogram(TEMP.med ~1, t_var)

loadfonts(device = "win")
windowsFonts()
ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 20) +
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
variograma_nube <- variogram(TEMP.med~1, t_var, cloud =TRUE)
plot(variograma_nube, main= "Variograma nube de Temperatura")

dat_fit_tem <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, vgm(psill=20, model ="Lin", nugget =0,range=20))
plot(variograma, dat_fit_tem)

### Análisis de isotropía

v2 <- variogram(TEMP.med ~1, t_var,cutoff=50, width=5,  map = T)
# Que sea todo del mismo color me dice que el proceso es isotrópico porque solo mira la magnitud,
# no si es algo de norte a sur, etc Porque el mapa me dice que me muevo de norte a sur y no hay cambios debido a eso.
plot(v2)

v2.dir <-variogram(TEMP.med~1, t_var,alpha = (0:3) * 45)
v2.anis <- vgm(psill = 20, "Lin", 20, anis = c(45, 0.3),nugget=25)
plot(v2.dir, v2.anis)

v2.dir$direction <- factor(v2.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))

#Vemos aca que en el caso de la temperatura, la diferencia de Norte a Sur es la que sigue teniendo mayor correlación espacial aún a distancias grandes.
#En humedad esta diferencia se ve aún más entre Este y Oeste, que se refleja por una zona cuyana y cordillerana muy seca en comparación al litoral.
ggplot(v2.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional Temperatura"), 
       x = "distancia", y = "semivariance", colour = "dirección") + geom_line(size=1.5)+
  scale_color_manual(values=wes_palette("Cavalcanti1"))+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.text.y = element_text(size = 11, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 14, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))


# KRIGGING

proj4string(t_var) <- proj4string(dep_grilla)

dar_krg <- krige(TEMP.med ~ 1,t_var,grilla, model = dat_fit_tem, nmax = 5)
summary(dar_krg$var1.pred)

r <- raster(dar_krg, layer = "var1.pred" )
r.m <- mask(r, dep_grilla)
plot(r.m)

tm_shape(r.m) +
  tm_raster(n = 10, palette = wes_palette("Moonrise3")) +
  tm_shape(d_var) + tm_dots(size = .1) +
  tm_legend(legend.outside = TRUE)
