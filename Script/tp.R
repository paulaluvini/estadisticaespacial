library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library('Hmisc')
library(spdep)
library(sp)

# Primero leemos los archivos a utilizar
departamentos <- st_read("Datos/Codgeo_Pais_x_dpto_con_datos/pxdptodatosok.shp")
smn <- read.csv(file = 'Datos/estaciones_smn.csv', sep = ";")
temperaturas <- read.csv(file = 'Datos/temperaturas.csv')

#Primero miro un poco el de departamentos que utilizamos en clase.
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
smn <- smn %>% rename(Estacion =  ï..NOMBRE) 
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

ggplot() + geom_sf(data = departamentos) + geom_point(data = smn, aes(x=x, y=y),
                                                      colour = "dodgerblue1", size = 1)+ theme_classic()

departamentos_ba <- departamentos %>% filter(provincia == "Buenos Aires")
smn_ba <- smn %>% filter(Provincia == "BUENOS AIRES")

ggplot() + geom_sf(data = departamentos_ba) + 
  geom_point(data = smn_ba, aes(x=x, y=y),colour = "dodgerblue1", size = 1)+ 
  theme_classic() +geom_text(data= smn_ba, aes(x,y, label = Estacion), size = 2, fontface = "bold",vjust = -0.5)+
  ggtitle("Estaciones Meteorológicas", subtitle = "Provincia de Buenos Aires") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())

 # theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                       # size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#Los de los partidos del Conurbano + CABA no se leen muy bien así que hago un mapa sólo con ellos.
departamentos_amba <- departamentos %>% filter(provincia == "Buenos Aires" | provincia =="Ciudad Autónoma de Buenos Aires")
smn_amba <- smn %>% filter(Provincia == "BUENOS AIRES" | Provincia == "CAPITAL FEDERAL")

bbox_new <- st_bbox(departamentos_ba)
bbox_new
bbox_new[1] <- -59
bbox_new[2] <- -35
bbox_new[3] <- -57.8
bbox_new[4] <- -34.4
bbox_new  
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()

ggplot() + geom_sf(data = departamentos_amba) + 
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1],ylim = st_coordinates(bbox_new)[c(2,3),2])+ 
  geom_point(data = smn_amba, aes(x=x, y=y),colour = "dodgerblue1", size = 2)+ theme_classic() +
  geom_text(data= smn_amba, aes(x,y, label = Estacion), size = 3, fontface = "bold", vjust = -0.5)+
  ggtitle("Estaciones Meteorológicas", subtitle = "Area Metropolitana") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())
grafico2
  #theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed",size = 0.5),
    #    panel.background = element_rect(fill = "aliceblue"))

#Miro el dataset de temperaturas
#ACA ME FALTA MIRAR ESTO BIEN. TIENE NA, FALTA ANALISIS EXPLORATORIO, BOXPLOT DE 
#TEMPERATURAS, HISTOGRAMA, ETC.

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

library(geoR)
library(spdep)
#Lo mergeo con el otro dataset
smn_temp <- merge(x = temperaturas_2020, y = smn, by = "Estacion", all.x = TRUE)
#agrego este dataset general para que no se rompa el graph de temperaturas
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
  select(x,y,TMAX.med ) 

#temp_max <- temp_max_group[,c(16,17,3)]


smn_temp_gral <- smn_temp_gral %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs= 4326)
#b <- temp_max_gral %>% filter(!is.na(x) & !is.na(y)) %>% st_as_sf(coords =c("x", "y"), crs=4326)
temp_max <- smn_temp_gral[,c(16,3)]
#temp_min <- temp_max_group[,c(16,17,4)]
temp_min <- smn_temp_gral[,c(16,4)]

#hago un analisis de la temperatura 
ggplot() + geom_histogram(data=temp_max, aes(x= TMAX)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank()) +
      ggtitle("Histograma de temperatura mediana") + labs(x= "temperatura máxima")
qqnorm(temp_max$TMAX)

ggplot() + geom_sf(data= temp_max, aes(x=geometry, fill=TMAX))

b <- temp_max

plot(b)
class(b)

#Vemos que tenemos más observaciones con temperaturas maximas entre 20 y 30 grados centigrados
# Hay una clara tendencia mirando la latitud, lo que hace sentido en Argentina por tener 
# mayor variación climática de norte a sur que de este a oeste.
temp_max_geodata <- as.geodata(temp_max_group)
class(temp_max_geodata)
plot(temp_max_geodata)

# AUTOCORRELACION ESPACIAL

#PUNTOS
pares <- temp_max_group %>% select(x, y) %>% filter(!is.na(x), !is.na(y))
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

# VARIOGRAMA 
library(gstat)
d_var <- temp_max_group %>% select(TMAX.med, x,y)
coordinates(d_var) <-~y+x
d_var <- d_var[1:50,]
variograma <-variogram(TMAX.med ~1, d_var)
plot(variograma, main = "Variograma empírico de TMAX")
variograma$dir.ver

#Uno en ggplot ?)
library(wesanderson)
loadfonts(device = "win")
windowsFonts()
ggplot(variograma, aes(x = dist, y = gamma)) +
  geom_point(colour = wes_palette("Zissou1")[1]) + ylim(0, 55) +
  labs(title = expression("Variograma empírico"), 
       x = "distancia", y = "semivarianza")+  
  theme_classic()+theme(text = element_text(family = "Arial"),
                        axis.title.x =   element_blank(),
                        axis.title.y = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 14, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 18, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))

#vemos toda la nube
variograma_nube <- variogram(TMAX.med~1, d_var, cloud =TRUE)
plot(variograma_nube, main= "Variograma nube de TMAX")

dat_fit <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, vgm(psill=40, model ="Lin", nugget =0,range=20))
plot(variograma, dat_fit)

print("La meseta o sill es la suma de :")
dat_fit[1,"psill"]
dat_fit[2,"psill"]
dat_fit[1,"psill"]+dat_fit[2,"psill"]

dat_fit_s <- fit.variogram(variograma, fit.ranges=FALSE, fit.sills=FALSE, vgm(psill=40, model ="Sph", nugget =0,range=20))
plot(variograma, dat_fit_s)
#A diferencia del exponencial, acáa alcanzo la meseta
# Comparamos los modelos: 
# Calculamos la Suma de cuadrados del error  para cada uno de los modelos ajustados
#El esferico tiene menos error, nos quedamos con esto
attr(dat_fit, 'SSErr')
attr(dat_fit_s, 'SSErr')

# Son isotrópicos? Voy a averiguarlo
v1 <- variogram(TMAX.med ~1, d_var,cutoff=50, width=5,  map = T)
# Que sea todo del mismo color me dice que el proceso es isotrópico porque solo mira la magnitud,
# no si es algo de norte a sur, etc Porque el mapa me dice que me muevo de norte a sur y no hay cambios debido a eso.
plot(v1)

v.dir <-variogram(TMAX.med ~1, d_var,alpha = (0:3) * 45)
help(variogram)
v.anis <- vgm(psill = 40, "Lin", 20, anis = c(45, 0.3),nugget=0)
plot(v.dir, v.anis)

v.dir$dir.hor

#Que significa que la direccion es siempre E-O? Que es isotropico o no?
v.dir$direction <- factor(v.dir$dir.hor, levels = c(0, 45, 90, 135),
                          labels = c("E-O", "NE-SO", "N-S", "NO-SE"))
v.dir
ggplot(v.dir, aes(x = dist, y = gamma, colour = direction)) + 
  geom_point(size = 1.8) + 
  labs(title = expression("Variograma direccional"), 
       x = "distancia", y = "semivariance", colour = "dirección") + geom_line()+
  theme_classic()+theme(text = element_text(family = "Arial"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.text.x = element_text(size = 14, family = "Arial", face = 'bold'), 
                        axis.line = element_line(colour = "black"),plot.title = element_text(size = 18, face = "bold"),
                        axis.title = element_text(size = 11, face = "bold"))



#################################################################################
#CODIGO SUELTOS DE GEOSTATSGUY

par(mfrow=c(2,2))                              
plot(variogram(TMAX.med~1, d_var, cutoff=50, width=5, map=TRUE),main = "Semivariogram Map",max=1.0)
plot(variogram(TMAX.med~1, d_var, cutoff=50, width=5, map=TRUE),main = "Number of Points",np=TRUE)

var_035 = variogram(TMAX.med~1, d_var,cutoff = 50,width =5,alpha = 45.0,tol.hor=22.5)         # Calculate default isotropic variogram of N[Porosity]
var_125 = variogram(TMAX.med~1, d_var,cutoff =50,width =5,alpha = 125.0,tol.hor=22.5) 

plot(var_035,main="Porosity Anisotropic 035 Variogram")
plot(var_125,main="Porosity Anisotropic 125 Variogram")

# Anisotropy is parameterized as c(azimuth,dip,plunge,hratio,vratio) in #3D and c(azimuth,hratio) in 2D.
por.vm.ani <- vgm(psill = 40, "Lin", 20, anis = c(0.35, 1),nugget=0)
por.vm.ani                                     # check the variogram model parameters 
dat_fit

plot(var_035,por.vm.ani,main="Porosity Anisotropic 035 Variogram") # use the built in gstat variogram plot
plot(var_125,por.vm.ani,main="Porosity Anisotropic 125 Variogram") # use the built in gstat variogram plot

# Use auto-fitting to try to improve the variogram model, then check model and plot
por.vm.ani.auto <- fit.variogram(variograma,por.vm.ani,fit.sills = FALSE)
por.vm.ani.auto                                # check the autofit parameters and compare to our fit
plot(var_035,por.vm.ani.auto,main="Porosity Anisotropic 035 Variogram")
plot(var_125,por.vm.ani.auto,main="Porosity Anisotropic 125 Variogram")


#¿Cómo elegimos el tamaño de los bins Bm? ¿Cuántos bins?
