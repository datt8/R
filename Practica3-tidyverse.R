library(tidyverse) #Cargamos librería
library(nycflights13) #Cargamos librería 
data(flights) #Descargamos la base de datos
#A lo largo de la práctica se ejecutarán los códigos head para la visualización de únicamente x registros

##EJERCICIO 1 FILTER

filter(flights, arr_delay>=120)->retrasos120   #Vuelos retrasados más de 2 horas (10200)
retrasos120
filter(flights, (dest=="IAH"|dest=="HOU"))->vuelos_hou #Vuelos a HOU o IAH (9313)
vuelos_hou
filter(flights,(carrier=="AA"|carrier=="DL"|carrier=="UA")) -> vuelos_Delta #Vuelos de esas 3 compañias (139504)
vuelos_Delta
filter(flights, month>=7&month<=9) -> vuelos_verano #Vuelos del verano (86326)
vuelos_verano
filter(flights, dep_delay<=0&arr_delay>120) -> vuelos_retraso #Vuelos retrasados (29)
vuelos_retraso
filter(flights, dep_delay>=60 & (dep_delay-arr_delay>30))-> vuelos_recup #Vuelos recuperados (1844)
vuelos_recup
filter(flights,hour>=0&hour<=6) -> vuelos_medianoche #Vuelos de madrugada (27905)
vuelos_medianoche

##EJERCICIO 1 ARRANGE (Más retraso y más adelanto)
retraso<-arrange(flights, desc(arr_delay)) #Ordena por retraso en llegada (llegada)
head(retraso,10) #Muestra los primeros 10 vuelos (que son los que más retraso han acumulado en llegada)

adelanto<-arrange(flights, dep_delay) #Ordena por el menos retraso en la salida (origen)
head(adelanto, 10) #Muestra los 10 vuelos más retrasados en salida

##EJERCICIO 2 ARRANGE (Más rápido)
rapidos<-arrange(flights, air_time)
head(rapidos,10) #Muestra los 10 vuelos más rápidos (en cuanto a tiempo en el aire)

##EJERCICIO 3 ARRANGE (Más largo y corto)
distancia<-arrange(flights, distance)
head(distancia, 10) #Muestra los 10 vuelos más cortos (en distancia)
tail(distancia, 10) #Muestra los 10 vuelos más largos (en distancia)

##EJERCICIO 1 SELECT (Seleccionar 4 variables)
select(flights, dep_time, dep_delay, arr_time, arr_delay)->tablaA
tablaA
select(flights, starts_with("dep"), starts_with("arr"))-> tablaB
tablaB
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")-> tablaC
tablaC
select(flights, matches("^(dep|arr)_(time|delay)$")) -> tablaD
tablaD

##EJERCICIO 2 SELECT
datos<-select(flights, air_time, arr_time, dep_time)
mutate(datos,
       dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
       arr_time_min = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
       air_time_diff = air_time - arr_time + dep_time) ->datos.nuevos
datos.nuevos

## 1. Seleccionar todas las variables que acaben en ‘time’ de la base de datos flights de nycflights13
select(flights, ends_with("time"))-> var_tiempo #Escojo únicamente las variables que terminan por tiempo
var_tiempo

##2. Renombrar la variable arr_delay con retraso_llegada de flights y almacenar en un nuevo objeto llamado Vuelos
vuelos<-rename(flights,retraso_llegada=arr_delay)
vuelos_s<-select(vuelos,year:day, carrier,flight,retraso_llegada) #Elijo sólo esas columnas
vuelos_s

##3. Ordenar Vuelos de menor a mayor distancia recorrida
distancia_mas<-arrange(vuelos,distance)
distancia_mas_s<-select(distancia_mas,year:day, carrier,flight,distance)
head(distancia_mas_s,10) #Muestra los 10 más cortos
tail(distancia_mas_s,10) #Muestra los 10 más largos

##4. Encontrar el origen y el destino del vuelo de mayor distancia recorrida y el de menor distancia recorrida
distancia_mas_origen<-select(distancia_mas,year:day, origin,dest,distance)
head(distancia_mas_origen,1) #Muestra el más corto
tail(distancia_mas_origen,1) #Muestra el más largo

#5. Añadir a la base de datos Vuelos la velocidad media del vuelo como una variable calculada a partir del espacio y el tiempo, como se explica en la presentación de clase
veloc<-mutate(vuelos,
                  velocidad=distance/(air_time/60))
veloc_s<-select(veloc, velocidad, everything()) #Colocamos velocidad como la primera variable
veloc_s

# Retraso por destino 
by_dest<-group_by(flights, dest)
delay<- summarize(by_dest,
                  count=n(),
                  dist=mean(distance, na.rm=TRUE),
                  delay=mean(arr_delay, na.rm=TRUE)   #Organiza por destinos y luego realiza el conteo de los destino y la distancia media de los viajes y los retrasos
)
delay<-filter(delay, count >20, dest !="HNL")   #Filtra quitando los que tienen un conteo menor a 20 y excluye Honolulu

delays<-flights%>%
  group_by(dest) %>%
  summarize(
    count=n(),
    dist=mean(distance, na.rm=TRUE),
    delay=mean(arr_delay, na.rm=TRUE)
  ) %>%
  filter(count >20, dest!="HNL") #Realiza lo mismo que el anterior código


str(delay)
str(delays)
summary(delay)
summary(delays)
delay
delays
#Observamos que ambas variables son iguales


no_cancel <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))   #Coge la base flights y excluye los NA

no_cancel %>%
  group_by(year,month,day,origin) %>%
  summarize(mean=mean(dep_delay)) -> retraso_dia    #Organiza por grupo en año, mes, día y origen y luego calcula la media según esos grupos
