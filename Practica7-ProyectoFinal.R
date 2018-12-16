#EJERCICIO 2
library(tidyverse)
library(ggplot2)

##EJERCICIO 3

m<-function(dni){
  (dni%%4)->d
  return (d)
} #Se crea la función que nos dice el resto del DNI
m(12345678) #El módulo es 2, por lo tanto se trabaja con la base C

##EJERCICIO 4

read.csv("Datos/datos_XXC.csv", encoding = "UTF-8") -> datos #Se carga el dataset correspondiente

##EJERCICIO 5

str(datos) #Estructura de los datos
summary(datos) #Descripción numérica de los principales estadísticos
nrow(datos) #Número de registros (filas)
ncol(datos) #Número de variables (columnas)

##EJERCICIO 6
elimina<-c(0) #Se crea un vector donde guardar los resultados
datos_1<-datos
contar<-function(x){
  sum(is.na(x))
} #Se crea una función que cuenta los N/A

for (i in 1:length(datos)){
  nas<-contar(datos[,i])
  porcentaje<-nas/nrow(datos)
  if (porcentaje>=0.8) elimina[i]=1
  else elimina [i]=0
}#Se recorre el dataset original y se comprueba las columnas en las que el número de N/A es >80%
elimina
datos_1 <- datos_1[,elimina==0] #Se quitan de datos_1 las columnas con más del 80% de N/A

write.csv(datos_1, "datos_1.csv") #Se guarda el dataset como fichero .csv

##EJERCICIO 7

datos_2<-datos[,3:13] #Se escogen las variables desde fecha hasta lentitud

##EJERCICIO 8 (sólo las que tienen tilde)
datos_2<-rename(datos_2, mood=ánimo)
datos_2<-rename(datos_2, concentration=concentración)
datos_2<-rename(datos_2, motivation=motivación)

##EJERCICIO 9

summary(datos_2) #Vemos la estructura de los datos para la "mejor" elección
ggplot(data=datos_2, aes(y=despertar))+
  geom_boxplot()
ggsave("despertar.png")
#Se observa una distribución bastante simétrica con un atípicos por cada lado. La mediana está ligeramente más cercana al tercer cuartil que al primero
ggplot(data=datos_2, aes(y=motivation))+
  geom_boxplot()
ggsave("motivacion.png")
#Se observa un boxplot donde la mediana y el tercer cuartil se encuentran en el mismo lugar, habiendo un atípico en la parte superior
ggplot(data=datos_2, aes(y=lentitud))+
  geom_boxplot()
ggsave("lentitud.png")
#Se observa una distribución completamente asimétrica (existen muchos atípicos en la parte superior), a la vez que la mediana se sitúa mucho más cerca del primer cuartil que del tercero
ggplot(data=datos_2, aes(x=lentitud))+
  geom_histogram()
ggsave("hist_lentitud.png")
#En este histograma, que refleja la lentitud de los distintos registros, se observa la asimetría antes mencionada y como la mayoría de la muestra está entre el 60-70
ggplot(data=datos_2, aes(x=motivation))+
  geom_histogram(binwidth = 1)
ggsave("hist_motivacion.png")
#En el histograma se puede observar claramente lo antes mencionado. Casi toda la muestra valora esta variable como -2 o -1 provocando que la mediana y el tercer cuartil sean el mismo (-1)

##EJERCICIO 10
datos_2$despertar+24->datos_2$despertar
pasar_min<-function(columna) {
  horas<-floor(columna)*60 #Transformo las horas en minutos
  minutos<-round(columna, 2) - floor(columna) #Extraigo los minutos correspondientes
  mins<-horas + minutos #Sumo ambos para tener en la misma unidad (minutos)
  return(mins)
}
datos_2<- mutate(datos_2, sleep_time=pasar_min(datos_2$despertar)-pasar_min(datos_2$dormir))
#Se crea una nueva columna con la función creada que permite evaluar las horas de sueño
datos_3<-filter(datos_2,datos_2$sleep_time<900)
#Para quitar datos anómalos se deciden eliminar todos los registros de aquellos que hayan dormido más de 900 minutos (15 horas)

##EJERCICIO 11
matriz.corr<-cor(datos_3) #La variable una mayor correlacion con sleep time será la que se decida graficar
matriz.corr
#Se decide graficar la variables sueño, porque es la que mayor relacion tiene exceptuando dormir y despertar, ya que sleep time es combinacion lineal de ambas
plot(datos_3$sleep_time, datos_3$calidad_sueño, main="Correlación tiempo-calidad de sueño",
     xlab = "Tiempo de sueño (min)",ylab="Calidad sueño",type = "p") 
#Para su guardado se hace de forma manual mediante el boton export -> save as image
ggplot(datos_3, aes(x=datos_3$sleep_time, datos_3$calidad_sueño))+
  geom_point()+
  xlab("Tiempo de sueño (min)")+
  ylab("Calidad de sueño")+
  ggtitle("Correlación tiempo-calidad sueño")
ggsave("Correlación.png")
                          
  
