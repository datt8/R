##PRÁCTICA 4

##EXPLORANDO LA DISTRIBUCIÓN DEL PRECIO
data(diamonds)
library(tidyverse)
ggplot(data=diamonds, mapping=aes(x=price))+
  geom_histogram(binwidth=75)
#Lo primero en hacer, será un histograma de la variable precios para ver la distribución del precio. Se decide escoger 100$ como amplitud del intervalo
#Al fijarse en la distribución, se observa que en el entorno de los 1500-2000$ existe un intervalo que no tiene conteo. Por lo tanto, se decide hacer zoom en este intervalo
#En cuanto a las líneas generales del gráfico se observa una clara asimetría positiva. De tal manera, gran parte de los diamantes tienen un precio inferior a los, aproximadamente, 1000-1200$
atipicos<- diamonds %>%
  filter(between(price,1300,2000))
ggplot(data=atipicos, mapping=aes(x=price))+
  geom_histogram(binwidth=25)
#Al ver este histograma, queda bastante claro que en el intervalo (1450-1550) no existen diamantes. La razón es totalmente desconocida 

##DIAMANTES DE 0.99 QUILATES vs. DIAMANTES DE 1 QUILATE

quilates0.99<-diamonds %>%
  filter(carat==0.99)
nrow(quilates0.99)
#Con este comando, se averiguan los diamantes que tienen, exactamente, 0.99 quilates. El resultado es un data frame de 23 observaciones
quilates1<-diamonds %>%
  filter(carat==1)
nrow(quilates1)
#Realizando el mismo proceso que antes, se puede observar que existen 1558 diamantes que tienen 1 quilate

#Estos resultados no pueden ser por casualidad, debe haber una razón detrás para semejante diferencia. Una respuesta puede ser que el responsable de la venta de estos, adultere de una forma ínfima el pesaje de estos, en aras de buscar un mayor beneficio

#LA VARIABLE MÁS IMPORTANTE

#Para hallar la variable más importante del dataset, se ha decidido seguir el criterio de la que mayor correlación tenga con el atributo en cuestión, el precio.
#Para hacer un estudio de la correlación se deben omitir las variables cualitativas del dataset. Para posteriormente, realizar las correlaciones
diamondsn <- diamonds[c(1,5:10)]
cor(diamondsn)
#En esta matriz de correlación se observa una gran correlación directa y positiva entre el precio con los quilates y las dimensiones del diamante. Por ello, se decide agrupar las variables x,y,z en un nueva variable llamada volumen.
diamondsn$volumen<-diamondsn$x*diamondsn$y*diamondsn$z
#Para realizar nuevamente la matriz de correlacion, se vuelve a crear un data frame nuevo que contenga la nueva variable y no las tres dimensiones
diamondsv<-diamondsn[c(1:4,8)]
library(corrplot)
corrplot(cor(diamondsv),method="number", title="Matriz correlaciones",type="upper")
#En el gráfico de la matriz de correlaciones se observa claramente que la mayor correlación del precio es con la cantidad de quilates. Aunque seguida muy de cerca, por la nueva variable volumen.

#Tras observar el data frame, se descubren algunos ejemplares que no tienen alguna dimensión, provocando que su volumen sea 0; u otro ejemplar que posee un volumen gigantesco. Por lo tanto, se decide excluir del gráfico estos registros para que no distorsionen
diamondsc <- diamondsv %>%
  mutate(volumen=ifelse(volumen==0|volumen>900, NA, volumen))%>%
  na.omit(diamondsv)

ggplot(diamondsc, aes(x=volumen, y=price))+
  geom_point()

#En el gráfico se observa una clara correlación entre el precio y el volumen, y como el precio crece exponencialmente a medida que el volumen aumenta.
#Sin embargo, esto tiene un límite y se ve como ante volúmenes más grandes existe una menor correlación, ya que se ven los puntos más dispersos

ggplot(diamondsc, aes(y=price, x=carat))+
  geom_point()

#En este gráfico ocurre un suceso similar al del anterior. Existe una correlación directa y positiva de caracter exponencial entre el precio y los quilates del diamente.
#Aún así, tambien se ve como ante diamantes de más quilates hay una mayor dispersión. 
#Además se reafirma el hecho estudiado en los diamantes de 0.99 y 1 quilate. Se intuye como en valores exactos de los quilates (1, 1.5, 2, 2.5, ....) hay muchos más registros que en valores más "inexactos"

##RELACION CORTE-PRECIO

#Para estudiar esta relación no se puede realizar un análisis de correlación, como el usado anteriormente, al tratarse de una variable cualitativa (cut) y una variable numérica (precio).
ggplot(diamonds, aes(x=cut, y=carat))+
  geom_boxplot()
#Se realiza el diagrama de cajas y bigotes porque es la forma más simple y visual de mostrar la relación entre las dos variables
#Se observa como a pesar de que el corte Fair debería ser el de una "calidad" menor es el que tiene unos cuartiles superiores a cualquiera de los demás. Esto no deja de ser curioso
#Por lo demás, todos los cortes tienen una representación y rango intercuartilico similar. Aún así, el corte Fair sigue destacando por unos datos anómalos superiores al resto.

##¿POR QUÉ ESTAS DOS CARACTERISTICAS LLEVAN A UN DIAMANTE DE MENOR CALIDAD?
#Normalmente, un diamante más grande suele ser mejor vendido y será más sencillo darle cualquier tipo de corte. Por otro lado, un diamante pequeño será menos "vendible" y deberá tener un corte mucho más preciso

