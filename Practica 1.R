
## INICIO DE PRACTICA GUIADA ---------------------------------------------------

## En el siguiente script vamos a iniciar un analisis exploratorio y de visualizacion de un dataset que ya conocemos, delitos! La idea es poner a prueba todo lo que vimos en las clases y desafiarnos con preguntas que requieren una busqueda particular. Esta pensado para que exploremos libremente, intenten ser creativos y ambiciosos a la hora del analisis y sumen cualquier conocimiento adicional que puedan compartir con el resto. 

## Introduccion ----------------------------------------------------------------

## 1. Vamos a iniciar creando un proyecto nuevo en una carpeta para alojar todos los documentos de este trabajo. Recuerden poner un nombre acorde y setear el espacio de trabajo. 

## 2. Cargen todas las librerias y datasets a utilizar. Instalen las librerias que no tengan descargadas y luego importenlas en su espacio de trabajo.      

library(skimr)
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(lubridate)
library(summarytools)
library(dplyr)
library(ggsci)
library(ggplot2)

delitos_2019 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv")

delitos_2018 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2018.csv")

delitos_2017 <- read_csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2017.csv")

## 3. Como pueden ver importamos las librerias desde una pagina web. Estas paginas suelen ser una gran fuente de datos cuando queremos inciar un analisis. Pueden revisarlo en el siguiente link: https://mapa.seguridadciudad.gob.ar/ 

## Limpieza de los datos -------------------------------------------------------

## 4. Unifica los tres datasets en uno solo de tal modo que quede uno debajo del otro. En el caso de que aparezca un error, que sucede con la variable franja horaria? Transformala y luego uni los datasets. 

dim(delitos_2017)
delitos <- bind_rows(delitos_2019,
                     delitos_2018,
                     delitos_2017 %>% mutate(franja_horaria = as.double(franja_horaria)))



## 5.Cuantas filas tiene cada dataset? Y cuantas columnas? 

names(delitos_2017) ##120564 filas y 10 columnas
names(delitos_2018) ##123733 filas y 10 columnas
names(delitos_2019) ##117661 filas y 10 columnas

##O también:
dim(delitos_2017)
##> dim(delitos_2017)
##[1] 120564     10

dim(delitos_2018)
##> dim(delitos_2018)
##[1] 123733     10

dim(delitos_2019)
##> dim(delitos_2019)
##[1] 117661     10



## 6. Que tipo de datos contiene el dataset? 

class(delitos$id)
class(delitos$fecha)
class(delitos$franja_horaria)
class(delitos$tipo_delito)
class(delitos$subtipo_delito)
class(delitos$cantidad_registrada)
class(delitos$comuna)
class(delitos$barrio)
##> class(delitos$id)
##[1] "numeric"
##> class(delitos$fecha)
##[1] "Date"
##> class(delitos$franja_horaria)
##[1] "numeric"
##> class(delitos$tipo_delito)
##[1] "character"
##> class(delitos$subtipo_delito)
##[1] "character"
##> class(delitos$cantidad_registrada)
##[1] "numeric"
##> class(delitos$comuna)
##[1] "numeric"
##> class(delitos$barrio)
##[1] "character"
##Tipos de datos: numéricos y caracteres, pero también parece indicar la fecha como un tipo de dato independiente.

skim(delitos) #En este me salió otro tipo aparte de Date, numeric y character: factor. 


## 7. Cuantos valores faltantes se registran en cada variable? 

summary(is.na(delitos))

##Al ser un operador lógico, entiendo que los resultados listados como TRUE son los valores ausentes: 
#id: 0 fecha:0 franja_horaria: 43 tipo_delito: 0 subtipo_delito: 311570 
#cantidad_registrada: 0 comuna:7738 barrio: 7738 lat: 7738 long: 7738


## 8. Que sucede con la variable cantidad registrada? Explora los valores unicos, cuales son los valores mas frecuentes y saca conclusiones al respecto. Puede que tengas que buscar sobre tablas de frecuencia.


freq(delitos$cantidad_registrada) #(expresadas en porcentajes)
unique(delitos$cantidad_registrada)
##Conclusión: casi el total de los delitos ocurrieron una Ãºnica vez.


## 9. Cual es la relacion entre tipo de delito y subtipo de delito? Describir. Puede que tengas que buscar sobre tablas de contingencia

tabladelitos <-delitos %>% 
  select(tipo_delito, subtipo_delito) %>% 
  table()

#En la tabla resultante se observa que los únicos tipos de delitos que tienen especificación en subtipo son Homicidio (especificado como doloso y siniestro vial), Lesiones (siniestro vial), Hurto sin violencia y Robo con violencia (automotor).



## 10. Hace el grafico pertinente para mostrar los tipos de delitos existentes y sus frecuencias. No olvides incluir titulo, nombres a los ejes y colores.  
#Acá estuve probando varias cosas (no siempre con éxito) para dar alguna correlación estética entre los tipos de delitos y la frecuencia. Conseguí matizar los colores según el porcentaje. Después probé cambiar el orden de los elementos del eje x según su cantidad en el eje y. Al final me salió, haciendo esto primero antes de la tabla:
delitos$tipo_delito = factor(delitos$tipo_delito, levels=c("Homicidio","Lesiones","Hurto (sin violencia)",
                                                           "Robo (con violencia)"))
levels(delitos$tipo_delito)

#La tabla:
tab1 <- delitos %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = round(n/sum(n) *100, 2))
tab1 <- tab1%>% 
  rename(Porcentaje = perc) #Hice esto para que no quedara un seco "perc" en la Leyenda del gráfico

#El gráfico:
plot1 <- ggplot(data = tab1, aes(x = tipo_delito, y= Porcentaje, fill = Porcentaje))+ #fill=Porcentaje es lo que coordina el color
  geom_col(stat= "identity", width = 0.5)+
  geom_text(aes(label= Porcentaje), vjust=-1.6, color="darkblue", size=3.5)+
  theme_update()+
  labs(title = "Frecuencias de delitos 2017-2019", x = "Tipo de delito", caption = "Datos del Gobierno de Buenos Aires")+
  ylim(c(0, 60))
#No me salía en "plots" automáticamente así que agregué esto:
print(plot1)



## 11. Hace el grafico pertinente para mostrar como se distribuye la variable franja horaria. No olvides incluir titulo, nombres a los ejes y colores.  


#Usé un geom_density:
plot2 <- ggplot(data = delitos) +
  geom_density(mapping = aes(x=franja_horaria), color= "steelblue", fill = "blue",  alpha=.10)+
  theme_update()+
  labs(title="Frecuencia de Delitos según Franja Horaria",
       y="Cantidad de delitos",
       x="Franja Horaria (00 a 23 hs)",
       caption = "Datos del gobierno de Buenos Aires")
print(plot2)

## 12. Incorporaremos al grafico anterior una segmentacion por tipo de delito y un filtro para quedarnos con los delitos que hayan ocurrido especialmente en Puerto Madero. 


plot3 <- delitos %>% 
  filter(barrio == 'Puerto Madero') %>% 
  ggplot(mapping = aes(x = franja_horaria, color = "red")) + 
  geom_density(aes(y =..density..), bins = 30, position = 'identity', adjust=1.5, alpha=.6, show.legend = F)+
  scale_y_continuous(labels = scales::percent)+
  theme_update()+
  facet_wrap(~ tipo_delito, nrow = 2)+
  labs(title = "Puerto Madero: tipo de delito y franja horaria",
       x = "Franja Horaria", y = "", 
       caption = "Datos del Gobierno de Buenos Aires")
print(plot3)




























