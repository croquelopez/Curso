################# Práctico PLN 1ª Parte #######################################

#Como en todo ejercicio de análisis de texto a través de lenguaje natural, vamos a explorar el df con el 
#objetivo puesto en entender de qué trata este corpus de texto. Por eso, en principio, no damos nunguna pista de 
#qué contenidos tiene este corpus y partimos de la base de que hay que descubrirlo.

#Cargamos el df

df<- read_csv("practicopln.csv")
my_stopwords <- read_csv("my_stopwords.csv")

#1)Superficialmente, ¿De qué trata este corpus? Utilizar funciones exploratorias

str(df)
glimpse(df)
head(df)
tail(df)
summary(df)
skim(df)

#Conjunto de tweets con 90 columnas y 242564 filas. Tipos de datos: caracteres, lógicos, numéricos y hay uno que no conocía: POSIXct, que por lo visto se refiere a la fecha/hora de creación de los tweets. Buscando información al respecto encontré lo siguiente: "R dispone en su paquete base de dos clases específicamente diseñadas para tratar con datos de tipo fecha/hora: Date (solo para fechas) y POSIXt (además de la fecha incluye hora y huso horario); esta última clase contiene dos subclases, POSIXct y POSIXlt que se diferencian simplemente en la forma en que almacenan internamente la fecha y la hora. El paquete lubridate dispone de diversas funciones que facilitas la extracción de componentes de un objeto fecha/hora de clase POSIXct" (fuente: http://estadistica-dma.ulpgc.es/cursoR4ULPGC/6h-Fechas.html)

dfpalabras <- df %>% unnest_tokens(word, text) %>% 
  anti_join(my_stopwords) %>%
  count(word, sort = TRUE)
#Palabras m?s frecuentes en el corpus: feminista, feminismo, feministas, t.co, https, mujeres, maradona, mujer, muerte, mierda. Como aparecen expresiones no lexemáticas (t.co, https), busqué una forma de eliminarlas.
nosirve <-  data.frame(word= c("t.co", "https"))
my_stopwords <- bind_rows(my_stopwords, nosirve)

head(df, 3)
tail(df, 3)
#Período de tiempo de los tweets: entre el 23/09/20 al 04/02/2021 (la muerte de Maradona fue el 25/11, lo que explica que aparezca entre las palabras más mencionadas)


#2) ¿En qué otros idiomas fueron escritos estos tweets? Me quedo con aquellos tweets que solo han sido escritos en español
freq(df$lang)
#94,62% parece ser español; 4,56% portugués; el resto no llega al 1%.
unique(df$lang)
#[1] "es"  "pt"  "und" "en"  "it"  "tl"  "ca"  "in"  "et"  "eu"  "da"  "pl"  "lt" [14] "fr"  "ro"  "tr"  "ht"  "no"  "fi"  "cy"  "sv"  "nl"
#Algunas abreviaturas pueden inferirse: español, portugués, inglés, francés, italiano.

glimpse(df)
df1 <- df%>%
  filter(lang == "es")
#Quedan 229526 de 242564 filas.

#3)¿Cuáles son las 50 palabras más frecuentes para estos tweets?
dfpalabras <- df1 %>% unnest_tokens(word, text) %>% 
  anti_join(my_stopwords) %>%
  count(word, sort = TRUE)
palabrasmasfrecuentes <- dfpalabras %>% top_n(50)
#feminista, feministas, feminismo, mujeres, maradona, mujer, muerte, mierda, re, aborto, festejar, causa, pelotuda, ideologia, reforzar, hombres, derechos, movimiento, lucha, sos, vos, a?os, militancia, género, violencia, país, hora, chile, diego, silencio, frente, hablar, hombre, ley, vida, varones, gente, favor, bueno, quiero, violadores, acá, femicidios, motuda, estado, social, popular, partido, problema, izquierda. Entre las palabras hay algunos argentinismos frecuentes y derivados del voseo: re, sos, vos, acá, que quizá sean menos significativos como palabras coloquiales de uso común. 



#4)  Ahora, podemos hacernos una idea de lo que tratan estos tweets, pero podríamos ir un poco más allá con otras herramientas. También podríamos explorar n-grams. Aquí podríamos explorar el número de n-grams que quisiéramos, pero lo recomendable en PLN es explorar hasta 3-grams y no más ya que suele ser infructuoso. 
#Veamos entonces ¿cuáles son los 10 bigramas y los 10 trigramas más comunes? Recordemos que aquí la estrategia para limpiar palabras vacías no es anti_join

##BIGRAMAS

bigramas <- df1  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) 

bigramas <- bigramas %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Para limpiar palabras vacías (no se aplica antijoin porque las columnas no tienen el mismo nombre):
bigramas <- bigramas %>%
  filter(!word1 %in% my_stopwords$word) %>%
  filter(!word2 %in% my_stopwords$word)


bigramas <- bigramas %>%
  unite(bigram, word1, word2, sep = " ")
bigramas %>% top_n(10)
#Top 10 bigramas:
#1 re mierda            11510
#2 movimiento feminista  4030
#3 aborto legal          3400
#4 sos vos               2603
#5 amiga feminista       2477
#6 media hora            2392
#7 problema sos          2388
#8 mujeres empezaron     2387
#9 bikini media          2386
#10 cuerpos cualquiera    2386
#11 feminismo pensalo     2386
#12 hablar amigo          2386
#13 pensalo rey           2386

##TRIGRAMAS
trigramas <- df1  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) 
 
trigramas <- trigramas %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigramas <- trigramas %>%
  filter(!word1 %in% my_stopwords$word,
         !word2 %in% my_stopwords$word,
         !word3 %in% my_stopwords$word)

trigramas <- trigramas %>%
  unite(trigram, word1, word2, word3, sep = " ")
trigramas %>% top_n(10)
#Top 10 trigramas:
#1 problema sos vos               2388
#2 bikini media hora              2386
#3 feminismo pensalo rey          2386
#4 prometieron aborto legal       1786
#5 soltaron viejos violadores     1783
#6 feministas abrazadas llorando  1672
#7 re mil mierda                  1309
#8 profe feministas lucharemos    1001
#9 profesora sandra pizarro       1001
#10 sandra pizarro v?ctima         1001

#5) ¿Qué nodos de significado existen y cuáles son son los núcleos más importantes? Lo graficamos a través de redes de n.gramas. Recordar que para graficar estas redes, los bigramas tienen que estar separados.
#Acá cuando corría el código para hacer las matrices/gráficos me daba este error: "Error: no se puede ubicar un vector de tamaño x". Lo estuve buscando y parece ser un problema de Windows y la cantidad de memoria RAM que le deja usar a R (mi compu es relativamente nueva, así que no creo que vaya por el lado del hardware la cosa). Por lo que vi en los foros otros SO como Linux no suelen tener ese error. Lo solucioné eliminando prácticamente todos los objetos del environment excepto el indispensable de bigramas, para liberar la RAM, y funcionó.

#MATRICES

bigramasmatriz <- bigramas %>% 
  filter(n > 1000) %>%
  graph_from_data_frame()
print(bigramasmatriz)

trigramasmatriz <- trigramas%>% 
  filter(n > 1000) %>%
  graph_from_data_frame()
print(trigramasmatriz)

#GRÁFICOS

plot1 <- ggraph(bigramasmatriz, layout= "fr") +
  geom_edge_link(color= "steelblue") +
  geom_node_point(color="red") +
  geom_node_text(aes(label = name), color= "darkblue", vjust = 1, hjust = 1)
print(plot1)

plot2 <- ggraph(trigramasmatriz, layout= "fr") +
  geom_edge_link(color= "steelblue") +
  geom_node_point(color="red") +
  geom_node_text(aes(label = name), color= "darkblue", vjust = 1, hjust = 1)



