library(tidyverse)

#Cargamos los df
travel_details <- read.csv("data/traveltrip/travel_details.csv")

#Leer un csv en un zip
temp <- tempfile()
ArchivoCsv = unz("data/airbnb.zip", "Airbnb_listings.csv")
airbnb <- read.csv(file=ArchivoCsv, header=TRUE, sep=",")

unlink(temp)
remove(ArchivoCsv)
remove(temp)
  

#observamos su estructra
str(travel_details)
str(airbnb)

#Vemos que tipo es
class(airbnb)
class(travel_details)

#observamos una descripcion resumen de los datos de cada variable
summary(travel_details)
summary(airbnb)

#Observamos los nombres de cada variable
colnames(travel_details)

#numero de filas y columnas
nrow(travel_details)
ncol(travel_details)

# Observamos los distintos destinos de viaje
levels(travel_details$Destination)
travel_details$Destination <- factor(travel_details$Destination)

# Aquí otra opción es usar la función distinct() que devuelve los valores distintos de cada variable
travel_details %>% 
  distinct(Destination)
  

#si queremos contabilizar la cantidad de viajes x destino
travel_details %>% 
  count(Destination , sort = T) 


#Los generos de los viajeros
travel_details %>% 
  count(Traveler.gender , sort = T)

          
## Seleccionamos las columnas que nos interesa analizar con select()

info_viajeros <- travel_details %>%
  select(c("Traveler.name", "Traveler.age", "Traveler.gender", "Traveler.nationality"))

### y si quiero eliminar variables que no me interesa analizar? select(-)
travel_sin_viajeros <- travel_details %>% 
  select(-c("Traveler.name", "Traveler.age", "Traveler.gender", "Traveler.nationality"))
  
travel_sin_viajeros <- travel_details %>% 
  select(-c("Traveler.name", "Traveler.age", "Traveler.gender", "Traveler.nationality"))

## Me quedo solo con los viajeros Japoneses

japoneses <- info_viajeros %>% 
  filter(Traveler.nationality == "Japanese")
### si lo comparo con el resultado de contar nacionalidades observo que el resultado es el correcto
info_viajeros %>% 
  count(Traveler.nationality , sort = T)

# podemos combinar operadores lógicos para obtener la seleccion por ej queremos viajeros de mas de 40 años mujeres

info_viajeros %>%
  filter(Traveler.gender == "Female" & Traveler.age >= 40)
#Ya que seleccionamos la info de los viajeros en un df aparte no es necesario que el nombre de la variable remita a los viajeros

info_viajeros <- info_viajeros %>% 
  rename(c("Name" = "Traveler.name",
           "Age" = "Traveler.age",
           "Gender" = "Traveler.gender",
           "Nacionality" = "Traveler.nationality"))

# Vamos a ordenar los datos en funcion de la edad de los viajeros
info_viajeros_ordenada <- info_viajeros %>% 
  arrange(Age)
# ¿y si quisieramos en orden descendente? agregamos desc()
info_viajeros_ordenada <- info_viajeros %>% 
  arrange(desc(Age))

## valores na en la columna?
is.na(info_viajeros$Gender)

# Una función muy util es group_by la cual nos permite agrupar datos en funcion de una o mas variables
#la funcion summarise() nos permite obtener resumenes de datos, combinar ambas es muy interesante y util
edadmedia_pais_gro <- info_viajeros %>% 
  group_by(Gender, Nacionality) %>% 
  summarise(media_age = mean(Age),
            count = n()) 
  

# otras funciones utiles son 
"""
* `mean()`: Obtiene el promedio de los valores
* `sum()`: Obtiene la suma
* `min()`: Obtiene el valor más bajo
* `max()`: Obtiene el valor más alto
   n()
"""

#agregar columnas con mutate() podemos agregar columnas con resultados de operaciones matemáticas, con valores que querramos, o con distintas transformaciones.
#algo muy util es agregar columnas id utilizando la funcion row_number()
info_viajeros <- info_viajeros %>% 
  mutate(edad_por10 = Age * 10,
         id_viajero = row_number())

#podemos querer dividir nombre y apellido en dos columnas, para eso utilizamos separate()

info_viajeros <- info_viajeros %>% 
  separate(col = Name, into = c("Nombre", "Apellido"), sep = " ", remove = F)

#Slicing es una operacion comun y util, en python usamos loc e iloc, aqui la funcion slice

info_50_65 <- info_viajeros %>% slice(50,65)
#notese que me selecciona las dos filas cuyo indice son el 50 y el 65, si quisiera del 50 al 65 debería hacer
info_50_65 <- info_viajeros %>% slice(50:65)

#Podemos obtener los primeros o ultimos n cantidad de valores
info_viajeros %>% slice_head(n=5)

#puede ser un porcentaje no solo un nro entero en este caso queremos el último 10%
info_viajeros %>% slice_tail(prop=0.1)

#podríamos tambien querer dividir el df en funcion de valores de cierta columna

#Devuelve los tres viajeros más jóvenes por género 
info_viajeros %>%
  group_by(Gender) %>%
  slice_min(Age, n = 3, with_ties = FALSE)

#Devuelve el 2% del total de viajeros con mayor edad por genero
info_viajeros %>% group_by(Gender) %>% slice_max(Age, prop = 0.02, with_ties=FALSE)

#Puedo querer obtener muestras

#20 muestras aleatorias
info_viajeros %>% slice_sample(n=20)

#Muestra del 20% 
info_viajeros %>% slice_sample(prop=0.2, replace=True)

#Muestra del 10% del total de observaciones por cada genero.
info_viajeros %>% group_by(Gender) %>% 
  slice_sample(prop=0.1)


# Join

#Observamos la columna destino, vemos que tiene valores con nombres de ciudades y paises separados por coma, 
#aunque no siempre es así por ejemplo Paris aparece sin referencia al pais Francia
# para solucionar este tema vamos a recurrir al dataset cities by countries de kaggle
#este dataset además tiene datos de la población de cada una de las ciudades, según datos oficiales del 2022


cities_bycountries <- read.csv("data/cities_by_countries/worldcities.csv")

# Separamos la columna destination en dos utilizando la coma como separador como ya vimos

travel_details <- travel_details %>% 
  separate(col = Destination, into = c("City", "Country"), sep = ",", remove = F)

travel_details %>% 
  count(is.na(Country)) #tenemos 68 destinos sin países

# buscaremos obtener este dato mediante la base de ciudades por países, para ello usaremos la función left_join() de dplyr

cities_bycountries %>% 
  distinct(city)

travel_details_countries <- left_join(travel_details, cities_bycountries, by = c("City" = "city"))

#observamos un error y es que el dataset tiene ciudades con nombres repetidos como el Paris de Francia y el Paris de Texas
# buscamos solucionar este problema solo quedandome con las ciudades capitales o capitales administrativas

cities_bycountries1 <- cities_bycountries %>% 
  filter(capital == "primary" | capital == "admin") %>% 
  arrange(city)

travel_details_countries <- left_join(travel_details, cities_bycountries1, by = c("City" = "city"))
#no es tan preciso porque no todas las ciudades destino son capitales

#otra opcion es quedarnos por nombre de ciudad, con la ciudad con mayor población. 

# Agrupar las ciudades por nombre
groups <- cities_bycountries %>%
  group_by(city_ascii)

# Seleccionar la ciudad con mayor población dentro de cada grupo
cities <- groups %>%
  slice_max(population) %>%
  ungroup() #notese la función ungroup dentro del pipe

# Descartar las demás ciudades
cities <- cities %>% distinct()

travel_details_countries1 <- left_join(travel_details, cities, by = c("City" = "city"))

#filtramos los valores con na en country
missing_data <- travel_details_countries %>%
  filter(is.na(country))

travel_details_countries1 <- travel_details_countries1 %>% 
  filter(!is.na(country))

# Ahora podemos comparar población con lugar de destino ya que tenemos las dos columnas en el df

viajerosxpais <- travel_details_countries1 %>% 
  group_by(population, country) %>% 
  summarise(cantidad_viajeros_pais = n()) %>% 
  arrange(desc(population)) 

# existe correlación entre viajeros recibidos y población del país?
cor(viajerosxpais$population, viajerosxpais$cantidad_viajeros_pais)


# Creamos el gráfico
plot(viajerosxpais$population, viajerosxpais$cantidad_viajeros_pais, pch = 19, col = "lightblue")

# Línea de regresión
abline(lm(viajerosxpais$cantidad_viajeros_pais ~ viajerosxpais$population), col = "red", lwd = 3)


# Funciones definidas y la función map() para aplicar a todo un df

#una herramienta muy util es como en otros lenguajes definir funciones en R se realiza utilizando function

mi.factorial <- function(n){
  factorial <- n
  while (n > 1){
    n <- n - 1
    factorial <- factorial * n
  }
  return(factorial)
}

mi.factorial(7)


cuadrado.raro <- function(x) if(x < 5) x^2 else -x^2 

cuadrado.raro(12)

#Lo interesante de R es que podemos al igual que en python aplicar funciones a toda una columna o a multiples columnas
#evitando tener que iterar sobre una columna lo cual nos ahorra mucho tiempo y computo
# para ello existen funciones como map() apply() o sapply()

sapply(1:10, cuadrado.raro)

#provemos aplicar el cuadrado raro a la edad de los viajeros

cuadrado_raro_edad <- travel_details %>% 
  filter(!is.na(Traveler.age)) 

cuadrado_raro_edad$age_rara <- map(cuadrado_raro_edad$Traveler.age, cuadrado.raro)

#otro punto interesante es la posibilidad de definir funciones temporales y aplicarlas al estilo lambda

cuadrado_raro_edad$age_alapi <- map(cuadrado_raro_edad$Traveler.age, function(x) x^pi)

# Links y capacitaciones Úiles 
"""
https://r4ds.had.co.nz/
https://www.rdocumentation.org/
https://www.datanalytics.com/libro_r
https://bitsandbricks.github.io/ciencia_de_datos_gente_sociable
https://datosgcba.github.io/ciencia-de-datos-politicas-publicas
https://bookdown.org/jboscomendoza/r-principiantes4
https://bookdown.org/gboccardo/manual-ED-UCH/


# Paquetes
https://github.com/pablotis/asombrosos-paquetes-r-latinoamerica

"""
