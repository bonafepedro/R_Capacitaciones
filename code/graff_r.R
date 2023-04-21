#Ahora vamos a ver algunas formas simples de graficar en R usando las herramientas vistas anteriormente

library(dplyr)

# Graficos de barras

## podemos graficar la edad promedio de los viajeros por país de origen, retomando el código usado anteriormente

info_viajeros %>% 
  group_by(Nacionality) %>% 
  summarise(age_prom = mean(Age)) %>%
  ggplot() +
  geom_bar(aes(x = Nacionality, weight =age_prom ))

#poco práctico no? nos quedemos con los 5 paises con más cantidad de viajeros


viajeros_nacionalidad_edad <- info_viajeros %>%
  group_by(Nacionality) %>% 
  summarise(age_prom = mean(Age),
            count_travelers = n()) %>%
  slice_max(count_travelers, n=5) 

ggplot(viajeros_nacionalidad_edad) +
  geom_bar(aes(x = Nacionality, weight =age_prom))
# Scatter plot ¿existe una relación entre días de viaje y edad de los viajeros?

ggplot(travel_details) +
  geom_point(aes(x = Duration..days. , y = Traveler.age))
#no parece
