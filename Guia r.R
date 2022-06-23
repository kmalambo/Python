
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
cars
x<-cars$dist

##promedio
x %>% mean() %>% round()

##dplyr 
datos<-iris
  
datos %>% select(Sepal.Length,Sepal.Width)

datos %>% slice(10:15)

datos %>% filter(Species == "setosa")

datos %>% arrange(Sepal.Length)#aariba a baji
datos %>% arrange(desc(Sepal.Length)) # de abajo a ariiba

##agregar una columna
datos %>% mutate(Total.Petalo = Petal.Length+Petal.Width,
                 Total.Sepalo =Sepal.Length+Sepal.Width)

## Cabimar nombre renamel
datos %>% rename(Largo.Sepalo=Sepal.Length)

## count
tabla_1= datos %>% count(Species)#data.frame
tabla_1 %>% as.tibble() %>% count(Species)## Numero de variables
 

##group_by
data_agrupada <- datos %>% group_by(Species)

###filtro general
datos %>% summarise(Promdio_Largo_petalo= mean(Petal.Length))

#filtro por especie
data_agrupada %>% summarise(Promdio_Largo_petalo= mean(Petal.Length))


##Unir funciones 

data3 <- datos %>% 
  select(Sepal.Length,Petal.Length,Species) %>%
  filter(Sepal.Length >5,Petal.Length>3.5) %>%
  mutate(Total.Largo=Sepal.Length+Petal.Length) %>%
  group_by(Species)%>% 
  summarise(Promedio_largo=mean(Total.Largo,na.rm=TRUE)) 
data3                          



##Modelo de regresión lineal
# Calcular los parámetros del modelo lineal
ab_fit <- lm(Petal.Length ~ Sepal.Length, data = iris)
# Plotear la línea de regresión estimada sobre el gráfico de dispersión
plot(Petal.Length ~ Sepal.Length, data = iris, xlab = "Longitud del sépalo", ylab = "Longitud del pétalo")
abline(ab_fit, col = "skyblue", lwd = 3)
summary(ab_fit)


#GGPLOT
mtcars
ggplot(data = mtcars) + 
  geom_point(aes(mpg, qsec, colour = factor(am))) +
  facet_grid(~vs)

ggplot(data = mtcars) + geom_histogram(aes(x=qsec,fill=factor(am)),bins=10, position = "stack",alpha = 0.5)#stack

ggplot(mtcars, aes(mpg, qsec)) + geom_point(aes(size = hp), alpha = 0.4)+
  facet_wrap(~factor(am))


### Clustering
install.packages("pacman")
library(pacman)
p_load(cluster, aplpack, fpc, foreign, TeachingDemos,
       factoextra, NbClust, ape, corrplot, DataExplorer,
       funModeling, compareGroups, tidyverse, dendextend,
       igraph, FeatureImpCluster, flexclust, LICORS, h2o,
       gghighlight)
