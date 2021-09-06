#Preparamos el set de datos
insurance <- read.csv("insurance.csv", encoding="UTF-8", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
insurance.scale <- as.data.frame(scale(insurance[,5:9])) # escalar los datos

#Creamos los clusters
set.seed(80) # fijar semilla

insurance.km <- kmeans(insurance.scale, centers = 4) # Realizamos clustering
names(insurance.km) # contenido del objeto

head(insurance.km$cluster) # asignación observaciones a clusters
insurance.km$totss # inercia total
insurance.km$betweenss # inercia ínter grupos
insurance.km$withinss # inercia intra grupos
insurance.km$tot.withinss # inercia intra grupos (total)

#Determinar un número de clusters óptimo
sumbt<-kmeans(insurance.scale, centers = 1)$betweenss

for(i in 2:10) sumbt[i] <- kmeans(insurance.scale, centers = i)$betweenss

plot(1:10, sumbt, type = "b", xlab = "número de clusters", ylab = "suma de cuadrados ínter grupos")

#Inspeccionando los resultados
plot(insurance$ant_comp,insurance$ant_perm, col=insurance.km$cluster ,xlab = "Fidelidad a la compañía", ylab = "Experiencia" )

aggregate(insurance[,5:9] ,by = list(insurance.km$cluster), mean)
