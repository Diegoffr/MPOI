#********UNIVERSIDAD POLITECNICA SALESIANA*************

#******************************************************

#TAREA 1 CLUSTERING

#Vicente Barba
#Angel Gaibor
#Bryan Mora
#Dario Jacho
#Diego Flores


#install.packages("readr")
library(readr)
segmentation_data <- read_csv("Maestria UPS/Herramientas para control de la produccion/Deberes/segmentation_data.csv")
#View(segmentation_data)
sd = segmentation_data
sd
#####Escalamiento Multidimensional######
d = dist(sd[,2:4], method = "euclidean")
fit = cmdscale(d,eig=TRUE, k=2) # k es el numero de dimensiones
x = fit$points[,1] 
y = fit$points[,2]
plot(x,y)
text(x, y, labels = row.names(iris), cex=1)


#########Crear Grupos: K-Means########

set.seed(1)
grupos = kmeans(sd[,2:4],4)
g1 = grupos$cluster
g2 = grupos$size
plot(x,y,col=c("red","green3","blue","black")[g1], main = "sd Dataset K-Means")
g1
g2
#Creacion de las Clases

cliente= factor(grupos$cluster,labels= c("VIP","POCO_FRECUENTE","NUEVOS","POSIBLE_VIP"))
cliente

plot(x,y,col=c("red","green3","blue","black")[cliente], main = "SD Dataset Original")
cliente
summary(cliente)

#########Crear Grupos: Jerarquico#########
library("dendextend")
hc = hclust(d, method = "complete" )
clus4 = cutree(hc, 4)
dend = as.dendrogram(hc)
dend = color_branches(dend, 4)
colors = c("red", "green3", "blue","black")
plot(dend, fill = colors[clus4], cex = 0.1 , main = "Clustering Jerarquico")

######### Elbow #########
# Crea diferentes valores de k
wi = c()
for (i in 1:10) 
{
  g = kmeans(sd[,2:4],i) 
  wi[i] = g$tot.withinss
}
plot((1:length(wi)),wi, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

######### Validacion Interna #########
# Indice de Dunn
#install.packages("cluster")
#install.packages("clValid")
library(cluster)
library(clValid)
du1 = dunn(d,g1)
du2 = dunn(d,clus4)

# Coeficiente de Silueta
sil1 = silhouette(g1,d)
plot(sil1,col=1:4, border=NA)

#sil2 = silhouette(clus4,d)
#plot(sil2,col=5:8, border=NA)

######### Validacion Externa #########
# ARI, AMI, NMI
#install.packages("aricode")
library(aricode)
library(plyr)
ground = cliente
ground = revalue(ground, c("VIP"="1","Poco_Frecuente"="2", "Nuevos"="3", "POSIBLE_VIP"="4"))
ARI1= ARI(ground,g1) 
ARI2= ARI(ground,clus4)
AMI1= AMI(ground,g1)
AMI2= AMI(ground,clus4)
NMI1= NMI(ground,g1,variant = c("joint"))
NMI2= NMI(ground,clus4,variant = c("joint"))
ENT = entropy(ground,clus4)
ENT
 
