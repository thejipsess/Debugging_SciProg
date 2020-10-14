# You need these packages for the example to run
install.packages("randomForest")
library(randomForest)
library(ROCR)
library(MASS)
library(ggplot2)
#Scaling of the data (iris data with 4 variables and 3 classes)
data2<-scale(iris[,1:4], center = TRUE, scale = TRUE)
#data2 <- log10(scale_iris[,1:4])
class<-iris[,5]
#PCA analysis

pca<-prcomp(data2,center = TRUE,scale = TRUE)
#calculating %variance
variance_PC<-100*pca$sdev^2/sum(pca$sdev)
#Add plotting 
PCA_scores<-pca$x
ggplot(PCA_scores[,1:2], aes(x=PC1, y=PC2, shape=class, color=class)) +geom_point()
#COmparing PCA and URF
Iris.urf <- randomForest(data2)
crowsD<-nrow(Iris.urf$proximity)
R = do.call(cbind, rep(list(rowMeans(Boston.urf$proximity)), crowsD))
C = do.call(cbind, rep(list(colMeans(Boston.urf$proximity)), crowsD))
Doublecenter <-Iris$proximity-M-R-mean(Boston.urf$proximity[])
PCA_URF<-prcomp(Doublecenter)
scores<-data.frame(PCA_URF$x[,1:2])
ggplot(scores[,1:2], aes(x=PC1, y=PC2, shape=class, color=class)) + geom_point()
