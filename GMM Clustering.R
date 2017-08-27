install.packages("flexclust", dependencies = T)
library(flexclust)
milk
a<-data(milk, package="flexclust")
write.csv(milk,"F:\\Data Scientist\\Day12\\milk.csv")
str(a)
milk.scaled <- scale(milk)
mclust::mclustBIC(milk.scaled)
library(mclust)
xyMclust=Mclust(milk.scaled)
summary(xyMclust)
names(xyMclust)
plot(xyMclust)

classMilk <- xyMclust$classification
mmm <- data.frame(milk,classMilk)
mmm