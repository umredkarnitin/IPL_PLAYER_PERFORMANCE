library(MASS)
library(ISLR)
state.x77

datax77= as.data.frame(state.x77)
str(datax77)
write.csv(datax77,"F:\\Data Scientist\\Day5\\datax77.csv")
colnames(datax77)[4]="Life_Exp"
colnames(datax77)[6]="HS.Grad"

datax77[,9]=datax77$Population*1000/datax77$Area

colnames(datax77)[9]="Population_Density"
str(datax77)
summary(datax77)

cor(datax77)

pairs(datax77)

model1<- lm(Life_Exp ~ ., data=datax77)

summary(model1)
plot(model1)

install.packages("leaps")
install.packages("bestglm")

library(leaps)
library(bestglm)

lea<-regsubsets(Life_Exp~ Income+Illiteracy+Murder+HS.Grad+Frost+Population_Density,
           data=datax77,nbest=10)

lea<-regsubsets(Life_Exp~ .,
                data=datax77,nbest=10)


summary(lea)
plot(lea,scale="adjr2")

leanv<-regsubsets(Life_Exp~.,data=datax77,nvmax=4)
summary(leanv)
plot(leanv,scale="adjr2")

leaback<-regsubsets(Life_Exp~.,data=datax77,method="backward")
summary(leaback)
plot(leaback,scale="adjr2")


leaforw<-regsubsets(Life_Exp~.,data=datax77,method="forward")
summary(leaforw)
plot(leaforw,scale="adjr2")

lea<-regsubsets(Life_Exp~ Murder+HS.Grad+Frost+Population_Density,data=datax77,nvmax=4)


summary(lea)
plot(lea,scale="adjr2")
