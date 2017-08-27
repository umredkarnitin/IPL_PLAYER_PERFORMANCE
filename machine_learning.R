##4.2 Machine learning 
##Collect data
##prepare the input data

##analyze the input data

ls()
load("./titanic.raw.rdata")
load("F:/Data Scientist/Day4/titanic.raw.rdata")
write.csv(titanic.raw,"F:/Data Scientist/Day4/titanic.csv")


summary(titanic.raw)

library(arules)
install.packages("grid")
library(grid)

library(arulesViz)



table(titanic.raw$Class,titanic.raw$Sex)
table(titanic.raw$Class,titanic.raw$Age)
table(titanic.raw$Class,titanic.raw$Survived)

str(titanic.raw)

table(titanic.raw$Survived)
dim(titanic.raw)

table(titanic.raw$Class,titanic.raw$Survived)

rules<- apriori(titanic.raw,parameter=list(minlen=2,supp=.005,conf=.8))
?apriori
inspect(rules)

plot(rules)

summary(rules)
str(rules)


rules<- apriori(data=titanic.raw,
                parameter=list(minlen=2,supp=.005,conf=.8),
                appearance=list(rhs=c("Survived=No","Survived=Yes"),
                                default="lhs"),
                control=list(verbose=F))
inspect(rules)
rules.sorted<- sort(rules,by ="lift")
inspect(rules.sorted)


## Find the redundant rules

subset.matrix<- is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag=T)]<- NA
redundant<- colSums(subset.matrix,na.rm=T)>=1
which(redundant)

plot(rules)
plot(rules.sorted,method="graph",control=list(type="items"))




## Apriori for Adult dataset
data("Adult")
write.csv(Adult,"F:/Data Scientist/Day4/Adult.csv")

??arules
str(Adult)
summary(Adult)
head(Adult)
attach(Adult)

rules<- apriori(Adult, parameter=list(support=.5,conf=.9, target="rules" ))
inspect(rules)

rules<- apriori(data=Adult,
                parameter=list(minlen=2,supp=.005,conf=.8),
                appearance=list(rhs=c("capital-loss=None","capital-gain=None"),
                                default="lhs"),
                control=list(verbose=F))
inspect(rules)
dim(Adult)
rules.sorted<- sort(rules,by ="lift")
inspect(rules.sorted)

## Find the redundant rules

subset.matrix<- is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag=T)]<- NA
redundant<- colSums(subset.matrix,na.rm=T)>=1
which(redundant)

plot(rules)
plot(rules.sorted,method="graph",control=list(type="items"))

## CHAID ANALYSIS for ADULT
data(ADULT)
str(Adult)
summary(Adult)
head(Adult)
attach(Adult)
detach(Adult)

head(Adult)

dt.chaid<- chaid(formula=~.,
                 control=chaid_control(minprob = 0.001,minsplit = 500,minbucket = 200),
                 data=Adult
                 )


## Analytics Vidya Data set

Groceries<- read.csv("F:/Data Scientist/Day4/groceries.csv")
str(Groceries)
head(Groceries)
dimnames(Groceries)

data(Groceries)
# Create an item frequency plot for the top 20 items
itemFrequencyPlot(Groceries,topN=20,type="absolute")
?itemFrequencyPlot

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules)
# Show the top 5 rules, but only 2 digits
options(digits=2)
rules<- sort(x=rules,by="lift")
inspect(rules[1:5])
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])


rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
inspect(rules[1:5])

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules[1:5])
plot(rules)


##Targeting Items
##What are customers likely to buy before buying whole milk
##What are customers likely to buy if they purchase whole milk?
rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


##Likewise, we can set the left hand side to be "whole milk" and find its antecedents.
##We set the confidence to 0.15 since we get no rules with 0.8
##We set a minimum length of 2 to avoid empty left hand side items

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

##
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)

##CHAID ANALYSIS

dt.chaid  <- chaid(~ , 
                   control = chaid_control(minprob = 0.001,
                                           minsplit = 500,minbucket = 200),
                   data=grocery)

plot(dt.chaid, 
     uniform = T, 
     compress = T, 
     margin = 0.2, 
     branch = 0.3)
# Label on Decision Tree
text(dt.chaid, 
     use.n = T, 
     digits = 3, 
     cex = 0.6)
summary(dt.chaid)



## IRIS Dataset
str(iris)
dim(iris)
write.csv(iris,"F:/Data Scientist/Day4/iris.csv")
summary(iris)
head(iris)
## Multiple regression for Iris data set in R 

set.seed(2000)
data(iris)

x<- iris[sample(1:nrow(iris)),]

train<- x[1:100,]
test<- x[101:150,]



head(train)
head(test)
plot(x,col=x$Species)

x$Species
iris$Species
iris

nrow(train)
nrow(test)
pairs(train,col=train$Species)
cor(train$Petal.Length,train$Petal.Width)
install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)
str(iris)
chart.Correlation(iris[1:4],method="spearman")

hist(x$Petal.Width)
multi_reg_model<- lm(Petal.Width ~ Petal.Length+Sepal.Length+Sepal.Width,data=train)
summary(multi_reg_model)

boxplot(multi_reg_model$residuals)
par(mfrow=c(2,2))
plot(multi_reg_model)

predict.lm(multi_reg_model,newdata=nd)
nd<-(x[101:150,])
test$Petal.Width

boxplot(predict.lm(multi_reg_model,test)-test$Petal.Length)




## Evaluate multi collinearity

library(car)
vif(multi_reg_model) ## Variance inflation factors

sqrt(vif(multi_reg_model))

#####################################################################################
install.packages("caret")
install.packages("fields")
install.packages("plot3D")
install.packages("nortest")
install.packages("e1071")
install.packages("caret")
install.packages("fields")
install.packages("plot3D")
install.packages("nortest")
install.packages("e1071")

library(nortest)
library(ggplot2)
library(data.table)
library(magrittr)
library(caret)
library(fields)
library(plot3D)
library(e1071)
library(MASS)
library(ISLR)

## with MR data set
plot(x = Boston$medv,y = Boston$lstat)
str(Boston)
chart.Correlation(Boston,method="pearson")


set.seed(2000)
x<- Boston[sample(1:nrow(Boston)),]

nrow(Boston)
train<-x[1:400,]
test<-x[401:506,]

pairs(Boston)

cor(train$medv,train$indus)

Sample_Multi_Model<- lm(medv~.,data=train)
summary(Sample_Multi_Model)
plot(Sample_Multi_Model)
vif(Sample_Multi_Model)

rm(Sample_Multi_Model)
Sample_Multi_Model_1<- update(Sample_Multi_Model,~.-train$indus-train$age)
summary(Sample_Multi_Model_1)
plot(Sample_Multi_Model_1)

vif(Sample_Multi_Model)
boxplot(predict.lm(Sample_Multi_Model,test)-test$medv)

## Best subset analysis
lea<- regsubsets(medv~.,train,nbest=10)
summary(lea)
plot(lea,scale="adjr2")

leanv<- regsubsets(medv~.,train,nvmax=4)
summary(leanv)
plot(leanv,scale="adjr2")

leaback<-regsubsets(medv~.,train,method="backward")
summary(leaback)
plot(leaback,scale="adjr2")


## with MR data set
library(foreign)
MR_REG_CONSUMER_DURABLE<- read.spss("F:/Study/SECOND SEMESTER/MR-2/mr(2)case/2.Correlation Regression/Consumer Durables/case 3-Consumer Durables.sav")
MR_REG_CONSUMER_DURABLE<-as.data.frame(MR_REG_CONSUMER_DURABLE)
attach(MR_REG_CONSUMER_DURABLE)
str(MR_REG_CONSUMER_DURABLE)
summary(MR_REG_CONSUMER_DURABLE)

pairs(MR_REG_CONSUMER_DURABLE)

library(PerformanceAnalytics)
chart.Correlation(MR_REG_CONSUMER_DURABLE,method="pearson")

set.seed(2000)
data(MR_REG_CONSUMER_DURABLE)
x<- MR_REG_CONSUMER_DURABLE[sample(1:nrow(MR_REG_CONSUMER_DURABLE)),]

multi_reg_model<-lm(SLS~.,MR_REG_CONSUMER_DURABLE)
summary(multi_reg_model)
plot(multi_reg_model)
library(leaps)
vif(multi_reg_model)

## Since F statistic showing model Adjusted RSquare is quite significant F-statistic: 57.13 on 6 and 8 DF,  p-value: 3.911e-06
##& t value for variable is not showing that much significance we need to remove the correlated varible

multi_reg_model<-lm(SLS~SLSSTAFF+COMPACT,MR_REG_CONSUMER_DURABLE)
summary(multi_reg_model)
plot(multi_reg_model)
library(leaps)
library(car)

vif(multi_reg_model)
??vif


## Analysis of normality of the independent data
ad.test(MR_REG_CONSUMER_DURABLE$SLSSTAFF)
qqnorm(MR_REG_CONSUMER_DURABLE$SLSSTAFF)
qqline(MR_REG_CONSUMER_DURABLE$SLSSTAFF)

hist(MR_REG_CONSUMER_DURABLE$SLSSTAFF,breaks = 5)
skewness(MR_REG_CONSUMER_DURABLE$SLSSTAFF,type = 2)
kurtosis(MR_REG_CONSUMER_DURABLE$SLSSTAFF,type = 2)

plot(SLS ~ SLSSTAFF, col="brown")
pairs(MR_REG_CONSUMER_DURABLE,col="brown")
fit_1<-lm(SLS ~ SLSSTAFF, data=MR_REG_CONSUMER_DURABLE)
fit_1
par(mfrow=c(2,2))
plot(fit_1)
str(fit_1)
summary(fit_1)
par(mfrow=c(1,1))
plot(SLS ~ SLSSTAFF, col="brown")
abline(fit_1,col="red")
names(fit_1)
confint(fit_1)
predict(fit_1,data.frame(SLSSTAFF=c(55,100,444)),interval="confidence")


##HBAT Data
MT_HBAT<- foreign::read.spss("E:/Multivariant ANalysis/Multivariate_Data_Analysis_7e_Datasets_and_Documentation/HBAT.sav",to.data.frame = TRUE)
dplyr::transmute(MT_HBAT, xx7 = log10(x7))

names(MT_HBAT)
str(MT_HBAT)

MT_HBAT<- as.data.frame(MT_HBAT)
str(MT_HBAT)

cor(x=MT_HBAT$x19,y=MT_HBAT$x6+MT_HBAT$x7+MT_HBAT$x8+MT_HBAT$x9+MT_HBAT$x10+MT_HBAT$x11+MT_HBAT$x12+MT_HBAT$x13+MT_HBAT$x14+MT_HBAT$x15+MT_HBAT$x16+MT_HBAT$x17+MT_HBAT$x18)
library(PerformanceAnalytics)
chart.Correlation(MT_HBAT[7:20])

## Analysis of normality of the dependent data
ad.test(MT_HBAT$x19)
qqnorm(MT_HBAT$x19)
qqline(MT_HBAT$x19)
hist(I(MT_HBAT$x19),breaks = 10)
skewness(MT_HBAT$x6,2)
kurtosis(MT_HBAT$x6,2)

skewness(MT_HBAT$x6,2)
kurtosis(MT_HBAT$x6,2)

ad.test(MT_HBAT$x7)
qqnorm(MT_HBAT$x7)
qqline(MT_HBAT$x7)

hist(MT_HBAT$x7,breaks = 10)
skewness(MT_HBAT$x7)
kurtosis(MT_HBAT$x7)


##x8
ad.test(MT_HBAT$x8)
qqnorm(MT_HBAT$x8)
qqline(MT_HBAT$x8)

hist(MT_HBAT$x8,breaks = 10)
skewness(MT_HBAT$x8)
kurtosis(MT_HBAT$x8)

##x9
ad.test(MT_HBAT$x9)
qqnorm(MT_HBAT$x9)
qqline(MT_HBAT$x9)

hist(MT_HBAT$x9,breaks = 10)
skewness(MT_HBAT$x9)
kurtosis(MT_HBAT$x9)

##x10
ad.test(MT_HBAT$x10)
qqnorm(MT_HBAT$x10)
qqline(MT_HBAT$x10)

hist(MT_HBAT$x10,breaks = 10)
skewness(MT_HBAT$x10)
kurtosis(MT_HBAT$x10)

##x11
ad.test(MT_HBAT$x11)
qqnorm(MT_HBAT$x11)
qqline(MT_HBAT$x11)

hist(MT_HBAT$x11,breaks = 10)
skewness(MT_HBAT$x11)
kurtosis(MT_HBAT$x11)


##x12
ad.test(MT_HBAT$x12)
qqnorm(MT_HBAT$x12)
qqline(MT_HBAT$x12)

hist(MT_HBAT$x12,breaks = 10)
skewness(MT_HBAT$x12)
kurtosis(MT_HBAT$x12)

##x13
ad.test(MT_HBAT$x13)
qqnorm(MT_HBAT$x13)
qqline(MT_HBAT$x13)

hist(MT_HBAT$x13^3,breaks = 10)
skewness(MT_HBAT$x13^3)
kurtosis(MT_HBAT$x13^3)

##x14
ad.test(MT_HBAT$x14)
qqnorm(MT_HBAT$x14)
qqline(MT_HBAT$x14)

hist(MT_HBAT$x14,breaks = 10)
skewness(MT_HBAT$x14)
kurtosis(MT_HBAT$x14)

##x15
ad.test(MT_HBAT$x15)
qqnorm(MT_HBAT$x15)
qqline(MT_HBAT$x15)

hist(MT_HBAT$x15,breaks = 10)
skewness(MT_HBAT$x15)
kurtosis(MT_HBAT$x15)

##x16
ad.test(MT_HBAT$x16)
qqnorm(MT_HBAT$x16)
qqline(MT_HBAT$x16)

hist(MT_HBAT$x16,breaks = 10)
skewness(MT_HBAT$x16)
kurtosis(MT_HBAT$x16)

##x17
ad.test(MT_HBAT$x17)
qqnorm(MT_HBAT$x17)
qqline(MT_HBAT$x17)

hist(MT_HBAT$x17,breaks = 10)
skewness(MT_HBAT$x17)
kurtosis(MT_HBAT$x17)

##x18
ad.test(MT_HBAT$x18)
qqnorm(MT_HBAT$x18)
qqline(MT_HBAT$x18)

hist(MT_HBAT$x18,breaks = 10)
skewness(MT_HBAT$x18)
kurtosis(MT_HBAT$x18)

par(mfrow=c(1,1))
pairs(x19 ~ x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16,data = MT_HBAT, col="Brown",)
cor(x19 ~ paste("x",6:18,sep=""),data = MT_HBAT)

chart.Correlation(MT_HBAT[7:20],method="pearson")


MT_HBAT_Multi





plot(MT_HBAT$x19, I(MT_HBAT$x7), col="red")
MT_fit1<-lm(MT_HBAT$x19~I(MT_HBAT$x7^2), data=MT_HBAT)
MT_fit1
summary(MT_fit1)
par(mfrow=c(2,2))
plot(MT_fit1)



### Multiple linear regression
fit2=lm(medv~log10(lstat)+age,data=Boston)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)

fit_2<-lm(SLS ~ SLSSTAFF+POTL+COMPACT, data=MR_REG_CONSUMER_DURABLE)
fit_2
summary(fit_2)
plot(fit_2)

MT_fit2<-lm(MT_HBAT$x19 ~ MT_HBAT$x6+MT_HBAT$x7+MT_HBAT$x8+MT_HBAT$x9, data=MT_HBAT)
summary(MT_fit2)
plot(MT_fit2)

##NOW consider all the variables 
fit3=lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)

##Updating existing linear model by removing the existing varibale for the model
fit4=update(fit3,~.-age-indus)
summary(fit4)
plot(fit4)
### Nonlinear terms and Interactions or Synergy effect
fit5=lm(medv~lstat*age,Boston)
summary(fit5)

fit6=lm(medv~lstat +I(lstat^2),Boston)
summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
dev.set(2)
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)

## Use polynomial
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)

##NOW consider all the variables 

fit_3<-lm(SLS ~ ., data=MR_REG_CONSUMER_DURABLE)
fit_3
summary(fit_3)
plot(fit_3)
##Updating existing linear model by removing the existing varibale for the model

fit_4<-update(fit_3,~.-CUSTBASE-SRVC)
summary(fit_4)
plot(fit_4)

##NOW consider all the variables 

MT_fit3<-lm(MT_HBAT$x19 ~ MT_HBAT$x6+MT_HBAT$x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17, data=MT_HBAT)
summary(MT_fit3)
plot(MT_fit3)
##Updating existing linear model by removing the existing varibale for the model
MT_fit4<-update(MT_fit3,~.-MT_HBAT$x8-MT_HBAT$x15-MT_HBAT$x17, data=MT_HBAT)
summary(MT_fit4)
par(mfrow=c(2,2))
plot(MT_fit4)

## Add polynomial & functional transformation
MT_fit6<- lm(x19 ~ I(x6^2)+I(log10(x7))+x8+x9+x10+x11+x12+I(x13^3)+x14+x15+I(x16^2)+I(x17^-1)+x18,data = MT_HBAT)
summary(MT_fit6)
par(mfrow=c(2,2))
plot(MT_fit6)


#####################################################################################################################










##Using Dummy varibale


hsb2<- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")
str(hsb2)

## Race 1= hispanic 2= Asian, 3= african, 4 = Caucasian
summary(lm(write~ race, data=hsb2))


## creating the factor variable

hsb2$race.f<- factor(hsb2$race)
is.factor(hsb2$race.f)

##regression analysis using dummy variable
summary(lm(write~ race.f, data=hsb2))

summary(lm(write~ factor(race), data=hsb2))


