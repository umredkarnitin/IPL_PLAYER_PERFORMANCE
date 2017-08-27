## Logistics Regression on MTcars

library(MASS)
attach(mtcars)
write.csv(mtcars,"F:\\Data Scientist\\Day5\\mtcars.csv")
str(mtcars)
str(Smarket)

head(Smarket)
summary(Smarket)

dim(mtcars)

set.seed(99)
x<-mtcars[sample(1:nrow(mtcars)),]

train<-x[1:20,]
test<-x[21:nrow(mtcars),]

is.factor(train$am)

train$am<-as.factor(train$am)

am.glm<- glm(formula=as.factor(am)~.,data=train,family=binomial,control=list(maxit=100))
summary(am.glm)
newdata<- data.frame(hp=120,wt=2.8)

predict(am.glm,newdata,type="response")

round(predict(am.glm,test,type="response"),0)
test$am

## Linear Descriminant Function

am.lda<- lda(formula=as.factor(am)~.,data=train)
am.lda
am.lda.pd<- predict(am.lda,test)$class
table(am.lda.pd,test[,8])

## From best subset analausis we know that Cyl & qsec are the best varibale which ca

am.lda2<- lda(formula=as.factor(am)~cyl+qsec,data=train)
am.lda2

am.lda.pd2<- predict(am.lda2,test)$class
table(am.lda.pd2,test[,8])


## For an automobile with 120 hp engine and 2800 ibs weight
##the probability of it being fitted with a manual transmission is about 64%

##We are provided a sample of 1000 customers. We need to predict the probability whether a customer will buy (y) a particular magazine or not. As you can see, we've a categorical outcome variable, we'll use logistic regression.
##To start with logistic regression, I'll first write the simple linear regression equation with dependent variable enclosed in a link function:
  
  g(y) = ??o + ??(Age)  

This function is established using two things: Probability of Success(p) and Probability of Failure(1-p). p should meet following criteria:
It must always be positive (since p >= 0)
It must always be less than equals to 1 (since p <= 1)

Performance of Logistic Regression Model

To evaluate the performance of a logistic regression model,
1. AIC (Akaike Information Criteria) - The analogous metric of adjusted R² in logistic regression is AIC. AIC is the measure of fit which penalizes model for the number of model coefficients. Therefore, we always prefer model with minimum AIC value.

2. Null Deviance and Residual Deviance - Null Deviance indicates the response predicted by a model with nothing but an intercept. Lower the value, better the model. Residual deviance indicates the response predicted by a model on adding independent variables. Lower the value, better the model.

3. Confusion Matrix: It is nothing but a tabular representation of Actual vs Predicted values. This helps us to find the accuracy of the model and avoid overfitting. This is how it looks like:
  
  

am.glm<- glm(formula=as.factor(am)~hp+wt+disp+gear,data=train,family=binomial)

am.glm<- glm(formula=as.factor(am)~hp+wt+disp+gear,data=train,family=binomial,control=list(maxit=50))
summary(am.glm)
newdata<- data.frame(hp=120,wt=2.8)

##
am.glm<- glm(formula=as.factor(am)~hp+wt+gear,data=train,family=binomial)
summary(am.glm)
predict <- predict(am.glm, type = 'response')

pred=predict(am.glm,test,type="response")

round(pred,0)
cbind(test$am,round(pred,0))

## Using Wages dataset
require(ISLR)
attach(Wage)
str(Wage)

write.csv(Wage,"F:\\Data Scientist\\Day5\\Wage.csv")
## logistic regression model to a binary response variable, constructed from wage. We code the big earners (>250K) as 1, else 0.

plot(I(wage > 250)~age,Wage)
fit = glm(I(wage > 250) ~ age, data = Wage, family = binomial)
summary(fit)

table(I(wage > 250))


#confusion matrix
table(test$am, predict > 0.5)


library(leaps)
regfit.full <- regsubsets(am ~ mpg+cyl+disp+hp+drat+wt+qsec+vs+gear+carb, data = train)
summary(regfit.full)

regfit.full <- regsubsets(am ~ mpg+cyl+disp+hp+drat+wt+qsec+vs+gear+carb, data = train,nvmax=5)
reg.summary = summary(regfit.full)
names(reg.summary)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)

plot(regfit.full,scale="Cp")

coef(regfit.full,5)

## Forward stepwise 
regfit.fwd = regsubsets(am ~ ., data = train, nvmax = 10, method = "forward")
summary(regfit.fwd)

plot(regfit.fwd,scale="Cp")


##Model Selection Using a Validation Set

dim(train)
predict(am.glm, data = test)  # notice the -index!

pairs(Smarket,col=Smarket$Direction)
sum(is.na(Smarket))
##
## Logistic regression

glm.fit<- glm(as.factor(Smarket$Direction)~Smarket$Lag1+Smarket$Lag2+Smarket$Lag3+Smarket$Lag4+Smarket$Lag5+Smarket$Volume,data=Smarket,family=binomial)

summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 
round(glm.probs[1:5],0)
glm.pred=ifelse(glm.probs>0.5,"up","down")
attach(Smarket)
write.csv(Smarket,"F:\\Data Scientist\\Day5\\Smarket.csv")
table(glm.pred,Direction)


lda.fit<- lda(as.factor(Smarket$Direction)~Smarket$Lag1+Smarket$Lag2+Smarket$Lag3+Smarket$Lag4+Smarket$Lag5+Smarket$Volume,data=Smarket)
lda.fit

lda.fit.p<-predict(lda.fit,Smarket)$class
table(Smarket$Direction)
table(lda.fit.p)
