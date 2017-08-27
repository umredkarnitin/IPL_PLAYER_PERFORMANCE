## Hitter Data Set from Statistical Learning course
library(ISLR)
?Hitters
summary(Hitters)
str(Hitters)
sum(is.na(Hitters))

write.csv(Hitters,"F:\\Data Scientist\\Day4\\Hitter.csv")
##There are some missing values here, so before we proceed we will remove them:

Hitters<- na.omit(Hitters)
sum(is.na(Hitters))

with(Hitters,sum(is.na(Hitters$Salary)))

## Use Leaps Library for Best data set
library(leaps)

regfit.full<- regsubsets(Hitters$Salary~.,data=Hitters)
summary(regfit.full)

##It gives by default best-subsets up to size 8; lets increase that to 19, i.e. all the variables
regfit.nvmax<- regsubsets(Hitters$Salary~., Hitters,nvmax=19)
summary(regfit.nvmax)
regfit.nvmax.summary<-summary(regfit.nvmax)

names(summary(regfit.nvmax))
plot(regfit.nvmax.summary$cp,xlab = "Number of Variables", ylab = "Cp")

which.min(regfit.nvmax.summary$cp)
points(10, regfit.nvmax.summary$cp[10], pch = 20, col = "red")

plot(regfit.full, scale = "Cp")

##Model Selection Using a Validation Set

dim(Hitters)
x<-sample(Hitters[1:nrow(Hitters),])

train<- x[1:(.8*nrow(Hitters)),]
test<- x[-train,]

regfit.fwd <- regsubsets(Salary ~ ., data =train, nvmax = 19, method = "forward")
regfit.fwd$id
val.errors = rep(NA, 19)
x.test<- model.matrix(Salary~.,train)
summary(x.test)

for(i in 1:19){
  coeif=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)] %*% coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}

##Ridge Regression and the Lasso
##We will use the package glmnet, which does not use the model formula language, so we will set up an x and y.

library(glmnet)
x = model.matrix(Salary ~ . - 1, data = Hitters)
summary(x)
y = Hitters$Salary

##Here alpha=0 means Ridge penalty will be use alpha=1 means Lasso Penalty will be used
fit.ridge = glmnet(x, y,alpha=0)
plot(fit.ridge,xvar= "lambda", label = TRUE)

##There is also a cv.glmnet function which will do the cross-validation for us.
cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

##Now we fit a lasso model; for this we use the default alpha=1

fit.lasso = glmnet(x, y,alpha=1)
plot(fit.lasso,xvar= "lambda", label = TRUE)

##There is also a cv.glmnet function which will do the cross-validation for us.
cv.lasso = cv.glmnet(x, y, alpha = 1)
plot(cv.lasso)


coef(cv.lasso)
class(coef(cv.lasso))

##Suppose we want to use our earlier train/validation division to select the lambda for the lasso. This is easy to do.

lasso.tr<- glmnet(x[train, ], y[train])
lasso.tr