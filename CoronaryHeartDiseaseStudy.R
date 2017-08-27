#framingham study

framingham<-read.csv("F:/Data Scientist/Day6/CoronaryHeartDiseaseStudy.csv")
str(framingham)

framingham$TenYearCHD<- as.factor(framingham$TenYearCHD)
framingham$education<- as.factor(framingham$education)
str(framingham)

table(framingham$education)

install.packages("caTools")
library(caTools)
set.seed(1000)
split<-sample.split(framingham$Ten,SplitRatio = .65)
train<-subset(framingham,split ==T)
test<-subset(framingham,split ==F)

pairs(train)

hist(framingham$age)
hist(log10(framingham$cigsPerDay +1))
hist(framingham$totChol,40)
sum(is.na(framingham))
summary(framingham)
str(framingham)
cor.test(framingham$cigsPerDay,framingham$totChol,na.action=NULL,data=framingham,method="pearson")

framinghamLog<-glm(TenYearCHD~.,data=train,family="binomial",na.action = na.omit)

summary(framinghamLog)

framinghamLog<-glm(as.factor(TenYearCHD)~.,data=train,family="binomial")
summary(framinghamLog)

str(test)
predictTest<-predict(framinghamLog,type="response",newdata=test)

table(test$TenYearCHD,predictTest>.5)

#Overall accuracy is TN and TP divided by total N
(1069+11)/sum(1069,187,6,11)

##baseline method suggests that you would predict 0,
#so that would be getting an accuracy of adding up the accuracy row, or 1069+6
#divided by total number of N.
(1069+6)/sum(1069,187,6,11)
install.packages("ROCR")
library(ROCR)
ROCRpred<-prediction(predictTest,test$TenYearCHD)
str(ROCRpred)
as.numeric(performance(ROCRpred,"auc")@y.values)
perf<- performance(ROCRpred,measure="tpr",x.measure="fpr")
plot(perf,col="red")
abline(0,1)
auc<-performance(ROCRpred,measure="auc")
auc<- auc@y.values[[1]]

11/(187+11)
1069/(1069+6)

## German Credit Card dataset
http://dni-institute.in/blogs/logistic-regression-using-r-german-credit-example/

GermanCredit<-read.csv("F:/Data Scientist/Day6/GermanCredit.csv",header=T)
str(GermanCredit)

summary(GermanCredit)
class(GermanCredit)

boxplot(GermanCredit$Amount~GermanCredit$Class)

boxplot(GermanCredit$Age~GermanCredit$Class)

Creating dummy variables in R

# Create dummy variables for  Amount and Duration
GermanCredit$Age_c1 <- ifelse(GermanCredit$Age <=26,1,0)
GermanCredit$Age_c2 <- ifelse(GermanCredit$Age >26 & GermanCredit$Age <=33,1,0)
GermanCredit$Amount_c1 <- ifelse(GermanCredit$Amount <=1260,1,0)
GermanCredit$Amount_c2 <- ifelse(GermanCredit$Amount >1260 & GermanCredit$Amount <=4700,1,0)
GermanCredit$Duration_c1 <- ifelse(GermanCredit$Duration <=15,1,0)
GermanCredit$Duration_c2 <- ifelse(GermanCredit$Duration >15 & GermanCredit$Duration <=30,1,0)

##Splitting Sample into Development (or Training) and Validation ( Testing) samples

##Test and Train Samples
GermanCredit.flag <- sample(1:nrow(GermanCredit),
                            0.5*nrow(GermanCredit),
                            replace = F)
GermanCredit.dev <- GermanCredit[GermanCredit.flag,]
GermanCredit.val <- GermanCredit[-GermanCredit.flag,]

##Logistic Regression using R

# Create Model formula
varlist <- names(GermanCredit)
varlist <- varlist[!varlist %in% c("Class")]

german.formula <- as.formula(paste("Class",paste(varlist,collapse = "+"),sep="~"))


german.logit <- glm(german.formula,
                    family=binomial,
                    data=GermanCredit)
summary(german.logit)
##It will consider all variables and their significance level. We can use stepwise model selection to select the significant variables.
step(german.logit,direction = "both")

ogistic Regression

german.logit <- glm(Class ~ 
                      #Amount +
                      #InstallmentRatePercentage + 
                      #ForeignWorker + 
                      CheckingAccountStatus.lt.0 +
                      CheckingAccountStatus.0.to.200 + 
                      CreditHistory.NoCredit.AllPaid + 
                      CreditHistory.ThisBank.AllPaid +
                      CreditHistory.PaidDuly + 
                      #CreditHistory.Delay + 
                      Purpose.NewCar + 
                      #Purpose.Furniture.Equipment + 
                      #Purpose.Radio.Television + 
                      #Purpose.Repairs +
                      Purpose.Education + 
                      #Purpose.Business + 
                      SavingsAccountBonds.lt.100 + 
                      #SavingsAccountBonds.100.to.500 + 
                      EmploymentDuration.4.to.7 + 
                      #Personal.Male.Single + 
                      OtherDebtorsGuarantors.None + 
                      OtherDebtorsGuarantors.CoApplicant + 
                      #Property.RealEstate + 
                      #OtherInstallmentPlans.Bank + 
                      #Housing.Rent +
                      Age_c1 + 
                      #Amount_c1 + 
                      Duration_c1 + Duration_c2, 
                    family = binomial, 
                    data = GermanCredit)
summary(german.logit )


german.logit <- glm(Class ~ 
                      #Amount +
                      #InstallmentRatePercentage + 
                      #ForeignWorker + 
                      CheckingAccountStatus.lt.0 +
                      CheckingAccountStatus.0.to.200 + 
                      CreditHistory.NoCredit.AllPaid + 
                      CreditHistory.ThisBank.AllPaid +
                      CreditHistory.PaidDuly + 
                      #CreditHistory.Delay + 
                      Purpose.NewCar + 
                      #Purpose.Furniture.Equipment + 
                      #Purpose.Radio.Television + 
                      #Purpose.Repairs +
                      Purpose.Education + 
                      #Purpose.Business + 
                      SavingsAccountBonds.lt.100 + 
                      #SavingsAccountBonds.100.to.500 + 
                      EmploymentDuration.4.to.7 + 
                      #Personal.Male.Single + 
                      OtherDebtorsGuarantors.None + 
                      OtherDebtorsGuarantors.CoApplicant + 
                      #Property.RealEstate + 
                      #OtherInstallmentPlans.Bank + 
                      #Housing.Rent +
                      Age_c1 + 
                      #Amount_c1 + 
                      Duration_c1 + Duration_c2, 
                    family = binomial, 
                    data = GermanCredit)
summary(german.logit )

library(car)
vif(german.logit)

predictTest<-predict(german.logit,type="response",newdata=GermanCredit.val)

##Commonly used Model Predictive Power Statistics are

##Confusion Matrix
##Concordance
##Gini Coefficient
##ROC /AUC
##Gains Table and Rank Ordering
##Lift Chart
##Cumulative Lift Chart
##KS Statistics


GermanCredit.dev$Pred_prob <- predict(german.logit,
                                      GermanCredit.dev,
                                      type = c("response")
)
GermanCredit.dev$Pred_Class <- ifelse(GermanCredit.dev$Pred_prob>0.67,1,0)
##Now can create or get Confusion Matrix, it gives you % accuracy for the model predicted results.

install.packages("e1071")
library(e1071)
library(caret)
GermanCredit.dev$Class1 <- ifelse(as.character(GermanCredit.dev$Class)=="Good",1,0)

# Create Confusion Matrix
confusionMatrix(data=factor(GermanCredit.dev$Pred_Class),
                reference=GermanCredit.dev$Class1,
                positive='1')

library(ROCR)
ROCRpred<-prediction(predictTest,GermanCredit.val$Class)
str(ROCRpred)
as.numeric(performance(ROCRpred,"auc")@y.values)
perf<- performance(ROCRpred,measure="tpr",x.measure="fpr")
plot(perf,col="red")
abline(0,1)
auc<-performance(ROCRpred,measure="auc")
auc<- auc@y.values[[1]]

##Lift Chart

##In lift chart, based on predicted probabilities, the 10 or 20 groups are created. For each of the group, incremental default rate or % Good are calculated.

##Gains Table

##Again in Gains Table , 10 groups are created and for each groups Observed Values of Target class are counted 
install.packages("gains")
library(gains)
# gains table
gains.cross <- gains(actual=GermanCredit.dev$Class1,
                     predicted=GermanCredit.dev$Pred_prob, 
                     groups=10)
print(gains.cross)

library(gains)
# gains table
gains.cross <- gains(actual=GermanCredit.dev$Class1,
                     predicted=GermanCredit.dev$Pred_prob, 
                     groups=10)
print(gains.cross)

## MY Telecom Data analysis for upselling customers

UPSELL_PROPENSITY<-read.csv("E:/Multivariant ANalysis/Multivariate_Data_Analysis_7e_Datasets_and_Documentation/Practical/FEB_Report.txt",header=T,sep="|")
str(UPSELL_PROPENSITY)
summary(UPSELL_PROPENSITY)

missmap(UPSELL_PROPENSITY)


hist(UPSELL_PROPENSITY$OLD_MOU,100)
hist(UPSELL_PROPENSITY$OLD_LOCAL_MOU,100)
hist(UPSELL_PROPENSITY$OLD_STD_MOU,100)
hist(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU,100)
hist(UPSELL_PROPENSITY$OLD_DEC_ARPU,100)
hist(UPSELL_PROPENSITY$OLD_ROAMING_MOU,100)


UPSELL_PROPENSITY$OLD_MOU[which(is.na(UPSELL_PROPENSITY$OLD_MOU))]=0
UPSELL_PROPENSITY$OLD_LOCAL_MOU[which(is.na(UPSELL_PROPENSITY$OLD_LOCAL_MOU))]=0
UPSELL_PROPENSITY$OLD_STD_MOU[which(is.na(UPSELL_PROPENSITY$OLD_STD_MOU))]=0
UPSELL_PROPENSITY$OLD_ROAMING_MOU[which(is.na(UPSELL_PROPENSITY$OLD_ROAMING_MOU))]=0
UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE[which(is.na(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE))]=0
UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU[which(is.na(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU))]=0
UPSELL_PROPENSITY$OLD_DEC_ARPU[which(is.na(UPSELL_PROPENSITY$OLD_DEC_ARPU))]=0

par(mfrow=c(2,3))
hist(UPSELL_PROPENSITY$OLD_MOU,100)
hist(UPSELL_PROPENSITY$OLD_LOCAL_MOU,100)
hist(UPSELL_PROPENSITY$OLD_STD_MOU,100)
hist(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU,100)
hist(UPSELL_PROPENSITY$OLD_DEC_ARPU,100)
hist(UPSELL_PROPENSITY$OLD_ROAMING_MOU,100)

library(Hmisc)
library(mice)

LOGIT_MODEL<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU+UPSELL_PROPENSITY$Old.Bill.Plan_Rental+UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE+UPSELL_PROPENSITY$OLD_LOCAL_MOU+UPSELL_PROPENSITY$OLD_STD_MOU,family = binomial,method = "pearson",control=list(maxit=50))
summary(LOGIT_MODEL)



## Variable 1
summary(UPSELL_PROPENSITY$OLD_DEC_ARPU)
sum(is.na(UPSELL_PROPENSITY$OLD_DEC_ARPU))
table(is.na(UPSELL_PROPENSITY$OLD_DEC_ARPU))
which(is.na(UPSELL_PROPENSITY$OLD_DEC_ARPU))
UPSELL_PROPENSITY$OLD_DEC_ARPU[which(is.na(UPSELL_PROPENSITY$OLD_DEC_ARPU))]=0
summary(UPSELL_PROPENSITY$OLD_DEC_ARPU)

UPSELL_PROPENSITY$OLD_DEC_ARPU<-ifelse(test=UPSELL_PROPENSITY$OLD_DEC_ARPU<0,0,UPSELL_PROPENSITY$OLD_DEC_ARPU)
summary(UPSELL_PROPENSITY$OLD_DEC_ARPU)

hist(log10(UPSELL_PROPENSITY$OLD_DEC_ARPU),100)

## Transformation of independent variable 1 
table(UPSELL_PROPENSITY$OLD_DEC_ARPU==0)
UPSELL_PROPENSITY$OLD_DEC_ARPU<-ifelse(UPSELL_PROPENSITY$OLD_DEC_ARPU!=0,log10(UPSELL_PROPENSITY$OLD_DEC_ARPU),0)
table(is.infinite(UPSELL_PROPENSITY$OLD_DEC_ARPU))
UPSELL_PROPENSITY$OLD_DEC_ARPU[which(is.infinite(UPSELL_PROPENSITY$OLD_DEC_ARPU))]=0
summary(UPSELL_PROPENSITY$OLD_DEC_ARPU)

LOGIT_MODEL<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_DEC_ARPU,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL)


## Variable 2
summary(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)
table(is.na(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU))
UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU[which(is.na(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU))]=0

summary(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)
hist(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU,100)
hist(log10(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU),100)
UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU<-ifelse(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU!=0,log10(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU),0)
summary(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)

table(is.infinite(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU))
UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU[which(is.infinite(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU))]=0
summary(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)

LOGIT_MODEL_1<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL_1)

## Varibale_3
summary(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)
UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE[which(is.na(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE))]=0
summary(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)


UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE<-UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE[UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE!=0]/1024
hist(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE,100)

hist(log10(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE[UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE>=10])^3,100)
UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE<-ifelse(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE>0,log10(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)^3,0)
summary(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)

table(is.na(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE))

LOGIT_MODEL_1<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE>0,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL_1)

## Varibale _4
summary(UPSELL_PROPENSITY$OLD_MOU)
UPSELL_PROPENSITY$OLD_MOU[which(is.na(UPSELL_PROPENSITY$OLD_MOU))]=0
summary(UPSELL_PROPENSITY$OLD_MOU)

hist(UPSELL_PROPENSITY$OLD_MOU,100)
hist((log10(UPSELL_PROPENSITY$OLD_MOU)^2),100)
UPSELL_PROPENSITY$OLD_MOU<-ifelse(UPSELL_PROPENSITY$OLD_MOU!=0,log10(UPSELL_PROPENSITY$OLD_MOU)^2,0)
summary(UPSELL_PROPENSITY$OLD_MOU)
table(is.infinite(UPSELL_PROPENSITY$OLD_MOU))
LOGIT_MODEL_2<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_MOU,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL_2)

## Variable_5
summary(UPSELL_PROPENSITY$OLD_LOCAL_MOU)
table(is.na(UPSELL_PROPENSITY$OLD_LOCAL_MOU))
which(is.na(UPSELL_PROPENSITY$OLD_LOCAL_MOU))
UPSELL_PROPENSITY$OLD_LOCAL_MOU[which(is.na(UPSELL_PROPENSITY$OLD_LOCAL_MOU))]<-0
summary(UPSELL_PROPENSITY$OLD_LOCAL_MOU)

hist(UPSELL_PROPENSITY$OLD_LOCAL_MOU,100)
hist(log10(UPSELL_PROPENSITY$OLD_LOCAL_MOU)^2,100)
UPSELL_PROPENSITY$OLD_LOCAL_MOU<-log10(UPSELL_PROPENSITY$OLD_LOCAL_MOU)^2
summary(UPSELL_PROPENSITY$OLD_LOCAL_MOU)
table(is.infinite(UPSELL_PROPENSITY$OLD_LOCAL_MOU))
UPSELL_PROPENSITY$OLD_LOCAL_MOU[which(is.infinite(UPSELL_PROPENSITY$OLD_LOCAL_MOU))]<-0
summary(UPSELL_PROPENSITY$OLD_LOCAL_MOU)
hist(UPSELL_PROPENSITY$OLD_LOCAL_MOU,100)

LOGIT_MODEL_3<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$OLD_LOCAL_MOU,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL_3)


summary(UPSELL_PROPENSITY$OLD_STD_MOU)
hist(UPSELL_PROPENSITY$OLD_STD_MOU,100)
UPSELL_PROPENSITY$OLD_STD_MOU[which(is.na(UPSELL_PROPENSITY$OLD_STD_MOU))]<-0
hist((log10(UPSELL_PROPENSITY$OLD_STD_MOU)^.5),100)
UPSELL_PROPENSITY$OLD_STD_MOU<-log10(UPSELL_PROPENSITY$OLD_STD_MOU)
table(is.na(UPSELL_PROPENSITY$OLD_STD_MOU))
which(is.nan(UPSELL_PROPENSITY$OLD_STD_MOU))
UPSELL_PROPENSITY$OLD_STD_MOU[which(is.infinite(UPSELL_PROPENSITY$OLD_STD_MOU))]=0
which(is.na(UPSELL_PROPENSITY$OLD_STD_MOU))
table(is.na(UPSELL_PROPENSITY$OLD_STD_MOU))
summary(UPSELL_PROPENSITY$OLD_STD_MOU)

summary(UPSELL_PROPENSITY$OLD_ROAMING_MOU)
hist((log10(UPSELL_PROPENSITY$OLD_ROAMING_MOU)),100)
UPSELL_PROPENSITY$OLD_ROAMING_MOU<-log10(UPSELL_PROPENSITY$OLD_ROAMING_MOU)
summary(UPSELL_PROPENSITY$OLD_ROAMING_MOU)
table(is.nan(UPSELL_PROPENSITY$OLD_ROAMING_MOU))
UPSELL_PROPENSITY$OLD_ROAMING_MOU[which(is.nan(UPSELL_PROPENSITY$OLD_ROAMING_MOU))]<-0
UPSELL_PROPENSITY$OLD_ROAMING_MOU[which(is.infinite(UPSELL_PROPENSITY$OLD_ROAMING_MOU))]<-0


class(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)
levels(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)
sum(is.na(UPSELL_PROPENSITY))
str(UPSELL_PROPENSITY)

dim(UPSELL_PROPENSITY)
UPSELL_PROPENSITY<-na.omit(UPSELL_PROPENSITY$Old.Bill.Plan_Rental)
summary(UPSELL_PROPENSITY)
upselling_model<-glm(as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE)~UPSELL_PROPENSITY$Old.Bill.Plan_Rental+UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU+UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE+UPSELL_PROPENSITY$OLD_LOCAL_MOU+UPSELL_PROPENSITY$OLD_STD_MOU+UPSELL_PROPENSITY$OLD_ROAMING_MOU
                     ,data=UPSELL_PROPENSITY
                     ,family=binomial
                     ,control=list(maxit=50))
summary(upselling_model)

standarized Varibles

upselling_model<-glm(formula=as.factor(UPSELL_PROPENSITY$UPSELL_DOWNGRADE) ~ (UPSELL_PROPENSITY$Old.Bill.Plan_Rental+(log10(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)^2)+log10(UPSELL_PROPENSITY$OLD_DEC_ARPU)+log10(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)+(log10(UPSELL_PROPENSITY$OLD_LOCAL_MOU)^2)+(log10(UPSELL_PROPENSITY$OLD_STD_MOU)^2)+(log10(UPSELL_PROPENSITY$OLD_ROAMING_MOU)^(1/2))),data=UPSELL_PROPENSITY,family=binomial,na.action=na.omit,control=list(maxit=50))


MODEL<-lm(formula=UPSELL_PROPENSITY$NEW_THREE_MONTH_APRU~(UPSELL_PROPENSITY$Old.Bill.Plan_Rental+(log10(UPSELL_PROPENSITY$OLD_DEC_GPRS_USAGE)^2)+log10(UPSELL_PROPENSITY$OLD_DEC_ARPU)+log10(UPSELL_PROPENSITY$OLD_THREE_MONTH_APRU)+(log10(UPSELL_PROPENSITY$OLD_LOCAL_MOU)^2)+(log10(UPSELL_PROPENSITY$OLD_STD_MOU)^2)+(log10(UPSELL_PROPENSITY$OLD_ROAMING_MOU)^(1/2))),data=UPSELL_PROPENSITY,method="pearson")


## 

barotrop<- read.delim(header=T,"clipboard")
summary(barotrop)
hist((barotrop$LONGITUD),40)
hist(barotrop$LATITUDE,40)
table(barotrop$CLASS)

LOGIT_MODEL<-glm(formula=as.factor(barotrop$CLASS)~barotrop$LONGITUD+barotrop$LATITUDE,barotrop,family=binomial,control=list(maxit=50))
summary(LOGIT_MODEL)

summary(as.factor(round(predict.glm(object=LOGIT_MODEL,newdata=barotrop),0)))


