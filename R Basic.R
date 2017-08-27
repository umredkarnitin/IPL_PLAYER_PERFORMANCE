library()

##Assigning
assign("x", c(10.4, 5.6, 3.1, 6.4, 21.7))
or 
x=c(10.4, 5.6, 3.1, 6.4, 21.7)

seq(0,9,3)
m<-matrix(data=seq(from=1,to=4,by=1),nrow=2,ncol=2)
dim(m)
#create data frame
letters[1:6]
data.frame (L= LETTERS[1:4] , x=4)

letters

##(b) concatenation function

cat(0,2,4)

scan()

## Data Importing & exporting
objects()
class(objects())

"1"<- c(2,3,4,5)
objects()
"1"[3]

rm(url,tol)## to remove the variables
objects()

rm()
objects()

?rm

ls()
rm(list=ls()) ## Remove the list of all varibale in current session

## Shorter vector in the expression are recycled as often as need be until they match the lengh of the logest vector

x=c(2,5,3,1,1,2)
y=c(2,3)
z=c(1,2)

y+z

x+z

??y
install.package("RCPP")

##HDFS <-> R (use Language Interpreter)

?log

##LOG FUNCTION
log(exp(1), base = exp(1))

log(10, base = 10)

log1p(x)

##eg.1.  y = loga x <=> ay = x (a; x > 0; a != 1)

2^3 = 8

log(8,base=2)

##loga 1 = 0

log(1, base=exp(1))

##loga a = 1

log(6,base=6)

##loga(mn) = logam + loga n

log(x=8,base=2)
log(2,base=2)+log(4,base=2)

##logam/n = logam -loga n

log(x=2,base=2)
log(10,base=2)-log(5,base=2)

##logamn = n  logam

log(x=2^6,base=2)

6*log(x=2,base=2)

##logam = logbm  loga b

log(x=3,base=10)

log(x=3, base=exp(1))*log(x=exp(1), base=10)


##logam =logbm/logb a
log(x=3,base=exp(1))

log(x=3, base=10)/log(x=exp(1), base=10)


##loga b =a/logb a

log(x=3,base=10)

10/log(x=10, base=3)


##loga x =ln a/ln x

log(10,2)

logb(10)/logb(2)



sin(pi/4)

sin((22/7)/4)

pi

pi

tan(22/(7*4))

log(2.8)

logb(x=2,2)


log1p(2)

exp(1)

log(exp(1))

log(exp(-2))





## List
a<- 1:50
cell<-c(1,2,3,4)
y<- matrix(cell, nrow=2, byrow=TRUE)

w<- list( name="Fred",mynumbers= a, mymatrix=y,age=5.3)
w


## how to unlist with feild names
unlist(w)

## Dataframes

unlist(w$name)

unlist(w$mynumbers)

unlist(w$mymatrix)

unlist(w$age)


## vectors

a<- c(1,2,5.3,6,-2.4)
b<- c("one", "two", "three")
c<-c(TRUE,TRUE,TRUE,FALSE, TRUE,FALSE)

a[2]

a[4]

##Matrix

cells<- c(1,2,3,4)
rnames <- c("R1", "R2")
cnames<- c("C1", "C2")

mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames,cnames))


#Data Frame

d<-c(1,2,3,4)

e<-c("red", "white", "red", NA)

f<- c(TRUE, TRUE, TRUE, FALSE)

mydata <- data.frame(d,e,f)
names(mydata)
names(mydata)<- c("ID", "COLOR", "Passed") ## Varible name
names(mydata)
names(mydata[1])<- c("ID")

mydata$ID <- as.character(mydata$ID)

mydata
class(mydata$ID)
unlist(mydata)
list(mydata)



## Subsetting

x[-3]
## Range
x[1:3]
## Slicing includeing only part of the object
x[c(1,2,5)]
## select element  based on logical operators
x[x>3]
y <- x>3



p<- c(1,2,3,NA,5)
p<- data.frame(1,2,3,NA,5)


p<- c(1,2,3,NA,5)
is.na(p)

.Random.seed
..getNamespace(p)
.__C__environment


## Creating data files into R

Bank<-data.frame(Age=numeric(0), Gender=character(0),
                 Tran_Type=character(0), Amount=numeric(0))

Bank<- edit(Bank)## A data frame will open after this command

fix(Bank)
str(Bank)

Bank
print(Bank)

rm(Bank)

Bank2<-data.frame(Age=numeric(0), Gender=character(length=0L),
                 Tran_Type=character(0), Amount=numeric(0))

Bank2<- edit(Bank2)## A data frame will open after this command

Bank2$Gender<- as.character(Bank2$Gender)
str(Bank2)
fix(Bank2)

?character

class(Bank$Age)


##Factor:
##Ordinal Data & Nominal Data


df<- factor(c(1,2,2,2,3,3,3,1), levels=c("Low","Mid","High"))
df

df1<- factor(c(1,2,2,2,3,3,3,1), levels=c(1,2))
df1

df2<- factor(c(1,2,2,2,3,3,3,1), levels=c("1","2","3"))
df2

df<- as.character(df)
is.character(df)

df<-as.factor(df)
df
is.factor(df)

df<-as.numeric(df)
is.numeric(df)


## Saving data files in R-1

##a)filetype ".rdata"
save.image("F:/Data Scientist/Data.Rdata")  ## for RStudio: Complete R environment will be stored in this file
save.image("F:/Data Scientist/Data.rda")  ## for R only: Complete R environment will be stored in this file

##b) in csv.file
write.table(Bank,"F:/Data Scientist/Bank.csv")
write.table(Bank,"F:/Data Scientist/Bank.csv", sep="~")

write.table(Bank,"F:/Data Scientist/Bank.csv",col.names=TRUE, sep="~")


write.table(Bank,"F:/Data Scientist/Bank.txt", sep="~")

## store the copied  data in the data set
cells.data<- read.delim("clipboard")

fabric_data<- read.delim("clipboard")


mcars<- read.delim("clipboard")
str(mcars)

two_sample_t_test<-read.delim("clipboard")


table(mcars$Cylinders)
mcars$Cylinders<- as.factor(mcars$Cylinders, levels=c("3CYL","4CYL","5CYL","6CYL","8CYL"))



## 

## mtcar
str(mtcars)
data()
mtcars$wt
attach(mtcars)

wt
vs

detach(mtcars)

wt

##Data files in R
## list the objects in the working environment
ls()

## list the variables in mtcars
names(mtcars)

## list the structure of the mtcars
str(mtcars)

## list levels of factor v1 in mtcars
levels(mtcars$gear)

dim(mtcars)


class(mtcars)

head(mtcars,2)

tail(mtcars,3)

mtcars[1:2,]
mtcars[0,]

View(mtcars)

colnames(mtcars)


cut(x=10,breaks=2)

v1<- factor(c(1,2,3,1,2,3,2,2,1,3,3), levels=c("1","2","3"), labels=c("red","blue","green"))

v1

v2<-ordered(c(1,2,3,1,2,3,2,2,1,3,3),levels=c(1,2,3),labels=c("Low","Med","Hig"))

v2



##Day2

##Missing Value treatment
x1<-c(1,2,3,4,5,6,NA)

is.na(x1)

sum(is.na(x1))


mydata
mydata$COLOR[is.na(mydata$COLOR)==TRUE]<-"red"

is.na(mydata)

x1[is.na(x1)==TRUE]<-99

y1<-is.na(x1)

x1[!y1]


## Use na.rm to remove the NA value
sum(x1,na.rm=TRUE)

mean(x1,na.rm=TRUE)

sd(x1,na.rm=TRUE)

var(x1,na.rm=TRUE)

mydata$ID[1]=NA

sum(as.numeric(mydata$ID), na.rm=TRUE)

##Create new dataset without missing data

newdata<- na.omit(mydata)

mydata[complete.cases(mydata),]

mydata[!complete.cases(mydata),]

complete.cases()


##Manipulation & exploration
attach(mtcars)

str(mtcars)

## Ascending
mtcars[order(mpg),]

##Decending
mtcars[order(-mpg,cyl),]

mtcars[order(mpg,-cyl),]

mtcars[order(mpg,gear),]

detach(mtcars)


## Subsetting the data
str(iris)
dim(iris)
sum(is.na(iris))

myvars= c("Sepal.Length","Sepal.Width","Petal.Length")

myvars[1]

newdata<- iris[myvars]

newdata<- iris[,2:4]

newdata<- paste(iris,2:4,sep="")

str(iris)

## Subsetting
x<- data.frame(v1=1:5,v2=c(12,20,30,45,15),v3=c(0,1,0,1,1),v4=c("F","T","T","F","T"))

myvars<- paste("v",1:3,sep="")


## for a1_b, a2_b 
?paste
myvars<- paste("a",1:3,"_b",sep="")

myvars<- paste("a_b",1:3,sep="")


myvars<- cat(paste("a_b",1:3,sep="_Sale"),paste("",1:3,sep="_Revenue"))

paste(month.abb, letters,sep="")

## If you pass several vectors to paste0, they are concatenated in a
## vectorized way.
(nth <- paste0(1:10, c("st", "nd", "rd", rep("th",7))))

nnth<-cat(paste0(1:10, c("st", "nd", "rd", rep("th",7))),paste0(11:20, c(rep("th",10))),paste0(21:100, c("st", "nd", "rd", rep("th",7))))

names(nnth)

## select 1st and 5th thru 10th variables
y=c(2,4,6,7,8,9,10,12,14,16,18,20,21,22,26,28,27,29,45,21,25,26,7)
y[c(1,5:10)]

seq(1,10,1)

seq(1,10,2)
y[c(1,seq(2,10,3))]


typeof(y[c(1,5:10)])
class(y[c(1,5:10)])


newdata<-x[myvars]

newdata<-x[!myvars]## NOT APPLICABLE

newvars<- names(x) %in% c("v1","v3","v4")

newdata<-x[newvars]

newdata<-x[!newvars]


x = data.frame(v1 = 1:5, v2=c(12,20,30,45,15), v3=c(0,1,0,1,1), v4=c("F","T","T","F","T"))
dim(x)
# exclude 1st and 3rd variable
x[c(-1,-3)]  

x[c(-1,-3)]  
dim(x)
## delete variables v2 and v4 from x
dim(x)
x$v2 <-x$v4 <-NULL
x["v1"]
class(x["v1"])

class(x[[1]])


x[1]

class(x[1])

x$v1[2]
x$v1
x[,1]
x[,1][2]
class(x[,1])

dim(x)

##Selecting Observations and Selection using the subset function
mydata<-data.frame(supply=c(120,140,210,100,90),demand=c(90,150,200,80,60));

attach(mydata)

d= mydata[1:3,1:2]

d1<-subset(x=mydata,subset=demand>100)
d1<-subset(x=mydata,subset=demand>100 & demand<200)
d1<-subset(x=mydata,subset=demand>100 & demand<200 |supply>100)

d2<-transform(mydata,lack=log10((supply-demand)+1))
d2
detach(mydata)


## Mergeing the data
x =data.frame(k1 = c(NA,NA,3,4,5), k2=c(1,NA, NA,4,5), data=1:5); 
x
y = data.frame(k1 = c(NA,2,NA,4,5), k2=c(NA,NA,3,4,5), data=1:5); 
y

## Inner Join of x & y (Overallppping rows)
merge(x,y, by=c("k2"))

## left  Join for x & y ()
merge(x,y, by=c("k2"),all.x=TRUE)

## left  Join for x & y ()
merge(x,y, by=c("k2"),all.y=TRUE)

## Full Join for x & y
merge(x,y, by=c("k2"),all=TRUE)


merge(x,y, by=c("k2"),incomparables=NA)


## Adding Rowas
T<- rbind(x,y)
T

v1<-c(1,2,3)
v2<-c(4,5,6)

c(v1,v2)

rbind(v1,v2)

cbind(v1,v2)

data.frame(v1,v2)

a<- matrix(nrow=3,ncol=3,1:6)
a<- matrix(1:200,10)
a
a<- matrix(1:200,,10)
a

as.vector(a)

as.data.frame(a)


pi

exp(1)

abs(-2132.1)

sqrt(3)

signif(2.54353,2)

substr(x,1,3)


sub(pattern="~",replacement=".","I~AM YOU")
sub(pattern="\\s",replacement=".","Hello There")
??sub
toupper("Hello There")


myfirststring<-"We 10 student are in Data Science class, studying cocepts of data science which invovles : statistics , computer skills, domain knowledge"

length(myfirststring)
length(strsplit(myfirststring," "))
class(splitedstring)

splitedstring<- as.vector(strsplit(myfirststring," "))
unlist(splitedstring)
length(unlist(splitedstring))



getwd()

setwd("F:/Data Scientist")
getwd()

dir()


match(1:10,c(7,6,2))

unique(1:10)


myname<-"Nitin Wasudeo Umredkar"


library(MASS)
dim(Animals)
str(Animals)
sum(is.na(Animals))
names(Animals)<-c("Body_wt","Brain_Wt")



table(Animals)


sort(Animals$Brain_Wt,decreasing=FALSE)

Animals[order(Animals$Brain_Wt),]

MIN_BRAIN_WEIGHT <- head(Animals[order(Animals$Brain_Wt),],1)

rownames(MIN_BRAIN_WEIGHT)

subset(Animals,subset=Animals$Body_wt>60)


?c

## Functions

A.sum<- function(arg1, arg2)
{
  b<- arg1+arg2
  return(b)
}

A.sum(5454,322)

f1<- function(x,y)
{  z1<- 2*x + y
  z2<- x + 2*y
  z3<-2*x + 2*y
  z4<- x/y
  z4
}

f1(1,2)
print(f1(1,2))
print(f1(1,2))
print(z4)


f2<- function(x,y)
{
  z1<- 2*x + y
  z2<- x + 2*y
  z3<-2*x + 2*y
  z4<- x/y
  return(c(z1,z2,z3,z4))
}

f2(2,4)
print(f2(2,4))

rm(list=ls())

1) What value will print (function f1(1,2)) return?
0.5
2) what value f2(1,2) will retun?

3) which is good way of writing functions f1 or f2?
f2 since it returns the value & make user ware what operation happened


if()
{}
else{}

## Example
a<- c(10,21,22,54,36)
if(sum(a)>100)
{
  b=1+1
}
b

if(length(a)<=4)
{
  g=a+1
}
g


if((sum(a)+20)>150) 
{
  p=a*10
  q=a*20
}
else
{
  r=a+10
}

p
q
r



if((sum(a)+20)>200) 
{
  pk=a*10
  q=a*20
}

else if{}
else if{}

##while loop

while()
{
  
}

a=1
b=10
while(b>1)
{
  a=a+2
  b=b-1
  print(a,b)
}
a


## for loop
a<-rnorm(10)
for(i in 1:(length(a)-5))
{
  print(i+"---"+a[i])
}
class(a)

a[1:5]

a<-rnorm(10)
for(i in 6:(length(a)))
{
  ##cat("element no ",i,"\t")
  ##cat("value is -> ",a[i],"\n")
  paste(a[i],"is the ",i , "th value of the a")
}

?paste

paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)

for(n in c(2,5,10,20,50))
{
  x <- rnorm (n)
  cat(n, ": ", sum(x^2), sep = "")
}


f <-factor(sample(letters[1:5], 10, replace = TRUE))
for(i in unique(f))
print(i)


## Apply functions

m<- matrix(rnorm(10),5,2)
m
m<- matrix(1:6,2)
m

apply(m,1,mean)

class(apply(m,1,mean))

apply(m,2,mean)


lapply(X=m,sqrt)
sapply(X=m,sqrt)

class(lapply(1:10,mean))
class(lapply(1:10,sqrt))

class(lapply)

class(sapply(1:10,mean))
class(sapply(1:10,sqrt))

sapply(X=m,sqrt)

sapply(X=m,sqrt)


mode(m)

d<-c(2,1,2,3,1,2,3,4,1,5,5,3,2,3,3,5,5,5,5)
e<-c("a","b","a","c","a")

max(table(d))

max(table(d))

mode.function<- function(d){
unique.d<-unique(d)
num1<-c()
for(i in 1:length(unique.d)){
  num1[i]<-sum(d==unique.d[i])
}
return (unique.d[num1==max(num1)])
}

mode.function(d)
mode.function(e)

    xx<-data.frame(x1=c(1,2,3,4),x2=c(5,6,7,8),x3=c(9,10,11,12))
    xx[xx$x1>2,]

xx[,2:3]

paste(names(xx),sep=",")    

xx[,paste(names(xx),sep=",")]    

    x1<-c("x2","x3")

xx[,names(xx) %in% x1]


## Read external Files
read.csv(file=,header=,sep=, quote=, dec=,fill=,comment.char=)
install.packages("rJava")
library("rJava")
install.packages("xlsx")
library("xlsx")

install.packages("Hmisc")
library("Hmisc")


## Reading xlsx file using xlsx package
?read.xlsx
xlsx::read.xlsx(file="F:/Data Scientist/DataDictionary.xlsx",sheetName="LoanStats",sheetIndex=1,header=TRUE)

xls_data<-xlsx::read.xlsx2(file="F:/Data Scientist/DataDictionary.xlsx",sheetName="LoanStats",sheetIndex=1,header=TRUE)
str(xls_data)

Sample_data_analysis<-read.xlsx2(file="F:/Coursera/Business Analyst/Six SIgma/Black belt/Exercise Data Files/Excel Data Files/Minitab Data Files v2.xls",sheetName="Measure",header=T)
str(Sample_data_analysis)
head(Sample_data_analysis)

Sample_data_analy_phase<-read.xlsx2(file="F:/Coursera/Business Analyst/Six SIgma/Black belt/Exercise Data Files/Excel Data Files/Minitab Data Files v2.xls",sheetName="Analyze",header=T)
str(Sample_data_analy_phase)

Sample_data_analy_phase$Fabric_length<- as.numeric(Sample_data_analy_phase$Fabric_length)
summary(Sample_data_analy_phase$Fabric_length)
class(Sample_data_analy_phase$Fabric_length)
summary(as.numeric(Sample_data_analy_phase$Fabric_length))

## Reading SPSS Data file using foreign package
install.packages("foreign")
library("foreign")

foreign::read.spss(file="E:/Multivariant ANalysis/SAS Discovering_Statistics (V Good)/sas_datafiles/HBAT.sav",to.data.frame=T,trim.factor.names=T,use.missings=T)

foreign::()
## Read large files


##read csv Takes llot of time in order to fetch
read.csv(file="F:/ALL SAS CODES/BSNL/bsnl_core_profile_final_v1.csv", header=TRUE)

##  using fread function of data.table package for  reading large csv txt file
?fread

install.packages("data.table")
library("data.table")
help(data.table)

bsnl_profile_data<-data.table::fread("F:/ALL SAS CODES/BSNL/bsnl_core_profile_final_v1.csv", sep="|",header=TRUE)

test.data.table()


