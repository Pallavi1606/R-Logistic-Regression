#Using Logistic Regression for predicting Employee Attrition

#Reading the file HR dataset for Employee Attrition
setwd("E:\\Jigsaw\\BC1\\DataSets\\Class-Datasets\\Class Datasets")
hr<-read.csv("HR dataset for Employee Attrition.csv")

#Data Exploration
dim(hr)
str(hr)
View(hr)

#Dependent variable is made into 0s and 1s
hr$AttritionTarget<-as.numeric(hr$Attrition)-1
View(hr)

table(hr$AttritionTarget)
table(hr$AttritionTarget)/nrow(hr)

#checking for NA values
colSums(is.na(hr))

#Partioning data set into test and train
sampling<-sort(sample(nrow(hr),0.7*nrow(hr)))
sampling
length(sampling)

train<-hr[sampling,]
test<-hr[-sampling,]
nrow(test)
nrow(train)

table(train$AttritionTarget)
table(test$AttritionTarget)
table(train$AttritionTarget)/nrow(train)
table(test$AttritionTarget)/nrow(test)

#PRoperly renaming Age column
colnames(train)[1]<-"Age"
View(train)
colnames(test)[1]<-"Age"
View(test)

#Checking for multicollinearity
library(corrplot)
library(car)
str(train)
dim(train)
traincor<-train[,c(1,4,6,7,9,10,11,13,14,15,17,19,20,21,24:35)]
class(traincor)
library(corrgram)
cornmix<-corrgram(traincor)
write.csv(cornmix,"lo1.csv")

#Building the model
myresult<-glm(data=train,AttritionTarget ~ Age+BusinessTravel+
                +DailyRate+Department+DistanceFromHome+Education+
                EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+
                JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+
                NumCompaniesWorked+OverTime+PercentSalaryHike+
                RelationshipSatisfaction+StandardHours+StockOptionLevel+
                TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+
                YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family=binomial)
summary(myresult)
step(myresult,direction="backward")

#Iteration 1
myresult<-glm(data=train,AttritionTarget ~ Age + BusinessTravel + DailyRate + 
           Department + DistanceFromHome + EnvironmentSatisfaction + 
           Gender + JobInvolvement + JobSatisfaction + MaritalStatus + 
           MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
           TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
           YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, 
         family = binomial)
summary(myresult)

#Iteration 2
#Creating dummies for both train and test
train$BTF<-ifelse(train$BusinessTravel=="Travel_Frequently",1,0)
test$BTF<-ifelse(test$BusinessTravel=="Travel_Frequently",1,0)

train$MS<-ifelse(train$MaritalStatus=="Single",1,0)
test$MS<-ifelse(test$MaritalStatus=="Single",1,0)

#Iteration 3
myresult<-glm(data=train,AttritionTarget ~ Age + BTF + DailyRate + 
                Department + DistanceFromHome + EnvironmentSatisfaction + 
                Gender + JobInvolvement + JobSatisfaction + MS + MonthlyIncome + NumCompaniesWorked + OverTime + RelationshipSatisfaction + 
                TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
                YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, 
              family = binomial)
summary(myresult)
step(myresult,direction = "backward")

#Iteration 4
myresult<-glm(data=train,AttritionTarget ~ EnvironmentSatisfaction + 
              JobInvolvement + JobSatisfaction + MS + MonthlyIncome + OverTime, 
              family = binomial)
summary(myresult)

#Finding Predicted Values
train$predicted<-myresult$fitted.values
train$predicted

head(train$AttritionTarget)
head(train$predicted)

#ROCR Curve
library(ROCR)
pred<-prediction(train$predicted,train$AttritionTarget)
pred
perf<-performance(pred,"acc")
perf
class(perf)
class(perf@x.values)

cutoff<-as.numeric(unlist(perf@x.values))
cutoff

acc<-as.numeric(unlist(perf@y.values))
acc

cuttoffacc<-data.frame(cutoff,acc)
cuttoffacc
View(cuttoffacc)

train$predclass<-ifelse(train$predicted>=0.4819162,1,0)

#KAPPA
library(irr)
kappa2(data.frame(train$AttritionTarget,train$predclass))

library(caret)
library(e1071)
confusionMatrix(as.factor(train$AttritionTarget),as.factor(train$predclass),positive="1")

#Roc curve
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")

#abline
abline(0,1, lty=8,col="blue")

#AUC
auc<-performance(pred,"auc")
auc

#Gains chart
library(gains)
gains(as.numeric(train$AttritionTarget),as.numeric(train$predicted),groups=10)
quantile(train$predicted,seq(0,1,0.1))

targeted <- which(train$predicted >= 0.400835934)

targeted

test$pred <- predict(myresult, type = "response",newdata = test)
View(test)
