#Predicting the probability of heart attack for a patient
#Reading the file Patient Data
setwd("E:\\Jigsaw\\BC1\\DataSets\\Class-Datasets\\Class Datasets")
patient <- read.csv("Patient Data.csv", header = T, stringsAsFactors = F)

#Data Exploration

dim(patient)
str(patient)
summary(patient)
View(patient)


#Dependent variable is already in numeric type


#Ratio of non-risk and risk of heart failure
#Frequency distribution of heart failure

table(patient$HEARTFAILURE)

#percentage of risk and non-risk of heart failure
table(patient$HEARTFAILURE)/nrow(patient)


#checking for missing values 
#First method:

colSums(is.na(patient))

#second method:

summary(patient)

###### Therefore no missing  values #########

# Partition the dataset into training and validation dataset

sampling<-sort(sample(nrow(patient), nrow(patient)*.7))

length(sampling)

#Creating training and test dataset using index number 

train <- patient[sampling,]

#creating test or validation dataset by dropping index  numbers

test <- patient[-sampling,]

nrow(train)
nrow(test)

# Checking the frequency Distribution of the target variable 

table(train$HEARTFAILURE)
table(train$HEARTFAILURE)/7559# checking yes or no percent
table(test$HEARTFAILURE)/3241


#Finding correlation between numeric variables
library(corrplot)

str(train)
traincorrelation <-cor(train[,c(1,2,3,4,5,6,10)])

cormat<-corrgram(traincorrelation)

write.csv(cormat,"patientcorrelation.csv")

#no independent variable are correlated

colnames(train)

patientreg <- glm(data = train, HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY+
                    CHOLESTEROL + BMI + AGE +SEX + FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK,
                  family=binomial)

summary(patientreg)

step(patientreg,direction="backward")


patientreg <- glm(data = train,HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
                    BMI + SEX + FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK, 
                  family = binomial )

summary(patientreg)

patientreg <-  glm(data = train,HEARTFAILURE ~ AVGHEARTBEATSPERMIN + PALPITATIONSPERDAY + 
                     BMI  + FAMILYHISTORY + SMOKERLAST5YRS + EXERCISEMINPERWEEK, 
                   family = binomial )

summary(patientreg)


#Finding predicted values

train$predict <- patientreg$fitted.values
train$predict

#comparing with actual and predicted values

head(train$HEARTFAILURE)

head(train$predict)



# Let us convert the probabilities also into Good/Bad response(Risk or non-risk)
# based on a cut-off probability
library(ROCR)

pred<-prediction(train$predict,train$HEARTFAILURE)

class(pred)

pref <- performance(pred,"acc")

class(pref)

pref


# x values contain the cut-off probabilities whereas y contain accuracy

#use @ to access the slots

class(pref@x.values)
cutoffprob <- as.numeric(unlist(pref@x.values))

cutoffprob

class(pref@y.values)
accuracies <- as.numeric(unlist(pref@y.values))

#After extracting from list creating dataframe for x and y values

cutoffs <- data.frame(cutoffprob, accuracies )

# Displaying in decreasing order of accuracy

cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

#choosong the highest probability cutoff from high accuracy

train$predclass1 <- ifelse(train$predict>0.4959027,1,0)


# Kappa values and Confusion Matrix from caret package
#install.packages("caret")
#install.packages("irr")
#install.packages("e1071")
library(caret)
library(irr)
kappa2(data.frame(train$HEARTFAILURE,train$predclass1))

confusionMatrix(as.factor(train$HEARTFAILURE),as.factor(train$predclass1), positive = "1")

?performance


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

pref<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(pref,col="red")


# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred,"auc")
auc



#Creating a Gains chart to know how many values can be captured


library(gains)

#in first group top 10 % no of ppl who r riskier is 66.5% 
#to pick up the top 30 % no of 1's i will check with last three percentage in quantile or decile

gains(as.numeric(train$HEARTFAILURE),train$predict, groups =10)
quantile(train$predict, seq(0,1,0.1))#.1,.2.....,.9,1(decile)

targeted <- which(train$predict >= 0.15312246)
targeted

#obtaining model for validation dataset using predict() function

test$pred <- predict(patientreg, type = "response",newdata = test)

# Let us convert the probabilities also into Good/Bad response(Risk or non-risk)
# based on a cut-off probability
library(ROCR)


pred1<-prediction(test$pred,test$HEARTFAILURE)

class(pred1)

pref <- performance(pred1,"acc")

class(pref)

pref


# x values contain the cut-off probabilities whereas y contain accuracy

#use @ to access the slots

class(pref@x.values)
cutoffprob1 <- as.numeric(unlist(pref@x.values))

cutoffprob1

class(pref@y.values)
accuracies1 <- as.numeric(unlist(pref@y.values))

#After extracting from list creating dataframe for x and y values

cutoffs1 <- data.frame(cutoffprob1, accuracies1 )

# Displaying in decreasing order of accuracy

cutoffs1 <- cutoffs1[order(cutoffs1$accuracies1, decreasing=TRUE),]

#choosong the highest probability cutoff from high accuracy

test$predclass2<- ifelse(test$pred>0.5077362,1,0)


# Kappa values and Confusion Matrix from caret package
#install.packages("caret")
#install.packages("irr")
#install.packages("e1071")
library(caret)
library(irr)
kappa2(data.frame(test$HEARTFAILURE,test$predclass2))

confusionMatrix(as.factor(test$HEARTFAILURE),as.factor(test$predclass2), positive = "1")

?performance


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)

pref<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(pref,col="red")


# ROC curve should be higher than the AB line

abline(0,1, lty = 8, col = "blue")


# Area under the curve should be more than 50%

auc<-performance(pred1,"auc")
auc



#Creating a Gains chart to know how many values can be captured


library(gains)

#in first group top 10 % no of ppl who r riskier is 66.5% 
#to pick up the top 30 % no of 1's i will check with last three percentage in quantile or decile

gains(as.numeric(test$HEARTFAILURE),test$pred, groups =10)
quantile(train$predict, seq(0,1,0.1))#.1,.2.....,.9,1(decile)

targeted <- which(test$pred >= 0.15385604)
targeted


