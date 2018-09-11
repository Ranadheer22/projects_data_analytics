
library(readxl)
library(caTools)#splitting
library(e1071) #used for svm
library(caret)
telc <- read_excel("G:/Imarticus/R/R_Datasets/Telecom Churn data.xlsx")
View(telc)
summary(telc)

#eda check missing values
colSums(is.na(telc))
#convert categorical var into factors
telc$`Int'l Plan`<-as.factor(telc$`Int'l Plan`)
telc$`VMail Plan`<-as.factor(telc$`VMail Plan`)
telc$`Churn?`<-as.factor(telc$`Churn?`)
#remove irrelevant val
telc<-telc[,-4]
View(telc)
summary(telc)
#scaling
telc[,-c(1,3,4,5,6,20)]<-scale(telc[,-c(1,3,4,5,6,20)])
#outliers
boxplot(telc[,-c(1,3,4,5,6,20)])
#splitting
split<-sample.split(telc$`Churn?`,0.8)
Train1<-subset(telc,split==T)
Test1<-subset(telc,split==F)
#feature engr
Mins<-c(telc$`Day Mins`,telc$`Eve Mins`,telc$`Night Mins`,telc$`Intl Mins`)
Calls<-c(telc$`Day Calls`,telc$`Eve Calls`,telc$`Night Calls`,telc$`Intl Calls`)
Charges<-c(telc$`Day Charge`,telc$`Eve Charge`,telc$`Night Charge`,telc$`Intl Charge`)
telc<-telc[,-c(7:18)]
telc<-data.frame(Mins,Calls,Charges,telc)
View(telc)
#model
mod1<-glm(`Churn?`~.,family = binomial(link='logit'),data = Train1)
summary(mod1)
mod2<-glm(`Churn?`~`Int'l Plan`+`VMail Plan`+`VMail Message`+`Intl Calls`+`CustServ Calls`,family = binomial(link='logit'),data=Train1)
summary(mod2)
#validation

predtelc<-predict(mod2,newdata = Test1)
summary(predtelc)
#tuning based on grid search
tunesvm=tune(svm,`Churn?`~.,data= Train1,ranges=list(gamma=2^(-1:1),cost=2^(2:9)))
summary(tunesvm)

#classifier models

classifier5<-svm(formula=`Churn?`~`Int'l Plan`+`VMail Plan`+`VMail Message`+`Intl Calls`+`CustServ Calls`,data = Train1,type='C-classification',gamma=0.5,cost=4)
summary(classifier5)#830
classifier6<-svm(formula=`Churn?`~`Int'l Plan`+`VMail Plan`+`VMail Message`+`Intl Calls`+`CustServ Calls`,data = Train1,type='C-classification',gamma=0.5,cost=4,kernel='linear')
summary(classifier6)#792
classifier7<-svm(formula=`Churn?`~`Int'l Plan`+`VMail Plan`+`VMail Message`+`Intl Calls`+`CustServ Calls`,data = Train1,type='C-classification',gamma=0.5,cost=4,kernel='sigmoid')
summary(classifier7)#630
classifier8<-svm(formula=`Churn?`~`Int'l Plan`+`VMail Plan`+`VMail Message`+`Intl Calls`+`CustServ Calls`,data = Train1,type='C-classification',gamma=0.5,cost=4,kernel='polynomial')
summary(classifier8)#764
#predictions
pred_telc<-Train1[,-20]
pred5<-predict(classifier5,pred_telc)
summary(pred5)
pred6<-predict(classifier6,pred_telc)
summary(pred6)
pred7<-predict(classifier7,pred_telc)
summary(pred7)
pred8<-predict(classifier8,pred_telc)
summary(pred8)
#Cofusion matrix
cf5<-table(pred5,Train1[,20])
cf5
cf6<-table(pred6,Train1[,20])
cf6
cf7<-table(pred7,Train1[,20])
cf7

cf8<-table(pred7,Train1[,20])
cf8
cf9<-table(pred7,Train1[,20])
cf9


