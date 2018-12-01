require(gains)
require(ROCR)
require(caret)
require(smbinning)
require(ggplot2)
require(stringr)
require(lubridate)
require(data.table)
library(MASS)
library(rpart)
library(randomForest)
require(caret)
library(pROC)
library(e1071)
library(mice)

install.packages("mice")
# install.packages("H2o")

##set working directory

setwd("E:\\Analytics Vidhya\\Loan Prediction Problem")

##Read Train and Test Data

TrainD<-fread("train.csv",stringsAsFactors = T)

TestD<-fread("test.csv",stringsAsFactors = T)

levels(TestD$Dependents)[levels(TestD$Dependents) ==  "3+"]<-"3" ##Reassign Level

TestD$Dependents<-as.numeric(TestD$Dependents)


TestD<-mice(TestD,m=5)

head(TestD)

##Check Train data

str(TestD)
summary(TrainD)

##Impute missing values

TrainD[,LoanAmount:=ifelse(is.na(LoanAmount)==TRUE,mean(LoanAmount,na.rm=T),LoanAmount)]
TrainD[,Loan_Amount_Term:=ifelse(is.na(Loan_Amount_Term)==TRUE,mean(Loan_Amount_Term,na.rm=T),Loan_Amount_Term)]
TrainD[,Credit_History:=ifelse(is.na(Credit_History)==TRUE,mean(Credit_History,na.rm=T),Credit_History)]

TestD[,LoanAmount:=ifelse(is.na(LoanAmount)==TRUE,mean(LoanAmount,na.rm=T),LoanAmount)]
TestD[,Loan_Amount_Term:=ifelse(is.na(Loan_Amount_Term)==TRUE,mean(Loan_Amount_Term,na.rm=T),Loan_Amount_Term)]
TestD[,Credit_History:=ifelse(is.na(Credit_History)==TRUE,mean(Credit_History,na.rm=T),Credit_History)]


TrainD<-TrainD[Married!=""] ##Remove missing in Married

levels(TrainD$Dependents)[levels(TrainD$Dependents) ==  "3+"]<-"3" ##Reassign Level

TrainD$Dependents<-as.numeric(TrainD$Dependents) ##Convert Dependants to Numeric

##Impute Dependants by median of the group

TrainD[Dependents=="",Dependents:=median(Dependents),by=.(Gender,Married,Education,Self_Employed)]

TrainD[Self_Employed=="",Self_Employed:="Unknown"] ##impute Unknwon for missing in self Emplyed

TrainD[Gender=="",Gender:="Unknown"]  ##impute Unknwon for missing in Gender

TrainD$Loan_Status<-ifelse(TrainD$Loan_Status=='Y',1,0)
str(TrainD)
# TrainD$LoanAmount<-ifelse(is.na(TrainD$LoanAmount)==TRUE,mean(TrainD$LoanAmount,na.rm=T),TrainD$LoanAmount)
# TrainD$Loan_Amount_Term<-ifelse(is.na(TrainD$Loan_Amount_Term)==T,mean(TrainD$Loan_Amount_Term,na.rm=T),TrainD$Loan_Amount_Term)
# TrainD$Credit_History<-ifelse(is.na(TrainD$Credit_History)==T,median(TrainD$Credit_History,na.rm = T),TrainD$Credit_History)

##TrainD[,Gender:Loan_Status,with = FALSE]

# ##Build RandomForest Model



set.seed(131)

mod_build_forest<-randomForest(Loan_Status~.,data=TrainD[,Gender:UrbanProp,with = FALSE])

prediction<-predict(mod_build_forest)

pred1<-as.numeric(predict(mod_build_forest,type="class"))

predict.rforest <- as.data.frame(predict(mod_build_forest, TestD))

Rf_sub<-data.frame(Loan_ID=TestD$Loan_ID,Loan_Status=predict.rforest$predict)

write.csv(Rf_sub,file="submission4.csv")

##Logistic Regression Model

model1<-glm(Loan_Status~.,data=TrainD[,Gender:Loan_Status,with = FALSE],family = "binomial")

##Create dummy Variabbles for Train and Test with significant P values

TrainD[,Ismarried:=ifelse(Married=="Yes",1,0)]

TrainD[,semiUrbanProp:=ifelse(Property_Area=="Semiurban",1,0)]

TestD[,Ismarried:=ifelse(Married=="Yes",1,0)]

TestD[,semiUrbanProp:=ifelse(Property_Area=="Semiurban",1,0)]

##New Model

model2<-glm(Loan_Status~Credit_History+Ismarried+UrbanProp,data=TrainD[,Gender:UrbanProp,with = FALSE],family = "binomial")

predTrain<-predict(model2,type="response")

predTrain1<-predict(model2,TrainD,type="response")

table(TrainD$Loan_Status, predTrain1 > 0.5)

pred<-ifelse(predTrain1>=0.5,1,2)

confusionMatrix(pred,TrainD$Loan_Status)

TestPred<-predict(model2,newdata = TestD,type="response")

TestD$Loan_Staus<-ifelse(TestPred>=0.5,"Y","N")

TestD$Credit_History<-ifelse(is.na(TestD$Credit_History)==T,1,TestD$Credit_History)

ResponseData<-data.table(TestD$Loan_ID,TestD$Loan_Staus)

write.csv(ResponseData,file="submission2.csv")

str(TrainD$Credit_History)

summary(TestD$Credit_History)
######## GBM Model #########################################################

# install.packages("h2o", type="source", 
#                  repos="http://h2o-release.s3.amazonaws.com/h2o/rel-turchin/3/R")
library(gbm)
traind1<-TrainD[,Credit_History:semiUrbanProp,with=FALSE]

traind1<-traind1[,Property_Area:=NULL]


gbm.fit<-gbm(Loan_Status~.,
             data=traind1,
             distribution = "bernoulli",
             n.trees=500,
             shrinkage=0.005,
             interaction.depth=3,
             bag.fraction = 0.5,
             n.minobsinnode = 10,
             cv.folds = 5,
             verbose=TRUE
             )

summary(gbm.fit)

ntrees<-gbm.perf(gbm.fit)

print(ntrees)

TrainD$PredictedLS<-predict(gbm.fit,data=TrainD,ntrees=ntrees,type="response")
TrainD$PredictedLS<-ifelse(TrainD$PredictedLS>0.3,1,0)

table(TrainD$Loan_Status,TrainD$PredictedLS)

TestD$Loan_Status<-predict(gbm.fit,data=TestD,ntrees=ntrees,type="response")

TestD$Loan_Status<-ifelse(TestD$Loan_Status>0.3,"Y","N")

ResponseData<-data.table(Loan_ID=TestD$Loan_ID,Loan_Status=TestD$Loan_Status)

write.csv(ResponseData,file="submission5.csv")

##############Model using Caret GBM ##################################################################################


Cgbmdata<-TrainD[,Gender:Loan_Status,with=FALSE]

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)


objModel <- train(Loan_Status~.,data=Cgbmdata,
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)

print(objModel)

predictionsTrain <- predict(object=objModel, TrainD, type='raw')

predictions <- predict(object=objModel, TestD, type='raw')
head(predictions)

print(postResample(pred=predictionsTrain, obs=as.factor(TrainD$Loan_Status)))


?predict

library(pROC)
predictions <- predict(object=objModel, TrainD, type='prob')[,2]

predictionsTest <- predict(object=objModel, TestD, type='prob')[,2]
predictionsTestRaw <- predict(object=objModel, TestD, type='raw')

class(predictionsTest)

class(TestD$Loan_Status)

TestD$Loan_Status<-predictionsTest

TestD$Loan_Status<-ifelse(TestD$Loan_Status>0.5,"Y","N")

auc <- roc(ifelse(TestD$Loan_Status=="Y",1,0), predictionsTest[[2]])
print(auc$auc)


ResponseData<-data.table(Loan_ID=TestD$Loan_ID,Loan_Status=TestD$Loan_Status)

write.csv(ResponseData,file="submission6.csv")


NROW(TestD[complete.cases(TestD)==T])
