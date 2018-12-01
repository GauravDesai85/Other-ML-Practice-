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
library(dummies)
library(gbm)

##install.packages("dummies")

setwd("E:\\Analytics Vidhya\\The Smart Recruit")


Traind<-fread("Train.csv",stringsAsFactors = T)

Testd<-fread("Test.csv",stringsAsFactors = T)



head(Traind)
dim(Traind)
str(Traind)
summary(Traind)

head(Testd)
dim(Testd)
str(Testd)
summary(Testd)

##Combine Train and Test
##Create Target variable for Test so that Train and Test have equal columns to bind

Testd[,Business_Sourced:=median(Traind$Business_Sourced,na.rm=T)]
com<-list(Traind,Testd)

combin<-rbindlist(com)

##combin$Business_Sourced<-as.factor(as.character(combin$Business_Sourced))
str(combin)
summary(combin)
###Univariate Analysis###

combin[,prop.table(table(Applicant_Gender))]

combin[,prop.table(table(Applicant_Marital_Status))]

combin[,prop.table(table(Applicant_Occupation))]

combin[,prop.table(table(Applicant_Qualification))]

###Imputing Missing Values using Mode##

colSums(is.na(combin))

##function to calc mode of columns##
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

combin[,Manager_Grade:=ifelse(is.na(Manager_Grade)==T,getmode(Manager_Grade),Manager_Grade)]
combin[,Manager_Num_Application:=ifelse(is.na(Manager_Num_Application)==T,getmode(Manager_Num_Application),Manager_Num_Application)]
combin[,Manager_Num_Coded:=ifelse(is.na(Manager_Num_Coded)==T,getmode(Manager_Num_Coded),Manager_Num_Coded)]
combin[,Manager_Num_Products:=ifelse(is.na(Manager_Num_Products)==T,getmode(Manager_Num_Products),Manager_Num_Products)]
combin[,Manager_Num_Products2:=ifelse(is.na(Manager_Num_Products2)==T,getmode(Manager_Num_Products2),Manager_Num_Products2)]
combin[,Applicant_City_PIN:=ifelse(is.na(Applicant_City_PIN)==T,getmode(Applicant_City_PIN),Applicant_City_PIN)]

##Impute using mean##
combin[,Manager_Business:=ifelse(is.na(Manager_Business)==T,mean(Manager_Business,na.rm=T),Manager_Business)]
combin[,Manager_Business2:=ifelse(is.na(Manager_Business2)==T,mean(Manager_Business2,na.rm=T),Manager_Business2)]

##Impute Missing values using mean/mode
combin[,Manager_Status:=ifelse(Manager_Status == "",getmode(Manager_Status),Manager_Status)]
combin[,Manager_Current_Designation:=ifelse(Manager_Current_Designation == "",getmode(Manager_Current_Designation),Manager_Current_Designation)]
combin[,Manager_Joining_Designation:=ifelse(Manager_Joining_Designation == "",getmode(Manager_Joining_Designation),Manager_Joining_Designation)]
combin[,Manager_Gender:=ifelse(Manager_Gender == "",getmode(Manager_Gender),Manager_Gender)]

combin[,Applicant_Qualification:=ifelse(Applicant_Qualification == "",getmode(Applicant_Qualification),Applicant_Qualification)]
combin[,Applicant_Gender:=ifelse(Applicant_Gender == "",getmode(Applicant_Gender),Applicant_Gender)]
combin[,Applicant_Marital_Status:=ifelse(Applicant_Marital_Status == "",getmode(Applicant_Marital_Status),Applicant_Marital_Status)]
combin[,Applicant_Occupation:=ifelse(Applicant_Occupation == "","Missing",Applicant_Occupation)]

combin[,Applicant_Occupation:=factor(Applicant_Occupation)]

levels(combin$Applicant_Occupation)[levels(combin$Applicant_Occupation) ==  "Missing"] <- "1"##Revalue Missing Level

##Treating Date values ##
combin$Applicant_BirthDate<-as.character(combin$Applicant_BirthDate)
combin$Application_Receipt_Date<-as.character(combin$Application_Receipt_Date)
combin$Manager_DOJ<-as.character(combin$Manager_DOJ)
combin$Manager_DoB<-as.character(combin$Manager_DoB)

combin$Applicant_BirthDate<-mdy(combin$Applicant_BirthDate)
combin$Application_Receipt_Date<-mdy(combin$Application_Receipt_Date)
combin$Manager_DOJ<-mdy(combin$Manager_DOJ)
combin$Manager_DoB<-mdy(combin$Manager_DoB)

##Create new Age variables for Applicants and Manager

combin[,ApplicantAge_at_Receipt:=year(Application_Receipt_Date)-year(Applicant_BirthDate)]
combin[,ApplicantAge_at_Receipt:=ifelse(is.na(ApplicantAge_at_Receipt)==T,median(ApplicantAge_at_Receipt,na.rm=T),ApplicantAge_at_Receipt),by=Applicant_Qualification]

combin[,MgrAge_at_Joining:=year(Manager_DOJ)-year(Manager_DoB)]
combin[,MgrAge_at_Joining:=ifelse(is.na(MgrAge_at_Joining)==T,mean(MgrAge_at_Joining,na.rm=T),MgrAge_at_Joining)]


ggplot(data=combin,aes(x=ApplicantAge_at_Receipt,color=Business_Sourced))+ geom_bar(stat="count")+ facet_wrap(~Business_Sourced)

ggplot(data=combin,aes(x=MgrAge_at_Joining,color=Business_Sourced))+ geom_bar()+ facet_wrap(~Business_Sourced)

##Some other New variables#

##Applicant Count by office Pincode##

combin[, Applicant_Count := .N, by = Office_PIN]

##Applicant Count by city Pincode##
combin[, Applicant_Count_City := .N, by = Applicant_City_PIN]

##Applicant DOB Year and Month and Applicant's Receipt Date Year and Month

combin[,Applicant_DOB_Year:=year(Applicant_BirthDate)]
combin[,Applicant_DOB_Year:=ifelse(is.na(Applicant_DOB_Year)==T,getmode(Applicant_DOB_Year),Applicant_DOB_Year)]

combin[,Applicant_DOB_Month:=month(Applicant_BirthDate)]
combin[,Applicant_DOB_Month:=ifelse(is.na(Applicant_DOB_Month)==T,getmode(Applicant_DOB_Month),Applicant_DOB_Month)]

##Mgr DOb Year and Month and Mgr DOJ year and Month 

combin[,Mgr_DOB_Year:=year(Manager_DoB)]
combin[,Mgr_DOB_Year:=ifelse(is.na(Mgr_DOB_Year)==T,median(Mgr_DOB_Year,na.rm=T),Mgr_DOB_Year)]



combin[,Mgr_DOB_Month:=month(Manager_DoB)]
combin[,Mgr_DOB_Month:=ifelse(is.na(Mgr_DOB_Month)==T,getmode(Mgr_DOB_Month),Mgr_DOB_Month)]

combin[,Mgr_DOj_Year:=year(Manager_DOJ)]
combin[,Mgr_DOj_Year:=ifelse(is.na(Mgr_DOj_Year)==T,getmode(Mgr_DOj_Year),Mgr_DOj_Year)]

combin[,Mgr_DOj_Month:=month(Manager_DOJ)]
combin[,Mgr_DOj_Month:=ifelse(is.na(Mgr_DOj_Month)==T,getmode(Mgr_DOj_Month),Mgr_DOj_Month)]


###################### Model Creation ########################

#Divide into train and test
c.train <- combin[1:nrow(Traind),]
c.test <- combin[-(1:nrow(Traind)),]

c.train[,c("ID","Application_Receipt_Date","Applicant_BirthDate","Manager_DOJ","Manager_DoB"):=NULL]
############################################################################################################

##GBM Model
gbm.fit<-gbm(Purchase~.,
             data=c.train,
             distribution = "gaussian",
             n.trees=500,
             shrinkage=0.005,
             interaction.depth=3,
             bag.fraction = 0.5,
             n.minobsinnode = 10,
             cv.folds = 5,
             verbose=TRUE)


?gbm
summary(gbm.fit)

ntrees<-gbm.perf(gbm.fit)

print(ntrees)

obs<-c.train$Business_Sourced
pred<-predict(gbm.fit,c.train,ntrees=ntrees,type="response")

gbm.roc.area(obs, pred)

GBM_Predicted<-predict(gbm.fit,c.test,ntrees=ntrees,type="response")

GBM_Predicted_Response<-ifelse(GBM_Predicted>0.4,1,0)

sub_gbm <- data.frame(ID = c.test$ID, Business_Sourced = GBM_Predicted_Response)

write.csv(sub_gbm,file="Submission_gbm_tuned1.csv")
##########################################################################################################

