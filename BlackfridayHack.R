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

##install.packages("dummies")

setwd("E:\\Analytics Vidhya\\Black Friday hackathon")

Traind<-fread("train.csv",stringsAsFactors = T)

Testd<-fread("test-comb.csv",stringsAsFactors = T)

str(Traind)

levels(Traind$Stay_In_Current_City_Years)

head(Testd)

dim(Traind)

##Summary of data##

summary(Testd)

##Combine train and Test

Testd[,c("V1","Comb"):=NULL]

Testd[,Purchase:=mean(Traind$Purchase,na.rm=T)]

com<-list(Traind,Testd)

combin<-rbindlist(com)

###Univariate Analysis###

combin[,prop.table(table(Gender))] #analyzing gender variable
combin[,prop.table(table(Age))] ##Analyzing Age variable
combin[,prop.table(table(Stay_In_Current_City_Years))] ##Analyzing Stay_In_Current_City_Years variable
combin[,prop.table(table(Marital_Status))] ##Analyzing Marital_Status variable

#unique values in ID variables
length(unique(combin$Product_ID))

length(unique(combin$User_ID))


#missing values
colSums(is.na(combin))

#create a new variable for missing values
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2, is.na) ==    TRUE,1,0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3, is.na) ==  TRUE,1,0)]

#impute missing values
combin[,Product_Category_2 := ifelse(is.na(Product_Category_2) == TRUE, "-999",  Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(Product_Category_3) == TRUE, "-999",  Product_Category_3)]


#set column level
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) ==  "4+"] <- "4"

#recoding age groups
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

#convert age to numeric
combin$Age <- as.numeric(combin$Age)

#convert Gender into numeric
combin[, Gender := as.numeric(as.factor(Gender)) - 1]

#User Count
combin[, User_Count := .N, by = User_ID]

#Product Count
combin[, Product_Count := .N, by = Product_ID]

#Mean Purchase of Product
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]

#Mean Purchase of User
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]

##One hot encoding for City_category
 combin <- dummy.data.frame(combin, names = c("City_Category"), sep = "_")
 
 head(combin)
 
#converting Product Category 2 & 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)
 


#Divide into train and test
c.train <- combin[1:nrow(Traind),]
c.test <- combin[-(1:nrow(Traind)),]

c.train <- c.train[c.train$Product_Category_1 <= 18,]
########################################################################################################

##GBM H20

library(h2o)

localH2O <- h2o.init(nthreads = -1)

#data to h2o cluster
train.h2o <- as.h2o(c.train)
test.h2o <- as.h2o(c.test)


colnames(train.h2o)

y.dep <- 14

#independent variables (dropping ID variables)
x.indep <- c(3:13,15:20)

#GBM
system.time(
  gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
)
##################################################################################################
##Caret GBM##

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = FALSE)

gbmGrid <- expand.grid(.n.trees = 500,
                       .interaction.depth = 10,
                       .shrinkage = 0.05)

objModel <- train(Purchase~.,data=c.train,
                  method='gbm',distribution = "gaussian",
                  trControl=objControl,tuneGrid = gbmGrid
)

summary(objModel)


####################################################################################################

##GBM Pkg
library(gbm)

c.train<-c.train[,c(-1,-2)]

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


summary(gbm.fit)
t
ntrees<-gbm.perf(gbm.fit)

print(ntrees)
?predict

NROW(GBM_Predicted_Purchase)

GBM_Predicted_Purchase<-predict(gbm.fit,c.test,ntrees=ntrees,type="link")

sub_gbm <- data.frame(User_ID = c.test$User_ID, Product_ID = c.test$Product_ID, Purchase = GBM_Predicted_Purchase)

write.csv(sub_gbm,file="Submission_gbm.csv")
