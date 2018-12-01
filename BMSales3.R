setwd("C:\\Users\\Gaurav\\Documents\\Analytics Vidhya\\BigMart Sales 3")

require(data.table)
require(dplyr)
require(lubridate)
require(ggplot2)
require(rpart)
require(partykit)
require(caret)
require(randomForest)

Train<-fread("Train.csv")

Test<-fread("Test.csv")

head(Train)

str(Train)

summary(Train)

Train<-Train[,Item_Fat_Content:=gsub("LF","Low Fat",Item_Fat_Content)]

Train<-Train[,Item_Fat_Content:=gsub("low fat","Low Fat",Item_Fat_Content)]

Train<-Train[,Item_Fat_Content:=gsub("reg","Regular",Item_Fat_Content)]

Train<-Train[,Item_Weight:=ifelse(is.na(Item_Weight)==TRUE,mean(Item_Weight,na.rm=T),Item_Weight),by=Item_Type]

Train<-Train[,Outlet_Size:=ifelse((Outlet_Size)=='','NoDAta',Outlet_Size)]

Train<-Train[,Outlet_Age:=2013-Outlet_Establishment_Year]

Train<-Train[,Mean_MRP_OutletType:=mean(Item_MRP),by=Outlet_Type]

Train<-Train[,Mean_wt_OutletType:=mean(Item_Weight),by=Outlet_Type]

Train<-Train[,Mean_wt_OutletType:=mean(Item_Weight),by=Item_Type]

Train<-Train[,MrpbyWt:=Item_MRP/Item_Weight]

Train1<-Train%>%mutate_if(is.character,as.factor)

class(Train1)
Train1<-Train1[,-1]
##Convert all factorts to dummy variables

Train2<-dummyVars("~ .",data=Train1)

Train3<-data.frame(predict(Train2,newdata=Train1))


Test<-Test[,Item_Fat_Content:=gsub("LF","Low Fat",Item_Fat_Content)]

Test<-Test[,Item_Fat_Content:=gsub("low fat","Low Fat",Item_Fat_Content)]

Test<-Test[,Item_Fat_Content:=gsub("reg","Regular",Item_Fat_Content)]

Test<-Test[,Item_Weight:=ifelse(is.na(Item_Weight)==TRUE,mean(Item_Weight,na.rm=T),Item_Weight),by=Item_Type]

Test<-Test[,Outlet_Size:=ifelse((Outlet_Size)=='','NoDAta',Outlet_Size)]

Test<-Test[,Outlet_Age:=2013-Outlet_Establishment_Year]

Test<-Test[,Mean_MRP_OutletType:=mean(Item_MRP),by=Outlet_Type]

Test<-Test[,Mean_wt_OutletType:=mean(Item_Weight),by=Outlet_Type]

Test<-Test[,Mean_wt_OutletType:=mean(Item_Weight),by=Item_Type]

Test<-Test[,MrpbyWt:=Item_MRP/Item_Weight]

Test1<-Test%>%mutate_if(is.character,as.factor)

Test1<-Test1[,-1]
##Convert all factorts to dummy variables

Test2<-dummyVars("~ .",data=Test1)

Test3<-data.frame(predict(Test2,newdata=Test1))

##rm(Test3)

##RandomForest Model
rfmodel1<-randomForest(Item_Outlet_Sales~.,data=Train3,ntree=500,mtry=5)

rfmodel1
sqrt(1237142)

plot(rfmodel1)

# Variable Importance Plot
varImpPlot(rfmodel1,
           sort = T,
           main="Variable Importance",
           n.var=5)

##Predicting on Test Data

Test3$Item_Outlet_Sales<-predict(rfmodel1,Test3)

Test3$Item_Identifier<-Test$Item_Identifier
Test3$Outlet_Identifier<-Test$Outlet_Identifier

head(Test3)

sub_rf<- data.frame(Item_Identifier = Test3$Item_Identifier, Outlet_Identifier = Test3$Outlet_Identifier, Item_Outlet_Sales = Test3$Item_Outlet_Sales)
 write.csv(sub_rf, file = "first_sub.csv", row.names = F)

 
 ##GBM Model using caret
 
 gbmGrid <-  expand.grid(interaction.depth = 10,
                         n.trees = 500, 
                         shrinkage = .01,
                         n.minobsinnode = 10)
 
 objControl <- trainControl(method='repeatedcv', number=5, returnResamp='none')
 
 gbmModel2 <- train(Item_Outlet_Sales~.,data=Train3, 
                   method='gbm', 
                   trControl=objControl,  
                   metric = "RMSE",
                   tuneGrid = gbmGrid
                   )
 
 gbmModel2 <- update(gbmModel,shrinkage=0.01)
   
 summary(gbmModel3)
 
 importance(gbmModel2)
 
 gbmModel2$bestTune
 
 gbmModel3 <- train(Item_Outlet_Sales~Item_MRP+Outlet_Type.Grocery.Store+Outlet_Establishment_Year+Outlet_Identifier.OUT027+MrpbyWt+Outlet_Age+Item_Visibility,data=Train3, 
                    method='gbm', 
                    trControl=objControl,  
                    metric = "RMSE",
                    tuneGrid = gbmGrid
 )
 
 Test3$Item_Outlet_Sales_gbm<-predict(gbmModel2,Test3)
 
 sub_rf<- data.frame(Item_Identifier = Test3$Item_Identifier, Outlet_Identifier = Test3$Outlet_Identifier, Item_Outlet_Sales = Test3$Item_Outlet_Sales_gbm)
 write.csv(sub_rf, file = "Third_sub.csv", row.names = F)