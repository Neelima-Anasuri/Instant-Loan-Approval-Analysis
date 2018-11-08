library(dplyr)
library(caret)
library(ggplot2)
library(lattice)

train_PCA<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/train_PCA.csv', na.strings=c("","NA"))
test_PCA<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/test_PCA.csv', na.strings=c("","NA"))
Ntrain<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/Ntrain.csv', na.strings=c("","NA"))
Ntest<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/Ntest.csv', na.strings=c("","NA"))


names(Ntrain)
head(train_PCA)
#Converting outcome variable to numeric
train_PCA$Loan_Status<-ifelse(train_PCA$Loan_Status=='N',0,1)
train_PCA["Married"] <- Ntrain$Married
test_PCA["Married"] <- Ntest$Married
str(train_PCA)

#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = train_PCA,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_PCA))
dmyt <- dummyVars(" ~ .", data = test_PCA,fullRank = T)
test_transformed <- data.frame(predict(dmyt, newdata = test_PCA))

train_transformed$Loan_Status<- factor(train_transformed$Loan_Status)

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]


#Feature selection using rfe in caret

control <- rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 5,verbose = FALSE)
outcomeName<-'Loan_Status'
predictors<-names(trainSet )[!names(trainSet) %in% outcomeName]
Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],rfeControl = control)

predictors(Loan_Pred_Profile)

predictors<-c("PC3", "PC2", "PC4", "Property_Area.Semiurban", "PC1","Loan_Amount_Term","Married.Yes","Gen_Marr_Edu.MNG")
names(trainSet)
names(trainSet)
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
model_rpart<-train(trainSet[,predictors],trainSet[,outcomeName],method='rpart')
model_xgb<-train(trainSet[,predictors],trainSet[,outcomeName],method='xgbTree')

varImp(object=model_gbm)

#Parameter tuning \:

fitControl <- trainControl( method = "repeatedcv",  number = 5,  repeats = 5)

model_gbm1<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
model_rf1<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=10)
model_nnet1<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet',trControl=fitControl,tuneLength=10)
model_glm1<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=10)
#model_xgb1<-train(trainSet[,predictors],trainSet[,outcomeName],method='xgbTree',trControl=fitControl,tuneLength=10)
model_rpart1<-train(trainSet[,predictors],trainSet[,outcomeName],method='rpart',trControl=fitControl,tuneLength=10)

# Results prediction
gbmresults<-predict.train(object=model_gbm,test_transformed[,predictors],type="response")
rfresults<-predict.train(object=model_rf,test_transformed[,predictors],type="raw")
nnetresults<-predict.train(object=model_nnet,test_transformed[,predictors],type="raw")
glmresults<-predict.train(object=model_glm,test_transformed[,predictors],type="raw")
xgbresults<-predict.train(object=model_xgb,test_transformed[,predictors],type="raw")
cartresults<-predict.train(object=model_rpart,test_transformed[,predictors],type="raw")


fitted.results <- predict(model_gbm1,newdata=test_transformed[,predictors],type='prob')
fitted.results <- ifelse(gbm_tuned$`1` > 0.58,'Y',"N")
table(fitted.results)
str(gbm_tuned)
fitted.results <- data.frame(fitted.results)

gbm_tuned<-predict.train(object=model_gbm1,test_transformed[,predictors],type="prob")
rf_tuned<-predict.train(object=model_rf1,test_transformed[,predictors],type="raw")
nnet_tuned<-predict.train(object=model_nnet1,test_transformed[,predictors],type="raw")
glm_tuned<-predict.train(object=model_glm1,test_transformed[,predictors],type="raw")
#xgbresults<-predict.train(object=model_xgb1,test_transformed[,predictors],type="raw")
cart_tuned<-predict.train(object=model_rpart1,test_transformed[,predictors],type="raw")


test_results1 <- cbind(gbmresults,rfresults,nnetresults,glmresults,xgbresults,cartresults)
test_results2 <- cbind(gbm_tuned,rf_tuned,nnet_tuned,glm_tuned,cart_tuned)

table(test_results1[,1])
table(gbm_tuned)
class(test_results1)
write.csv(test_results1, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/test_results.csv", row.names = FALSE)
write.csv(test_results2, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/test_results.csv", row.names = FALSE)
write.csv(fitted.results, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/fitted.results.csv", row.names = FALSE)
