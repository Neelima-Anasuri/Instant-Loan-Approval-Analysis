#First Level :


trainloan<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/train.csv', na.strings=c("","NA"))

#Data Analysis

tableG<-table(Cattrain$Loan_Status,Cattrain$Gender)
barplot(tableG,beside=T, legend.text = c("NotApporved","Approved"),
        main= "Gender vs Loan Status",xlab= "Gender",las=1,col=c(2,4))

tableM<-table(Cattrain$Loan_Status,Cattrain$Married)
tableM
barplot(tableM,beside=T, legend.text = c("NotApporved","Approved"),
        main= "Married vs Loan Status",xlab= "Married",las=1,col=c(2,4))


tableD<-pr(Cattrain$Loan_Status,Cattrain$Dependents)
barplot(tableD,beside=T, legend.text = c("NotApporved","Approved"),
        main= "Depended vs Loan Status",xlab= "Dependents",las=1,col=c(2,4))
propDep= prop.table(tableD)
barplot(propDep,beside=T, legend.text = c("NotApporved","Approved"),
        main= "Depended vs Loan Status",xlab= "Dependents",las=1,col=c(2,4))
chisq.test(tableM)
install.packages("epiR")
library(epiR)
epi.2by2(tableM)
na.l
trainloan <- trainloan[c(-1)]  

#Handling Missing values;

install.packages(Hmisc)
library(Hmisc)
colSums(is.na(trainloan))

impute_arg <- aregImpute(~ Gender + Married + Dependents + Credit_History+ Self_Employed, data = trainloan, n.impute = 5)

trainloan$Married =na.fill(trainloan$Married,impute_arg$imputed$Married)
trainloan$Gender =na.fill(trainloan$Gender,impute_arg$imputed$Gender)
trainloan$Dependents =na.fill(trainloan$Dependents,impute_arg$imputed$Dependents)
trainloan$Credit_History =na.fill(trainloan$Credit_History,impute_arg$imputed$Credit_History)
trainloan$Self_Employed =na.fill(trainloan$Self_Employed,impute_arg$imputed$Self_Employed)

impute_arg1<- aregImpute(~ LoanAmount + Loan_Amount_Term, data = trainloan, n.impute = 5,nk=0)

trainloan$LoanAmount =na.fill(trainloan$LoanAmount,impute_arg1$imputed$LoanAmount)
trainloan$Loan_Amount_Term =na.fill(trainloan$Loan_Amount_Term,impute_arg1$imputed$Loan_Amount_Term)

#Logistic Model

install.packages('caTools')
library(caTools)

set.seed(88)
split <- sample.split(trainloan$Loan_Status, SplitRatio = 0.75)
head(trainloan)

trainloan <- trainloan[-1]

colSums(is.na(trainloan))
sum(is.na(predict))

#get training and test data
dresstrain <- subset(trainloan, split == TRUE)
dresstest <- subset(trainloan, split == FALSE)
trainloan$Loan_Status
#logistic regression model
model <- glm (Loan_Status ~., data = dresstrain, family = binomial)
summary(model)
predict <- predict(model, type = 'response')
length(dresstest)
length(dresstrain$Loan_Status)
table(dresstrain$Loan_Status, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, dresstrain$Loan_Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

p <- predict(model, newdata=subset(dresstest,select=c(1:11)), type="response")
pr <- prediction(p, dresstest$Loan_Status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#plot glm
library(ggplot2)
ggplot(dresstrain, aes(x=Rating, y=Recommended)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE)


#Decision Tree
library(rpart)
# grow tree 
fit <- rpart(dresstrain$Loan_Status~ ., data = dresstrain,method="class")
summary(fit)
#Predict Output 
predict = predict(fit, dresstest[-12], type = "prob")

library(ROCR)
ROCRpred <- prediction(predict[,2], dresstest$Loan_Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

p <- predict(fit, newdata=subset(dresstest,select=c(1:11)), type="response")
pr <- prediction(p, dresstest$Loan_Status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Ramdom Forest
install.packages("randomForest")
library(randomForest)
fit1 <- randomForest(dresstrain$Loan_Status~ ., dresstrain,ntree=100)
summary(fit1)
#Predict Output 
predicted= predict(fit1,dresstest[-12])

library(ROCR)
ROCRpred <- prediction(predicted[,2], dresstest$Loan_Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

p <- predict(fit1, newdata=subset(dresstest,select=c(1:11)), type="response")
pr <- prediction(p, dresstest$Loan_Status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#GBM in R (with cross validation)

library(caret)
fitControl <- trainControl(method = "cv", number = 10)
tune_Grid <-  expand.grid(interaction.depth = 2, n.trees = 500,shrinkage = 0.1,n.minobsinnode = 10)

dresstrain$Loan_Status <- as.factor(dresstrain$Loan_Status) 

set.seed(825)
library(gbm)
fit2 <- train(dresstrain$Loan_Status ~ ., data = dresstrain,method = "glm",trControl = fitControl,verbose = FALSE,
               tuneGrid = tune_Grid )                          

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),
                    n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

model_gbm<-train(dresstrain[,c(1:11)],dresstrain[,"Loan_Status"],method='gbm',trControl=fitControl,tuneGrid=grid)

summary(model_gbm)

#Predictions
predictions<-predict.train(object=model_gbm,dresstest[,c(1:11)],type="raw")
table(predictions)
confusionMatrix(predictions,dresstest[,"Loan_Status"])



plot(model_gbm)
fit1 <- randomForest(dresstrain$Loan_Status~ ., dresstrain,ntree=100)
model_nnet<-train(dresstrain[,c(1:11)],dresstrain[,"Loan_Status"],method='nnet')

summary(model_nnet)
predicted= predict(model_nnet,dresstest[-12])  

#Predictions
predictions<-predict.train(object=model_nnet,dresstest[,c(1:11)],type="raw")
table(predictions)
confusionMatrix(predictions,dresstest[,"Loan_Status"])



#Feature Engnieering:




varImp(object=model_nnet)
plot(varImp(object=model_gbm),main="GBM - Variable Importance")

# Outliners.


x <- trainloan[,2:12]
y <- trainloan[,13]

imp

boxplot(inttrain[1],main=names(inttrain)[1])
boxplot(inttrain[2],main=names(inttrain)[2])
boxplot(inttrain[3],main=names(inttrain)[3])


par(mfcol=c(1,3))
for(i in 1:3) {
  boxplot(table(inttrain[i]))
}

par(mfrow=c(3,3))
for(i in 1:9) {
  barplot(table(factrain[i]))
}

#featurePlot(x=inttrain, y=factrain[9], plot="ellipse")
# box and whisker plots for each attribute
#featurePlot(x=x, y=y, plot="box")
# Run algorithms using 10-fold cross validation

#Test Harness
control <- trainControl(method="cv", number=1)
metric <- "Accuracy"

trainloan$Loan_Status = ifelse(trainloan$Loan_Status == "Y",1,0)
trainloan1<-trainloan
colSums(is.na(trainloan1))

library(zoo)
trainloan =na.locf(trainloan)

class(trainloan1$Loan_Status)
factor(trainloan1$Loan_Status)
library("caret")
set.seed(7)

trainloan1= trainloan[-1]

# linear algorithms
fit.lda <- train(Loan_Status~., data=trainloan1, method="lda", metric=metric,na.action=na.exclude)
set.seed(7)
fit.lda <- train(Loan_Status~., data=trainloan1, method="lda", metric=metric, trControl=control, na.action=na.exclude)

# b) nonlinear algorithms
# CART

fit.cart <- train(Loan_Status~., data=trainloan, method="rpart", metric=metric,na.action = na.exclude)
