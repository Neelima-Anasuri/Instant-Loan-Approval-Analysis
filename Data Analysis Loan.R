Ntrain_Org<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/train_Orginal.csv', na.strings=c("","NA"))
Ntest_Org<- read.csv('C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/test_Original.csv', na.strings=c("","NA"))

Ntrain<- Ntrain_Org
Ntest<- Ntest_Org

names(Ntrain_Org)
#Impute the missing values:

library(Hmisc)
library(zoo)

colSums(is.na(Ntrain))

# No of values are less SO cann't use aregImpute function.
table(Ntrain$Loan_Amount_Term)
table(Ntest$Loan_Amount_Term) # 6 months is the extra level in test data

Ntrain$Loan_Amount_Term <-na.locf(Ntrain$Loan_Amount_Term)
Ntest$Loan_Amount_Term <-na.locf(Ntest$Loan_Amount_Term)

impute_arg <- aregImpute(~ Loan_Status + Gender + Married + Dependents + Credit_History+ Self_Employed+
                           Property_Area +Education +ApplicantIncome +CoapplicantIncome + LoanAmount 
                         , data = Ntrain, n.impute = 5)

Ntrain$Married =na.fill(Ntrain$Married,impute_arg$imputed$Married)
Ntrain$Gender =na.fill(Ntrain$Gender,impute_arg$imputed$Gender)
Ntrain$Dependents =na.fill(Ntrain$Dependents,impute_arg$imputed$Dependents)
Ntrain$Credit_History =na.fill(Ntrain$Credit_History,impute_arg$imputed$Credit_History)
Ntrain$Self_Employed =na.fill(Ntrain$Self_Employed,impute_arg$imputed$Self_Employed)
Ntrain$LoanAmount =na.fill(Ntrain$LoanAmount,impute_arg$imputed$LoanAmount)


impute_arg1 <- aregImpute(~  Gender + Married + Dependents + Credit_History+ Self_Employed+
                           Property_Area +Education +ApplicantIncome +CoapplicantIncome + LoanAmount 
                         , data = Ntest, n.impute = 5)

Ntest$Married =na.fill(Ntest$Married,impute_arg1$imputed$Married)
Ntest$Gender =na.fill(Ntest$Gender,impute_arg1$imputed$Gender)
Ntest$Dependents =na.fill(Ntest$Dependents,impute_arg1$imputed$Dependents)
Ntest$Credit_History =na.fill(Ntest$Credit_History,impute_arg1$imputed$Credit_History)
Ntest$Self_Employed =na.fill(Ntest$Self_Employed,impute_arg1$imputed$Self_Employed)
Ntest$LoanAmount =na.fill(Ntest$LoanAmount,impute_arg1$imputed$LoanAmount)


colSums(is.na(Ntrain))
colSums(is.na(Ntest))


# Check the Ranges of train VS TEST : All are in the Same Proposiotion

print(c("Train:",round(prop.table(table(Ntrain$Gender)),2)))
print(c("Test:",round(prop.table(table(Ntest$Gender)),2)))
print(c("Train:",round(prop.table(table(Ntrain$Married)),2)))
print(c("Test:",round(prop.table(table(Ntest$Married)),2)))
print(c("Train:",round(prop.table(table(Ntrain$Dependents)),2)))
print(c("Test:",round(prop.table(table(Ntest$Dependents)),2)))
print(c("Train:",round(prop.table(table(Ntrain$Self_Employed)),2)))
print(c("Test:",round(prop.table(table(Ntest$Self_Employed)),2)))
print(c("Train:",round(prop.table(table(Ntrain$Property_Area)),2)))
print(c("Test:",round(prop.table(table(Ntest$Property_Area)),2)))


# Check the Ranges seems to be good and one extra level in test-loan amount
print(c(" Appl Income",range(Ntrain$ApplicantIncome),"Co-App-income",range(Ntrain$CoapplicantIncome),"Loan AMount:",range(Ntrain$LoanAmount)))
print(c(" Appl Income",range(Ntest$ApplicantIncome),"Co-App-income",range(Ntest$CoapplicantIncome),"Loan AMount:",range(Ntest$LoanAmount)))
print(c(table(Ntrain$Loan_Amount_Term)))
print(c(table(Ntest$Loan_Amount_Term)))

# Adding New Fields:EMI, Balance Amount
mr = 4/(12*100)
Ntrain["EMI"] <- round((Ntrain$LoanAmount *1000* mr *((1+mr)^Ntrain$Loan_Amount_Term)) /(((1+mr)^Ntrain$Loan_Amount_Term)-1),2)
Ntest["EMI"] <- round((Ntest$LoanAmount *1000* mr *((1+mr)^Ntest$Loan_Amount_Term)) /(((1+mr)^Ntest$Loan_Amount_Term)-1),2)

Ntrain["Balance_Amount"] <- Ntrain$ApplicantIncome + Ntrain$CoapplicantIncome - Ntrain$EMI
Ntest["Balance_Amount"] <- Ntest$ApplicantIncome + Ntest$CoapplicantIncome - Ntest$EMI

print(c("EMI",range(Ntrain$EMI),"Balance_Amount",range(Ntrain$Balance_Amount)))
print(c("EMI",range(Ntest$EMI),"Balance_Amount",range(Ntest$Balance_Amount)))

Ntrain["Total_income"]<-Ntrain$ApplicantIncome + Ntrain$CoapplicantIncome
Ntest["Total_income"]<-Ntest$ApplicantIncome + Ntest$CoapplicantIncome
Ntrain["log.Total_income"]<-log(Ntrain$Total_income)
Ntest["log.Total_income"]<-log(Ntest$Total_income)
Ntrain["log.Loan_Amount"]<-log(Ntrain$LoanAmount)
Ntest["log.Loan_Amount"]<-log(Ntest$LoanAmount)

head(Ntest$Total_income)

# Dependends Yes or NO

levels(Ntrain$Dependents)<-c("0","1","1","1")
levels(Ntrain$Dependents)


table(Ntest$Dependents)
levels(Ntest$Dependents)<-c("0","1","1","1")
levels(Ntest$Dependents)

# Regular or Irregular Loans: thought to make two groups but some problem 
Ntrain["Loan_Trem_regular"] <- ifelse(Ntrain$Loan_Amount_Term %in% c(36,60,84,120,180,240,300,480),0,1)
Ntest["Loan_Trem_regular"] <- ifelse(Ntest$Loan_Amount_Term %in% c(36,60,84,120,180,240,300,480),0,1)

install.packages("magrittr")
library(magrittr)
library(dplyr)
Ntrainbk["Gen_Marr_Edu"] <- "Y"
Ntrainbk<- Ntrainbk %>%
  select(Gen_Marr_Edu:Gender,Married,Education) %>%
  mutate(
    type = case_when(
      Gender == "Male" & Married == "Yes" & Education == "Graduate" ~ "MYG",
      Gender == "Male" & Married == "No" & Education == "Graduate" ~ "MNG",
      Gender == "Female" & Married == "Yes" & Education == "Graduate" ~ "FYG",
      Gender == "Female" & Married == "No" & Education == "Graduate" ~ "FNG",
      Gender == "Male" & Married == "Yes" & Education == "Not Graduate" ~ "MYN",
      Gender == "Male" & Married == "No" & Education == "Not Graduate" ~ "MNN",
      Gender == "Female" & Married == "Yes" & Education == "Not Graduate" ~ "FYN",
      Gender == "Female" & Married == "No" & Education == "Not Graduate" ~ "FNN",
      TRUE                      ~  "other"
    )
  )

Ntestbk["Gen_Marr_Edu"] <- "Y"
Ntestbk<- Ntestbk %>%
  select(Gen_Marr_Edu:Gender,Married,Education) %>%
  mutate(
    type = case_when(
      Gender == "Male" & Married == "Yes" & Education == "Graduate" ~ "MYG",
      Gender == "Male" & Married == "No" & Education == "Graduate" ~ "MNG",
      Gender == "Female" & Married == "Yes" & Education == "Graduate" ~ "FYG",
      Gender == "Female" & Married == "No" & Education == "Graduate" ~ "FNG",
      Gender == "Male" & Married == "Yes" & Education == "Not Graduate" ~ "MYN",
      Gender == "Male" & Married == "No" & Education == "Not Graduate" ~ "MNN",
      Gender == "Female" & Married == "Yes" & Education == "Not Graduate" ~ "FYN",
      Gender == "Female" & Married == "No" & Education == "Not Graduate" ~ "FNN",
      TRUE                      ~  "other"
    )
  )

head(Ntrainbk$type)
Ntrain["Gen_Marr_Edu"]<- Ntrainbk$type
Ntest["Gen_Marr_Edu"]<- Ntestbk$type

Ntrain["Gen_Marr_Edu"] <- factor(Ntrain$Gen_Marr_Edu)
Ntest["Gen_Marr_Edu"] <- factor(Ntest$Gen_Marr_Edu)

table(Ntrain$Loan_Trem_regular)
#levels(Ntrain$Loan_Amount_Term)<-c("0","1","2","3","4","5","6","7")
#levels(Ntrain$Loan_Amount_Term)<-c("0","0","2","3","4","5","6","7")
#levels(Ntest$Loan_Amount_Term)<-c("8","0","1","2","3","4","5","6","7")


# Negative Values are in Balance so let's examine those records, typo error in term: changing from 36 to 360 & 12 to 120 
# LP001870 ,LP002588,LP002768
Ntrainbk <- Ntrain
Ntrain[Ntrain$Balance_Amount< 1000,]
Ntrain$Loan_Amount_Term[Ntrain$Loan_ID %in% c("LP001870","LP002768")] = 360
Ntrain$Loan_Amount_Term[Ntrain$Loan_ID %in% c("LP002588")] = 120
Ntrain["EMI"] <- round((Ntrain$LoanAmount *1000* mr *((1+mr)^Ntrain$Loan_Amount_Term)) /(((1+mr)^Ntrain$Loan_Amount_Term)-1),2)
Ntrain["Balance_Amount"] <- Ntrain$ApplicantIncome + Ntrain$CoapplicantIncome - Ntrain$EMI
Ntrain["log.EMI"] <- log(Ntrain$EMI)
Ntrain["log.Balance_Amount"] <- log(Ntrain$Balance_Amount)
Ntrain["log.Loan_AMount"] <- log(Ntrain$LoanAmount)


Ntestbk <- Ntest
Ntest[Ntest$Balance_Amount< 1000,]
Ntest$Loan_Amount_Term[Ntest$Loan_ID %in% c("LP001794")] = 120
Ntest$Loan_Amount_Term[Ntest$Loan_ID %in% c("LP002802")] = 60
Ntest$Loan_Amount_Term[Ntest$Loan_Amount_Term ==350] = 360

Ntest["EMI"] <- round((Ntest$LoanAmount *1000* mr *((1+mr)^Ntest$Loan_Amount_Term)) /(((1+mr)^Ntest$Loan_Amount_Term)-1),2)
Ntest["Balance_Amount"] <- Ntest$ApplicantIncome + Ntest$CoapplicantIncome - Ntest$EMI
Ntest["log.EMI"] <- log(Ntest$EMI)
Ntest["log.Balance_Amount"] <- log(Ntest$Balance_Amount)
Ntest["log.Loan_AMount"] <- log(Ntest$LoanAmount)


str(Ntest)
Ntest$Loan_Amount_Term <- Ntestbk$Loan_Amount_Term

# Selection of Features:
names(Ntrain)
names(Ntest)
Ntrain_Selected <- Ntrain[c(4,6,10,11,12,13,17,18,19,20,21,22)]
Ntest_Selected <- Ntest[c(4,6,10,11,12,16,17,18,19,20,21)]

summary(Ntrain_Selected)
summary(Ntest_Selected)

table(Ntrain_Selected$Gen_Marr_Edu)
table(Ntest_Selected$Gen_Marr_Edu)

# PCA of variables:
library(caret)
Ntrain_Selected$Loan_Amount_Term<-factor(Ntrain_Selected$Loan_Amount_Term)
Ntest_Selected$Loan_Amount_Term<-factor(Ntest_Selected$Loan_Amount_Term)

trans = preProcess(Ntrain_Selected[,c(-6)],method = c("BoxCox","center","scale","pca"))
train_PCA = predict(trans, Ntrain_Selected[,c(-6)])
test_PCA = predict(trans, Ntest_Selected)

train_PCA["Loan_Status"] <- Ntrain$Loan_Status
head(test_PCA )
#Store PCA files
write.csv(train_PCA, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/train_PCA.csv", row.names = FALSE)
write.csv(test_PCA, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/test_PCA.csv", row.names = FALSE)
write.csv(Ntrain, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/Ntrain.csv", row.names = FALSE)
write.csv(Ntest, file = "C:/Users/HARI/Desktop/Neelima/Analytical Vidhya/Loan/Ntest.csv", row.names = FALSE)
