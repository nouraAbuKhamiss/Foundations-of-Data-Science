#Predicting Diabetes
#Setting data folder
setwd("C:/Users/Noura/Documents/R/Data Science Foundation/Capstone/Diabetes/trainingSet/trainingSet")

#Reading Data Tables
patient <- read.csv("training_SyncPatient.csv")
medication <- read.csv("training_SyncMedication.csv")
labResults <- read.csv("training_SyncLabResult.csv")
diagnosis <- read.csv("training_SyncDiagnosis.csv")
transcript <- read.csv("training_SyncTranscript.csv")
allergy <- read.csv("training_SyncAllergy.csv")
smokingStatus <- read.csv("SyncSmokingStatus.csv")
patientSmoking <- read.csv("training_SyncPatientSmokingStatus.csv")

#Loading packages
library(plyr)
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("glmnet")
library(glmnet)
#install.packages("caret")
library(caret)
#install.packages("ROCR")
library(ROCR)
#install.packages("e1071")
library(e1071)


#Indicator Description
patient <- mutate(patient, Diagnosis =ifelse(DMIndicator == 1, "Diabetic", "Non Diabetic"))
#Calculating Age Column
patient <- within(patient, Age <- 2012 - YearOfBirth)
patient <- mutate(patient, AgeDesc = ifelse(Age <= 12, "Child", ifelse(Age >= 13 & Age <= 17, "Teenager",
                                                                       ifelse(Age >= 18 & Age <= 29, "Young Adult",
                                                                              ifelse(Age >= 30 & Age <= 39, "Thirties",
                                                                                     ifelse(Age >= 40 & Age <=64, "Middle Aged",
                                                                                            ifelse(Age >=65 , "Elder", NA)))))))

#Getting last visit BMI for patients
lastVisitBMI <- data.frame(transcript %>% group_by(PatientGuid) %>% filter( BMI > 0 & BMI < 100 )%>% filter(VisitYear == max(VisitYear)))
#populating the BMI in the patient table
patient$BMI <- lastVisitBMI[match(patient$PatientGuid, lastVisitBMI$PatientGuid), 6]
patient <- mutate(patient, BMIDesc = ifelse(BMI <=18.5, "Underweight", ifelse(BMI >= 18.5 & BMI < 25, "Healthy Weight",
                                                                              ifelse(BMI >= 25 & BMI < 30, "Overweight",
                                                                                     ifelse(BMI >= 30, "Obese", 0)))))

#Getting cholesterol diagnosis
diagnosis$ICD9Code <-  as.numeric(levels(diagnosis$ICD9Code))[diagnosis$ICD9Code]
cholesterol <- diagnosis %>% group_by(PatientGuid) %>% filter(ICD9Code >= 272 , ICD9Code< 273)  
patient$Cholesterol <- ifelse(patient$PatientGuid %in% cholesterol$PatientGuid, 1, 0)
patient <- mutate(patient, CholesterolDesc = ifelse(Cholesterol == 1, "Cholesterol", "No Cholesterol"))


#Getting hypertension diagnosis
hypertension <- diagnosis %>% group_by(PatientGuid) %>% filter(ICD9Code >= 401 , ICD9Code< 402)  
patient$Hypertension <- ifelse(patient$PatientGuid %in% hypertension$PatientGuid, 1, 0)

#Getting max Systolic blood pressure 
transcript$SystolicBP <-  as.numeric(levels(transcript$SystolicBP))[transcript$SystolicBP]
lastVisitBP <- data.frame(transcript %>% group_by(PatientGuid) %>% filter(SystolicBP > 0 )
                          %>% filter(VisitYear == max(VisitYear)))
index <- with(lastVisitBP, order(VisitYear, SystolicBP , decreasing = TRUE))
lastVisitBP <- lastVisitBP[index,]
patient$BP <- lastVisitBP[match(patient$PatientGuid, lastVisitBP$PatientGuid), 7]
patient$BP[is.na(patient$BP)] <- 0 
patient <- mutate(patient, BPDesc = ifelse(BP < 120, "Normal", ifelse(BP>=120 & BP <= 139, "Prehypertension",
                                                                      ifelse(BP >=140 & BP <=159, "Stage I Hypertension",
                                                                             ifelse(BP >= 160, "Stage II Hypertension", NA)))))

#Count of Allergy
allergyCount <- allergy %>% group_by(PatientGuid) %>% tally()
allergyCount <-data.frame(allergyCount)
patient$Allergy <- allergyCount[match(patient$PatientGuid, allergyCount$PatientGuid), 2]
patient$Allergy[is.na(patient$Allergy)] <- 0
patient <- mutate(patient, AllergyDesc = ifelse(Allergy == 0, "No Allergy", "Allegy"))

#Count of medications
medicationCount <- medication %>% group_by(PatientGuid) %>% tally(sort = TRUE)
medicationCount <- data.frame(medicationCount)
patient$Medication <- medicationCount[match(patient$PatientGuid, medicationCount$PatientGuid), 2]
patient$Medication[is.na(patient$Medication)] <- 0
patient <- mutate(patient, MedicationDesc = ifelse(Medication > 0, "Medication", "No Medication" ))

#Count of lab results
labCount <- labResults %>% group_by(PatientGuid) %>% tally()
labCount <- data.frame(labCount)
patient$Labs <- labCount[match(patient$PatientGuid, labCount$PatientGuid), 2]
patient$Labs[is.na(patient$Labs)] <- 0
patient <- mutate(patient, LabDesc = ifelse(Labs == 0, "No Labs", "Labs"))
patient$Labs[is.na(patient$Labs)] <- 0

smokingStatus <-  mutate(smokingStatus, PreviousSmoker = ifelse(Description == "Current status unknown" 
                                                                | Description =="Not a current tobacco user" 
                                                                | Description == "0 cigarettes per day (non-smoker or less than 100 in lifetime)"
                                                                ,0,1))

#Number of Cigarettes
smokingStatus <-  mutate(smokingStatus, CigarettesNum = ifelse(Description == "Few (1-3) cigarettes per day",3, 
                                                               ifelse(Description == "2 or more packs per day",40,
                                                                      ifelse(Description == "Up to 1 pack per day",20, 
                                                                             ifelse(Description == "1-2 packs per day",30,
                                                                                    ifelse(Description == "Current Tobacco user",10,0))))))

#Adding Smoking Status to Patient Smoking Table
#atient$BMI <- lastVisitBMI[match(patient$PatientGuid, lastVisitBMI$PatientGuid), 6]
patientSmoking$PreviousSmoker <- smokingStatus[match(patientSmoking$SmokingStatusGuid, smokingStatus$SmokingStatusGuid),4]
patientSmoking$CigarettesNum <- smokingStatus[match(patientSmoking$SmokingStatusGuid, smokingStatus$SmokingStatusGuid),5]

#Adding Smoking data to patient table
patient$PreviousSmoker<- patientSmoking[match(patient$PatientGuid, patientSmoking$PatientGuid),5]
patient$CigarettesNum<- patientSmoking[match(patient$PatientGuid, patientSmoking$PatientGuid),6]

patient$PreviousSmoker[is.na(patient$PreviousSmoker)] <- 0
patient$CigarettesNum[is.na(patient$CigarettesNum)] <- 0
patient <- mutate(patient, PreviousSmokerDesc = ifelse(PreviousSmoker == 1, "Previous Smoker", "Non-Smoker"))
patient <- mutate(patient, CigarettesNumDesc = ifelse(CigarettesNum == 0, "0 Cigarettes per Day", 
                                                      ifelse(CigarettesNum == 20, "Up to one pack",
                                                             ifelse(CigarettesNum == 30, "1-2 Packs",
                                                                    ifelse(CigarettesNum == 40, "2 or more Packs",
                                                                           ifelse(CigarettesNum == 3,"1-3 Cigarettes" ,"Not Specified"))))))



#EDA Univariate
ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = Gender, y = ..count..)) +geom_bar(aes(fill = Gender), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = AgeDesc, y = ..count..)) +geom_bar(aes(fill = AgeDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = BPDesc, y = ..count..)) +geom_bar(aes(fill = BPDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+
  scale_x_discrete(labels = c("Normal", "Pre", "Stage I", "Stage 2"))

ggplot(patient, aes(x = BMIDesc, y = ..count..)) +geom_bar(aes(fill = BMIDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = CholesterolDesc, y = ..count..)) +geom_bar(aes(fill = CholesterolDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = LabDesc, y = ..count..)) +geom_bar(aes(fill = LabDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = MedicationDesc, y = ..count..)) +geom_bar(aes(fill = MedicationDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = AllergyDesc, y = ..count..)) +geom_bar(aes(fill = AllergyDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = PreviousSmokerDesc, y = ..count..)) +geom_bar(aes(fill = PreviousSmokerDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")

ggplot(patient, aes(x = CigarettesNumDesc, y = ..count..)) +geom_bar(aes(fill = CigarettesNumDesc), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+
  scale_x_discrete(labels = c("0 Cig", "1-2 Pa", "1-3 Cig", "2/more Pa", "NA", "Up to one Pa"))

#MultiVariate
ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~Gender)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~AgeDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~BPDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~BMIDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~CholesterolDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~AllergyDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~MedicationDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~LabDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~PreviousSmokerDesc)

ggplot(patient, aes(x = Diagnosis, y = ..count..)) +geom_bar(aes(fill = Diagnosis), position = "dodge") + scale_fill_brewer(palette="Set1")+
  geom_text(stat='count',aes(label=..count..),vjust="inward", position = "dodge")+facet_wrap(~CigarettesNumDesc)

#Splitteing data to training and testing
patientTrain <- head(patient, n = 2/3 * nrow(patient))
patientTest <- tail(patient, n = 1/3 * nrow(patient))

#Logistic Regression
set.seed(100)
logisticModel <- glm(DMIndicator~Age + BMI + Cholesterol  +Medication +Gender+ Hypertension  + BP + Labs 
                     , data = patientTrain, family = "binomial")
summary(logisticModel)
predictTest <- predict(logisticModel, type="response", newdata = patientTest)
#Confusion Matrix
conf <- table(patientTest$DMIndicator, predictTest > 0.25)
rownames(conf) <- c("Actual_Non_Diabetic", "Actual_Diabetic")
colnames(conf) <- c("Predicted_Non_Diabetic", "Predicted_Diabetic")
conf
#ROC Curve
ROCRPred <- prediction(predictTest, patientTest$DMIndicator)
perf <- performance(ROCRPred, measure = "tpr", x.measure = "fpr")
plot(perf, col = rainbow(10))
auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#LASSO Model
#Transforming variables to Factors
#Training Set
patientTrain$DMIndicator <- as.factor(patientTrain$DMIndicator)
patientTrain$Gender <- as.factor(patientTrain$Gender)
patientTrain$Cholesterol <- as.factor(patientTrain$Cholesterol)
patientTrain$Hypertension <- as.factor(patientTrain$Hypertension)
patientTrain$PreviousSmoker <- as.factor(patientTrain$PreviousSmoker)
patientTrain$CigarettesNum <- as.factor(patientTrain$CigarettesNum)
#Testing Set
patientTest$DMIndicator <- as.factor(patientTest$DMIndicator)
patientTest$Gender <- as.factor(patientTest$Gender)
patientTest$Cholesterol <- as.factor(patientTest$Cholesterol)
patientTest$Hypertension <- as.factor(patientTest$Hypertension)
patientTest$PreviousSmoker <- as.factor(patientTest$PreviousSmoker)
patientTest$CigarettesNum <- as.factor(patientTest$CigarettesNum)

#Creating factor matrix for training set
xfactors <- model.matrix(patientTrain$DMIndicator ~ patientTrain$Gender + patientTrain$Cholesterol + patientTrain$Hypertension 
                         + patientTrain$PreviousSmoker +patientTrain$CigarettesNum )[,-1]
x <- as.matrix(data.frame(patientTrain$Age, patientTrain$BMI, patientTrain$BP, patientTrain$Medication, patientTrain$Labs, xfactors))

#The LASSO Model
glmmod<-glmnet(x,y=as.factor(patientTrain$DMIndicator),alpha=0.2,family='binomial')
plot(glmmod,xvar="lambda")
grid()
coef <-coef(glmmod)[,10]

#Best Lambda
cv.glmmod <- cv.glmnet(x,y=patientTrain$DMIndicator,alpha=1, family="binomial")
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min
best_lambda
data.frame(coef)

#Factor Matrix for testing st
xTestfactors <- model.matrix(patientTest$DMIndicator ~ patientTest$Gender + patientTest$Cholesterol + patientTest$Hypertension 
                             + patientTest$PreviousSmoker +patientTest$CigarettesNum )[,-1]

xTest <- as.matrix(data.frame(patientTest$Age, patientTest$BMI, patientTest$BP, patientTest$Medication, patientTest$Labs, xTestfactors))

#Prediction 
predictLasso <-predict(glmmod, s=best_lambda, newx = xTest, type="response")
predictLasso
summary(predictLasso)

#Confusion Matrix
confLasso <- table(patientTest$DMIndicator, predictLasso >0.25)
rownames(confLasso) <- c("Actual_Non_Diabetic", "Actual_Diabetic")
colnames(confLasso) <- c("Predicted_Non_Diabetic", "Predicted_Diabetic")
str(conf)
confLasso

#ROC Curve
ROCRPredLasso<- prediction(predictLasso, patientTest$DMIndicator)
perf <- performance(ROCRPredLasso, measure = "tpr", x.measure = "fpr")
plot(perf, col = rainbow(10))
aucLasso <- performance(ROCRPredLasso, measure = "auc")
aucLasso <- aucLasso@y.values[[1]]
aucLasso


