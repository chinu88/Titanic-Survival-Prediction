#========================Titanic===========================
library(ggplot2)
library(dplyr)
setwd("~/Documents/Datascience-R/Kaggle/titanic")
train<-read.csv("train.csv",stringsAsFactors = T)
test<-read.csv("test.csv",stringsAsFactors = T)

full<-bind_rows(train,test)

#=================Exploratory Analysis======================
#===========================================================

#To take initial idea about the data set

str(full)
summary(full)

#Convertion of variables to factors

full$Survived<-as.factor(full$Survived)
full$Pclass<-as.factor(full$Pclass)
full$Sex<-as.factor(full$Sex)
full$Embarked<-as.factor(full$Embarked)


#Univariant Analysis

ggplot(data=full[1:891,],aes(x=Survived,fill=Survived))+geom_bar()
ggplot(data=full[1:891,],aes(x=Age))+geom_histogram(fill="skyblue")
ggplot(data=full[1:891,],aes(x=Pclass))+geom_bar(fill="lightgreen")
ggplot(data=full[1:891,],aes(x=Embarked))+geom_bar(fill="lightgreen") #observation:Blanks present in Embarked

#Bivariant Analysis

ggplot(data=full[1:891,],aes(x=Sex,fill=Survived))+geom_bar() #Observation:Probablity of death of male is high
ggplot(data=full[1:891,],aes(x=Pclass,fill=Survived))+geom_bar() #Observation:Probablity of death in Pclass is high
ggplot(data=full[1:891,],aes(x=Age,fill=Survived))+geom_histogram()

#Multivariant Analysis

ggplot(data=full[1:891,],aes(x=Age,fill=Survived))+geom_histogram()+facet_grid(~Sex)#Probablity of death in Male is high
ggplot(data=full[1:891,],aes(x=Age,y=Pclass,colour=Survived))+geom_jitter()+facet_grid(~Sex)#Female in Class 1,2 are having high chance of survival

#=================Missing Value treatment==================
#in summary(Full) you can observe Missing values
#Embarkerd:2(blanks)

full$Embarked[full$Embarked==""]<-"S" #As count of "S" is highest between "C","Q"&"S"
summary(full$Embarked)

#Fare:1 Missing Value
full$Fare[is.na(full$Fare)]<-median(full$Fare,na.rm=TRUE)
summary(full$Fare)

#Age:263 Missing Value
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)# Extract title from Name & assign to a New variable :Title
table(full$Title,full$Sex)#To relate title vs Sex
full$Title[full$Title %in% c("Mlle","Mme","Ms")]<-"Miss"
full$Title[full$Title %in% c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major","Master","Rev","Sir","the Countess")]<-"Special"

library(data.table)
library(zoo)
setDT(full)[, Age := na.aggregate(Age,FUN=median) , by = Title]#Finds Na & assigns median of Age of that particular Title
summary(full$Age)

#New Variable Family Size
full$Fsize<-full$SibSp+full$Parch+1
ggplot(data=full[1:891,],aes(x=Fsize,fill=Survived))+geom_bar(stat="count",position="dodge")


#========Model Building===========
#Randomforest
install.packages("randomForest")
library(randomForest)
train<-full[1:891,]
test<-full[892:1309,]

set.seed(100)
rf_model<-randomForest(Survived~Pclass+Sex+Age+Title+Fare+Fsize,data=train)

rf_model

varImpPlot(rf_model) #variable importance mapping
test$Survived<-predict(rf_model,newdata=test)
titanic_prediction1<-test[,1:2]
write.csv(titanic_prediction1,file = "titanic_prediction1.csv",row.names = F)

#=================END=============




