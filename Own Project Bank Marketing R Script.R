#################Install all needed packages if not present

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")
if(!require(corrplot)) install.packages("corrplot")
if(!require(data.table)) install.packages("data.table")
if(!require(Amelia)) install.packages("Amelia")
if(!require(ROCR)) install.packages("ROCR")
if(!require(caTools)) install.packages("caTools")
if(!require(e1071)) install.packages("e1071")
if(!require(randomForest)) install.packages("randomForest")
if(!require(class)) install.packages("class")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")



###Loading all the needed libraries
library(tidyverse)
library(caret)
library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ROCR)
library(Amelia)
library(caTools)
library(e1071)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)

options(digits  = 4)


##########1. INITIAL DATA EXPLORATION###################################################
######1.1 Loading the Data set######

bank_marketing<-read.csv(
  "https://raw.githubusercontent.com/mgaur2009/Capstone-Own-Project/master/bank.csv")

######1.2 Viewing the data set####
head(bank_marketing)
str(bank_marketing)
summary(bank_marketing)


####1.3 checking for missing or NA values####
sapply(bank_marketing,function(df){
  sum(is.na(df)==TRUE)
})

missmap(bank_marketing,main = "Missing map")


#######1.4 Attribute Selection################################
####Looking at the previous campaign related attributes###
bank_marketing%>%group_by(pdays)%>%
  summarize(n=n())%>%
  arrange(desc(n))%>%head()%>%
  knitr::kable()

table(bank_marketing$poutcome)

table(bank_marketing$previous)%>%head()


####Removing the attributes from previous marketing campaign and contact attribute######
bank_marketing<-bank_marketing%>%select(-c("contact","pdays","poutcome","previous"))


#########2.Feature Engineering##############################################################

### 2.1 Converting the month variable to quarter variable########
table(bank_marketing$month)

bank_marketing$month<-as.factor(bank_marketing$month)

levels(bank_marketing$month)[c(4,5,8)] <- c("Qtr1_jan-march")
levels(bank_marketing$month)[c(1,7,6)] <- c("Qtr2_apr-june")
levels(bank_marketing$month)[c(2,5,8)] <- c("Qtr3_july-sep")
levels(bank_marketing$month)[c(3,5,6)] <- c("Qtr4_oct-dec")

colnames(bank_marketing)[c(10)]<-c("Qtr")


####2.2 Removing the 'unknown' values in 'job' variable###
table(bank_marketing$job)

bank_marketing<-bank_marketing%>%filter(job!="unknown")

######2.3 Conversion of 'unknown' category to 'secondary' category which is most frequent######

table(bank_marketing$education)

bank_marketing$education<-factor(bank_marketing$education)
levels(bank_marketing$education)[c(4)]<-c("secondary")
levels(bank_marketing$education)
table(bank_marketing$education)



########2.4 Changing the categorical variables to factor class#########
bank_marketing$job<-factor(bank_marketing$job)
bank_marketing$marital<-factor(bank_marketing$marital)
bank_marketing$default<-factor(bank_marketing$default)
bank_marketing$housing<-factor(bank_marketing$housing)
bank_marketing$loan<-factor(bank_marketing$loan)
bank_marketing$deposit<-factor(bank_marketing$deposit)




########2.5 Renaming categories for housing, loan and deposit variables########
levels(bank_marketing$housing)
table(bank_marketing$housing)
levels(bank_marketing$housing)[c(1,2)]<-c("no_housing_loan","housing_loan")
levels(bank_marketing$housing)
table(bank_marketing$housing)

levels(bank_marketing$loan)
table(bank_marketing$loan)
levels(bank_marketing$loan)[c(1,2)]<-c("no_personal_loan","personal_loan")
levels(bank_marketing$loan)
table(bank_marketing$loan)

levels(bank_marketing$deposit)
table(bank_marketing$deposit)
levels(bank_marketing$deposit)[c(1,2)]<-c("not_subscribed","subscribed")
levels(bank_marketing$deposit)
table(bank_marketing$deposit)


####################3. EXPLORATORY DATA ANALYSIS########################################
##############3.1 Target variable- deposit###########
ggplot(bank_marketing)+geom_bar(aes(deposit,fill=deposit))+
  scale_fill_manual(values = c("tomato","springgreen3"))+theme_bw()


######################3.2 marital status########
##marital and Deposit###

bank_marketing%>%ggplot(aes(marital))+
  geom_bar(aes(fill=deposit))+
  scale_fill_manual(values = c("tomato2","springgreen2"))+
  theme_bw()

prop.table(table(bank_marketing$marital))


###################3.3 Job###############

#####Job and Deposit####
bank_marketing%>%ggplot(aes(job))+
  geom_bar(aes(fill=deposit))+
  scale_fill_discrete()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))


####################3.4 Education##########
####Education and Deposit###
bank_marketing%>%ggplot(aes(education))+
  geom_bar(aes(fill=deposit))+
  scale_fill_manual(values = c("tomato1","springgreen3"))+
  theme_bw()

prop.table(table(bank_marketing$education))


###################3.5 Qtr###################
####Qtr and Deposit###

bank_marketing%>%ggplot(aes(Qtr))+
  geom_bar(aes(fill=deposit))+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()

prop.table(table(bank_marketing$Qtr))        


####################3.6 Age###################

ggplot(bank_marketing,aes(age))+
  geom_histogram(bins = 40,alpha=1,fill="springgreen3")+
  theme_bw()+geom_vline(aes(xintercept = mean(age)))+
  geom_text(x=45,y=850,label="Mean Age")+
  geom_vline(aes(xintercept=median(age)),linetype=2)+
  geom_text(x=39,y=750,label="Median Age")+
  scale_x_continuous(breaks=seq(min(0),max(100),by=10))


mean(bank_marketing$age)
median(bank_marketing$age)
sum(bank_marketing$age>60)/length(bank_marketing$age)


#######age and deposit###
ggplot(bank_marketing,aes(age,fill=deposit))+
  scale_fill_manual(values = c("tomato1","springgreen3"))+
  geom_histogram(bins=30)+
  facet_grid(deposit~.)+theme_bw()

ggplot(bank_marketing,aes(deposit,age))+
  geom_boxplot(aes(fill=deposit))+theme_bw()


#########################3.7 Campaign################
ggplot(bank_marketing)+
  geom_bar(aes(campaign,fill=campaign),fill="blue")+
  scale_x_continuous(breaks=seq(min(1),max(63),by=2))+
  scale_fill_brewer(palette = "Set1")+theme_bw()

prop.table(table(bank_marketing$campaign))%>%head()


#####################3.8 housing loan################

ggplot(bank_marketing)+geom_bar(aes(housing,fill=housing))+
  scale_fill_brewer(palette = "Set1")+theme_bw()

prop.table(table(bank_marketing$housing))


#####Housing and deposit####
bank_marketing%>%ggplot(aes(housing,fill=deposit))+
  geom_bar()+facet_grid(deposit~.,scales = "free")+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()


###############3.9 Personal Loan##################

bank_marketing%>%ggplot(aes(loan,fill=deposit))+
  geom_bar()+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()

prop.table(table(bank_marketing$loan))


#############3.10 Balance and Default and Deposit######

ggplot(bank_marketing,aes(default,balance,fill=deposit))+
  geom_boxplot(width=1)+
  scale_fill_manual(values = c("tomato2","springgreen3"))+
  theme_bw()

#########3.11 Visualization of Age, Balance and Deposit######

ggplot(bank_marketing,aes(age,balance))+
  geom_bar(aes(fill=deposit),binwidth=30,stat = "identity")+
  scale_x_continuous(breaks=seq(min(0),max(100),by=10))+
  scale_fill_manual(values = c("tomato","springgreen3"))+
  theme_bw()


ggplot(bank_marketing,aes(age,balance))+
  geom_point(aes(color=deposit),alpha=1,size=1)+
  scale_x_continuous(breaks=seq(min(0),max(100),by=10))+
  theme_bw()


###################3.12 Duration##########################

ggplot(bank_marketing,aes(duration))+
  geom_histogram(bins = 50,alpha=1,fill="red")+
  scale_x_continuous(breaks=seq(min(0),max(4000),by=200))+
  theme_bw()+
  geom_vline(aes(xintercept = mean(duration)))+
  geom_text(x=500,y=1500,label="Mean Duration=372")+
  geom_vline(aes(xintercept=median(duration)),linetype=2)+
  geom_text(x=200,y=750,label="Median Duration=255")


mean(bank_marketing$duration)

median(bank_marketing$duration)

ggplot(bank_marketing,aes(deposit,duration))+
  geom_boxplot(aes(fill=deposit))+theme_bw()

#######Removing the 'duration' variable from the data set#######
bank_marketing<-bank_marketing%>%select(-c("duration"))

########Final Data set used for Modeling Analysis######
str(bank_marketing)
head(bank_marketing)

####################4.MODELING APPROACH#####################################
#####4.1 Creating train and test set by 90/10 partition#####
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = bank_marketing$deposit, times = 1, p = 0.1, list = FALSE)
train_set <- bank_marketing[-test_index,]
test_set<- bank_marketing[test_index,]


######################5. MODELING METHODS AND ANALYSIS######################
#####5.1 Logistic Regression using all predictors####
##5.1.1 Training and evaluation of the model using all predictors###
set.seed(1,sample.kind = "Rounding")
glm_model<-train(deposit~.,method="glm",data=train_set)
glm_hat<-predict(glm_model,test_set)

###Summary of the model#####
summary(glm_model)

###ConfusionMatrix###
confusionMatrix(glm_hat,test_set$deposit,positive = "subscribed")

####5.1.2 Results of Logistic Regression Model####
cm_logit<-confusionMatrix(glm_hat,test_set$deposit,positive = "subscribed")
accuracy_logit_all<-cm_logit$overall["Accuracy"]
sensitivity_logit_all<-cm_logit$byClass["Sensitivity"]
specificity_logit_all<-cm_logit$byClass["Specificity"]

######5.1.3 Model Evaluation Metrices#########
####Accuracy####
accuracy_logit_all

#####Sensitivity####
sensitivity_logit_all

####Prediction Table####
table(glm_hat,test_set$deposit)


########5.1.4 Results Table##################
results<-tibble(Method="Logistic Regression",
                Accuracy=accuracy_logit_all,
                Sensitivity=sensitivity_logit_all,
                Specificity=specificity_logit_all)

results%>%knitr::kable()


##########5.2 SVM(Support Vector Machine)########################
###5.2.1 Training and Evaluation of Model######
set.seed(1,sample.kind = "Rounding")
svm_model<-svm(deposit~.,data = train_set,method="class")
svm_pred<-predict(svm_model,newdata = test_set,type="class")

###summary of the model###
summary(svm_model)


##########5.2.2 tuning the SVM model###########
svm_tune<-tune(svm,train.x = deposit~.,data = train_set,
               kernel="radial",
               ranges = list(cost=c(.1,1),gamma=c(.5,1)))

######summary of tuned SVM model####
summary(svm_tune)

######5.2.3 Training and Evaluation using tuned SVM parameters#####
set.seed(1,sample.kind = "Rounding")
svm_tune_model<-svm(deposit~.,data = train_set,cost=.1,gamma=0.5,method="class")
svm_tune_pred<-predict(svm_tune_model,newdata = test_set,type="class")

summary(svm_tune_model)

########5.2.4 Results of SVM Model############
cm_svm_tune<-confusionMatrix(svm_tune_pred,test_set$deposit,positive = "subscribed")
accuracy_svm<-cm_svm_tune$overall["Accuracy"]
sensitivity_svm<-cm_svm_tune$byClass["Sensitivity"]
specificity_svm<-cm_svm_tune$byClass["Specificity"]


######Confusion Matrix####
cm_svm_tune

#####Prediction Table####
table(svm_tune_pred,test_set$deposit)

#######5.2.5 SVM Model Evaluation Metrices############
#####OVerall Accuracy####
accuracy_svm

####Sensitivity###
sensitivity_svm


#########5.2.6 Updating Results Table######
results<-results%>%add_row(Method="SVM",
                           Accuracy=accuracy_svm,
                           Sensitivity=sensitivity_svm,
                           Specificity=specificity_svm)


results%>%knitr::kable()


#####################5.3 KNN (K Nearest Neighbors)##########################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

###5.3.1 Building the model########
knn_model<-train(deposit ~ .,
                 method = "knn",
                 preProcess=c("center","scale"),
                 data = train_set,
                 tuneGrid = data.frame(k = seq(1, 53, 2)),
                 trControl = trainControl(method = "cv", number = 10, p = 0.9))

####Making predictions using KNN model####
knn_pred<-predict(knn_model,test_set)

knn_model

###5.3.2 Plotting the Accuracy obtained with the model with different values of k####
ggplot(knn_model, highlight = TRUE)
knn_model$bestTune


##################5.3.3 Results of KNN Model#########################
cm_knn<-confusionMatrix(knn_pred,test_set$deposit,positive = "subscribed")
accuracy_knn<-cm_knn$overall["Accuracy"]
sensitivity_knn<-cm_knn$byClass["Sensitivity"]
specificity_knn<-cm_knn$byClass["Specificity"]

#####Confusion Matrix####
cm_knn

####Prediction Table
table(knn_pred,test_set$deposit)

######5.3.4 KNN Model Evaluation Metrices####

####Overall Accuracy###
accuracy_knn

#####Sensitivity###
sensitivity_knn

#########5.3.5 Updating the Results Table####
results<-results%>%add_row(Method="KNN",
                           Accuracy=accuracy_knn,
                           Sensitivity=sensitivity_knn,
                           Specificity=specificity_knn)


results%>%knitr::kable()



##################5.4 RPART Decision Tree###########################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

###5.4.1 Training the model###
rpart_model<-rpart(deposit~.,data = train_set,method = "class")

###Making predictions using the model###
rpart_pred<-predict(rpart_model,newdata = test_set,type = "class")


#########5.4.2 Tree Pruning######
printcp(rpart_model)
plotcp(rpart_model)
min(rpart_model$cptable[,"xerror"])

which.min(rpart_model$cptable[,"xerror"])

####Complexity Paramter that minimized the xerror###
cpmin<-rpart_model$cptable[4,"CP"]
cpmin

######Pruned RPART model with optimum cp(Complexity Parameter)####
rpart_pruned<-prune(rpart_model,cp=cpmin)


#########Plot of Tuned RPART (Decision Tree) Model####
rpart.plot(rpart_pruned)

######Evaluation with test set using pruned RPART model#####
prune_pred<-predict(rpart_pruned,newdata = test_set,type = "class")


#######5.4.3  Results of RPART model########
cm_rpart<-confusionMatrix(prune_pred,test_set$deposit,positive = "subscribed")
accuracy_prune<-cm_rpart$overall["Accuracy"]
sensitivity_prune<-cm_rpart$byClass["Sensitivity"]
specificity_prune<-cm_rpart$byClass["Specificity"]


#####Confusion Matrix####
cm_rpart

#####Prediction Table####
table(prune_pred,test_set$deposit)

######5.4.4 RPART Model Evaluation metrices######
######OVerall Accuracy#####
accuracy_prune

######Sensitivity####
sensitivity_prune


#######5.4.5 Updating the Results table######################
results<-results%>%add_row(Method="RPART Decision Tree",
                           Accuracy=accuracy_prune,
                           Sensitivity=sensitivity_prune,
                           Specificity=specificity_prune)


results%>%knitr::kable()


#####################5.5 Random Forest#################

set.seed(42,sample.kind = "Rounding")

###5.5.1 Training and evaluating the model###

rf_model<-randomForest(deposit~.,data=train_set,importance=TRUE,ntree=520, mtry=3)

###Making predictions using the model###

rf_pred<-predict(rf_model,test_set)

rf_model


####5.5.2 Plot of error rate and ntree#########

plot(rf_model)

######Plot of importance of variables#####
varImpPlot(rf_model)

###############5.5.3 Results of Random Forest Model##############
cm_rf<-confusionMatrix(rf_pred,test_set$deposit,positive = "subscribed")
accuracy_rf<-cm_rf$overall["Accuracy"]
sensitivity_rf<-cm_rf$byClass["Sensitivity"]
specificity_rf<-cm_rf$byClass["Specificity"]


#####Confusion Matrix######
cm_rf

#####Prediction Table####
table(rf_pred,test_set$deposit)


#######5.5.4 RF Model Evaluation metrices#####
###Overall Accuracy###
accuracy_rf

####Sensitivity####
sensitivity_rf

#########5.5.6 Updating the Results Table##############
results<-results%>%add_row(Method="Random Forest",
                           Accuracy=accuracy_rf,
                           Sensitivity=sensitivity_rf,
                           Specificity=specificity_rf)


results%>%knitr::kable()


############6. Final table for performance metrics for the five models used#########
final_evaluation_table<-tibble(method=c("Logistic Regression","SVM","KNN", 
                                        "RPART","Random Forest"),
                               Accuracy=c(accuracy_logit_all,accuracy_svm,
                                          accuracy_knn,
                                          accuracy_prune,accuracy_rf),
                               Sensitivity=c(sensitivity_logit_all,
                                             sensitivity_svm,sensitivity_knn,
                                             sensitivity_prune,sensitivity_rf))


final_evaluation_table%>%knitr::kable(caption = "Evaluation Matrics Table")
