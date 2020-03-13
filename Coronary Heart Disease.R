#COMP5310 Project 1
#Load Required Packages
install.packages('bindrcpp')
install.packages('curl')
install.packages('dplyr')
install.packages('plotly')
install.packages('tidyverse')
install.packages('scales')
install.packages('recipes')
install.packages('caret', dependencies = TRUE)
install.packages('corrplot')
install.packages('gridExtra')
install.packages('reshape2')
install.packages('tidyverse')
install.packages("modeest")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("genefilter", version = "3.8")

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(scales)
library(caret)
library(corrplot)
library(curl)
library(gridExtra)
library(reshape2)
library(pROC)
library(modeest)
library(gbm)
library(mice)
library(neuralnet)

#Read in Data from UCI machine learning Repository
heart_data <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",")
glimpse(heart_data)

nrow(heart_data)

#Set column names:
colnames(heart_data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")

#All variables numeric except ca and thal: 
#Variables ca and thal appearing as characters due to the presence of non-numeric values in the column:
#Lets investigate:

unique(heart_data$ca)  #  "0.0" "3.0" "2.0" "1.0" "?" 
unique(heart_data$thal) # "6.0" "3.0" "7.0" "?"


#Both columns include a question mark: convert to NA before modelling

#Value with 0 means ok, value with 1,2,3,4:heart disease
#Add Extra Column in with this specification: 
heart_data$hd_indicator<-ifelse(heart_data$num==0,0,1)

#Explore Response Variable: 

#creating a summary table for response variable
summarize_response<-heart_data %>% select(hd_indicator)  %>% mutate(hd_indicator_flag=ifelse(hd_indicator==0,"NO","YES"))%>%count(hd_indicator_flag)%>%mutate(percent = n/sum(n))

#Plot this as a histogram:
plot_response<-(ggplot(summarize_response, aes(x=hd_indicator_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
                +geom_label(aes(label = percent(percent)), color = "white", vjust = 1, show.legend = FALSE)
                +labs(title="Histogram for Presence of Heart Disease", x="Presence of Heart Disease", y="Percentage")
                +scale_y_continuous(labels = percent,limits=c(0,1)))
plot_response

#Specify appropriate data type for factor variables. 
heart_data$sex=as.factor(heart_data$sex)
heart_data$cp=as.factor(heart_data$cp)
heart_data$fbs=as.factor(heart_data$fbs)
heart_data$restecg=as.factor(heart_data$restecg)
heart_data$exang=as.factor(heart_data$exang)
heart_data$slope=as.factor(heart_data$slope)
heart_data$ca=as.factor(heart_data$ca)
heart_data$thal=as.factor(heart_data$thal)
heart_data$hd_indicator=as.factor(heart_data$hd_indicator)


#Change missing values to NA in ca and thal: 
heart_data[heart_data=='?']<-NA
#Checking the rows which contain NA:
heart_data[rowSums(is.na(heart_data)) > 0,]

#Lets view a histogram for each of our numeric variables: 

#Create a data table just for our numeric variables 
numerical_data=heart_data[,.(age,trestbps,chol,thalach,oldpeak)]

#Histograms for numeric variables 
hist_num<-ggplot(melt(numerical_data), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_histogram(bins = 20)

#Density Plots for numeric variables
dens_num<-ggplot(melt(numerical_data), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_density()

#We see that age is left skewed, trestbps is right skewed, chol is right skewed, thalach is left skewed, oldpeak is right skewed.

#Box Plots for numeric variables  
numerical_data %>% gather(key = "var", value = "value")%>% ggplot(aes(x = var,y=value)) +geom_boxplot() + facet_wrap(~ var, scales = "free")+theme_bw()
#Cholestral, oldpeak, trestbps all have a few outliers

#Correlation Plot: 
corrplot(cor(numerical_data,method="spearman"),method="circle") 

#Correlations between numeric variables seem quite low: strong multicolinearity not present

#Explore Factor Variables: 
#Sex: 
summarize_sex<-heart_data %>% select(sex)  %>% mutate(sex_flag=ifelse(sex==1,"Male","Female"))%>%count(sex_flag)%>%mutate(percent = n/sum(n))
plot_sex<-(ggplot(summarize_sex, aes(x=sex_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
                +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
                +labs(title="Sex", x="Sex", y="Percentage")
                +scale_y_continuous(labels = percent,limits=c(0,1))
                +scale_fill_continuous(labels = percent,name= "Percentage"))
#Fbs
summarize_fbs<-heart_data %>% select(fbs)  %>% mutate(fbs_flag=ifelse(fbs==1,"fbs>120 mg/dl","fbs<120 mg/dl"))%>%count(fbs_flag)%>%mutate(percent = n/sum(n))

plot_fbs<-(ggplot(summarize_fbs, aes(x=fbs_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
           +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
           +labs(title="Fbs", x="FBS level", y="Percentage")
           +scale_y_continuous(labels = percent,limits=c(0,1))
           +scale_fill_continuous(labels = percent,name= "Percentage"))

#Cp:
summarize_cp<-heart_data %>% select(cp) %>% mutate(cp_flag=sapply(cp, function(x) if(x==1) {"Typ. ang"} else if (x==2) {"Atyp ang."} else if (x==3) {"Non-ang"} else if (x==4) {"asymptomatic"} else {NULL}))%>%count(cp_flag)%>%mutate(percent = n/sum(n))

plot_cp<-(ggplot(summarize_cp, aes(x=cp_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
           +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
           +labs(title="Cp", x="cp type", y="Percentage")
           +scale_y_continuous(labels = percent,limits=c(0,1))
           +scale_fill_continuous(labels = percent,name= "Percentage"))
#restecg:
summarize_restecg<-heart_data %>% select(restecg) %>% mutate(restecg_flag=sapply(restecg, function(x) if(x==0) {"Normal"} else if (x==1) {"abnormal"} else if (x==2) {"VP"}else {NULL}))%>%count(restecg_flag)%>%mutate(percent = n/sum(n))

plot_restecg<-(ggplot(summarize_restecg, aes(x=restecg_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
          +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
          +labs(title="restecg", x="RESULT", y="Percentage")
          +scale_y_continuous(labels = percent,limits=c(0,1))
          +scale_fill_continuous(labels = percent,name= "Percentage"))

#exang:
summarize_exang<-heart_data %>% select(exang) %>% mutate(exang_flag=ifelse(exang==1,"yes","no"))%>%count(exang_flag)%>%mutate(percent = n/sum(n))

plot_exang<-(ggplot(summarize_exang, aes(x=exang_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
               +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
               +labs(title="exang", x="presence", y="Percentage")
               +scale_y_continuous(labels = percent,limits=c(0,1))
               +scale_fill_continuous(labels = percent,name= "Percentage"))

#slope:
summarize_slope<-heart_data %>% select(slope) %>% mutate(slope_flag=sapply(slope, function(x) if(x==1) {"upsloping"} else if (x==2) {"flat"} else if (x==3) {"downsloping"} else {NULL}))%>%count(slope_flag)%>%mutate(percent = n/sum(n))

plot_slope<-(ggplot(summarize_slope, aes(x=slope_flag,y=percent,fill=percent))+geom_bar(stat = "identity")
               +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
               +labs(title="slope", x="slope result", y="Percentage")
               +scale_y_continuous(labels = percent,limits=c(0,1))
               +scale_fill_continuous(labels = percent,name= "Percentage"))

#ca:
summarize_ca<-heart_data %>% select(ca) %>%count(ca)%>%mutate(percent = n/sum(n))

plot_ca<-(ggplot(summarize_ca, aes(x=ca,y=percent,fill=percent))+geom_bar(stat = "identity")
               +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
               +labs(title="ca", x="Number", y="Percentage")
               +scale_y_continuous(labels = percent,limits=c(0,1))
               +scale_fill_continuous(labels = percent,name= "Percentage"))

#thal:
summarize_thal<-heart_data %>% select(thal) %>%count(thal)%>%mutate(percent = n/sum(n))

plot_thal<-(ggplot(summarize_thal, aes(x=thal,y=percent,fill=percent))+geom_bar(stat = "identity")
               +geom_label(aes(label = percent(percent)), color = "white", vjust = 0, show.legend = FALSE)
               +labs(title="thal", x="RESULT", y="Percentage")
               +scale_y_continuous(labels = percent,limits=c(0,1))
               +scale_fill_continuous(labels = percent,name= "Percentage"))

grid.arrange(plot_sex, plot_fbs,plot_cp,plot_restecg,nrow = 2,ncol=2)
grid.arrange(plot_exang,plot_slope,plot_ca,plot_thal,nrow=2,ncol=2)

########### Explore Response across different levels of age and gender ###############
#GENDER: Create a summary table for heart disease broken down by sex: 
summarize_response_gender<-heart_data %>% select(hd_indicator,sex) %>% mutate(hd_indicator_flag=ifelse(hd_indicator==0,"NO","YES"),sex_flag=ifelse(sex==1,"Male","Female"))%>%count(hd_indicator_flag,sex_flag)%>%group_by(sex_flag)%>%mutate(percent = n/sum(n))

#Plot this as a histogram: 
plot_response_gender<-(ggplot(summarize_response_gender, aes(x = sex_flag, y = percent, fill = hd_indicator_flag))
+geom_bar(stat = "identity")
+geom_label(aes(label = percent(percent)), col="white",position = position_stack(),vjust=1,show.legend = FALSE)
+labs(title="Histogram for Presence of Heart Disease split by gender", x="Gender", y="Percentage")
+scale_y_continuous(labels = percent,limits=c(0,1))
+scale_fill_manual(values = c("darkgreen", "darkred"),name= "Heart Disease Presence")
+theme(legend.background = element_rect(fill = "lightgray"),legend.key = element_rect(fill = "lightblue")))

####################################################################
#Apply transformations: centre and scale numeric variables:
preprocobj<-preProcess(heart_data[,.(age,trestbps,chol,thalach,oldpeak)], method = c("center","scale"))
modeldata<-predict(preprocobj,heart_data)
modeldata$num<-NULL
summary(modeldata)
glimpse(modeldata)

#Check Histograms for numeric variables 
ggplot(melt(modeldata[,.(age,trestbps,chol,thalach,oldpeak)]), aes(x = value)) + 
  facet_wrap(~ variable, scales = "free", ncol = 2) + 
  geom_histogram(bins = 20)

glimpse(heart_data)
glimpse(modeldata)

##############################ADDITIONAL ANALYSIS####################

#AGE: Create a summary table for heart disease broken down by age categories: 

#first some exploration of age:
min(heart_data$age) #29
max(heart_data$age) #77
summarize_response_age<-heart_data  %>% select(hd_indicator,age)   %>% mutate(hd_indicator_flag=ifelse(hd_indicator==0,"NO","YES"),age_category=sapply(age, function(x) if(x < 35) "under 35" else if (35<=x && x<50) {"35 to 50"} else if (50<=x && x<60) {"50 to 60"} else if (60<=x && x<70) {"60 to 70"} else if (70<=x && x<80) {"70 to 80"} else {NULL}))%>%count(hd_indicator_flag,age_category)%>%group_by(age_category)%>%mutate(percent = n/sum(n))

#Plot this as a histogram: 
plot_response_age<-(ggplot(summarize_response_age, aes(x = age_category, y = percent, fill = hd_indicator_flag))
+geom_bar(stat = "identity")
+geom_label(aes(label = percent(percent)), color = "white",position = position_stack(),vjust = 1,show.legend = FALSE)
+labs(title="Histogram for Presence of Heart Disease split by age", x="age", y="Count")
+scale_y_continuous(labels = percent,limits=c(0,1))
+scale_fill_manual(values = c("darkgreen", "darkred"),name= "Heart Disease Presence")
+theme(legend.background = element_rect(fill = "lightgray"),legend.key = element_rect(fill = "lightblue")))

modified_heart_data<-modeldata[rowSums(is.na(heart_data)) == 0,]
glimpse(modified_heart_data)


##########Applying an imputation method##########################333

heart_data_missing<-modeldata[rowSums(is.na(modeldata)) > 0,]
install.packages("mice")
library(mice)

glimpse(modeldata)
unique(modeldata$ca)
imputed_data<-mice(data = modeldata, m = 5, method = "polyreg", maxit = 10, seed = 10)
ca_imputed<-setDT(imputed_data$imp$ca,keep.rownames = TRUE)
thal_imputed<-setDT(imputed_data$imp$thal,keep.rownames = TRUE)

imputed_vector_ca<-apply(ca_imputed[ ,2:length(ca_imputed)], 1, mfv)
imputed_vector_thal<-apply(thal_imputed[ ,2:length(thal_imputed)], 1, mfv)

modeldata[,"ca"][rowSums(is.na(modeldata[,"ca"])) > 0,]<-imputed_vector_ca
modeldata[,"thal"][rowSums(is.na(modeldata[,"thal"])) > 0,]<-imputed_vector_thal

glimpse(modeldata)
modeldata[rowSums(is.na(modeldata))>0,]

##########################Establishing a train/test split and cross validation approach##############################
set.seed(123)

inTraining <- createDataPartition(modeldata$hd_indicator, p = .75, list = FALSE)
training <- modeldata[ inTraining,]
testing  <- modeldata[-inTraining,]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
  )

##########################Applying Machine Learning Methods##############################

###################################### GBM ############################################
set.seed(123)
#Tuning Grid
gbmGrid <-  expand.grid(interaction.depth =  c(1,2,3),
                        n.trees = seq(50,250,by=50),
                        shrinkage = 0.05,
                        n.minobsinnode =5)

#To prevent overfitting: find the parameters from the training set that generalise best on the test set (in terms of accuracy)
gbm_row_vec<-seq(1,nrow(gbmGrid),1)
gbm_train_accuracy_scores<-c()
gbm_predictions_list<-list()
gbm_prediction_prob_list<-list()
for (row in row_vec){
prediction_fit<- train(hd_indicator~., data = training, 
                  method = "gbm", 
                  trControl = fitControl,
                  tuneGrid = gbmGrid[row,],
                  verbose = FALSE)  
training_accuracy_scores<-as.vector(prediction_fit$results['Accuracy'][1,])
pred<-predict(prediction_fit,newdata=testing) 
pred_prob<-predict(prediction_fit,newdata=testing,type = "prob")
gbm_train_accuracy_scores[row]<-training_accuracy_scores
gbm_predictions_list[[row]]<-pred
gbm_prediction_prob_list[[row]]<-pred_prob
}

#To prevent overfitting: find the parameters from the training set that generalise best on the test set. 
gbm_Test_Accuracy_Scores<-c()
i<-0
for (predictions in gbm_predictions_list){
i<-i+1
confusion<-confusionMatrix(testing$hd_indicator, predictions, positive = NULL, dnn = c("Predicted", "Actual"))  
Accuracy<-confusion$overall['Accuracy']  
gbm_Test_Accuracy_Scores[i]<-Accuracy
}

#Creating a training/test comparison table

setDT(gbmGrid,keep.rownames = TRUE)
gbmGrid$Training_Accuracy<-gbm_train_accuracy_scores
gbmGrid$Test_Accuracy<-gbm_Test_Accuracy_Scores

#Plotting avg.train/test accuracy by number of trees:

gbm_training_Test_tree_dt<-gbmGrid[, lapply(.SD,mean), by=n.trees,
   .SDcols = c("Training_Accuracy","Test_Accuracy")]

dd_gbm = melt(gbm_training_Test_tree_dt, id=c("n.trees"))

plots_training_Test_gbm<-ggplot(dd_gbm) + geom_line(aes(x=n.trees, y=value, colour=variable)) +scale_colour_manual(values=c("red","blue"))+labs(title="Average GBM accuracy across different n.trees", x="n.trees", y="Average Accuracy")

#Finding optimal Hyperparameters 
best_test_rows_gbm<-gbmGrid[Test_Accuracy==max(gbmGrid$Test_Accuracy)]
best_training_test_rows_gbm<-best_test_rows_gbm[Training_Accuracy==max(Training_Accuracy)]

#Evaluating this: 
best_predictions_gbm<-gbm_predictions_list[[as.numeric(best_training_test_rows_gbm$rn)]]
best_predictionsprob_gbm<-gbm_prediction_prob_list[[as.numeric(best_training_test_rows_gbm$rn)]]
best_confusionmatrix_gbm<-confusionMatrix(testing$hd_indicator, best_predictions_gbm, positive = NULL, dnn = c("Predicted", "Actual"))
best_ROC_gbm<-roc(testing$hd_indicator,best_predictionsprob_gbm$`1`)
PLot_ROC_gbm<-ggroc(best_ROC_gbm,alpha = 0.5, colour = "red", linetype = "solid", size = 2) +labs(title="ROC Curve GBM", x="Specificity", y="Sensitivity")+geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color="darkgrey", linetype="dashed")+ annotate("text", x = 0.25, y = .25, label = paste("AUC =", round(best_ROC_gbm$auc, 2)))

#Accuracy
Accuracy_gbm<-as.vector(best_confusionmatrix_gbm$overall['Accuracy'])

#Precision
gbm_dt<-as.data.table(best_confusionmatrix_gbm$table)
precision_gbm<-gbm_dt[Predicted==1&Actual==1][,sum(N)]/gbm_dt[Predicted==1][,sum(N)]

#Recall
recall_gbm<-gbm_dt[Predicted==1&Actual==1][,sum(N)]/gbm_dt[Actual==1][,sum(N)]

#F1-score
F1_GBM<-2*precision_gbm*recall_gbm/(precision_gbm+recall_gbm)

######################################Logistic Regression############################################
set.seed(123)
logregFit1 <- train(hd_indicator~., data = training, 
                 method = "glm", 
                 trControl = fitControl,
                 family = 'binomial')

predictions_glm<-predict(logregFit1, newdata = testing)

predictionsprob_glm<-predict(logregFit1, newdata= testing, type = "prob")

confusionmatrix_glm<-confusionMatrix(testing$hd_indicator, predictions_glm,positive = NULL, dnn = c("Predicted", "Actual"))

ROC_glm<-roc(testing$hd_indicator,predictionsprob_glm$`1`)

PLot_ROC_glm<-ggroc(ROC_glm,alpha = 0.5, colour = "red", linetype = "solid", size = 2) +labs(title="ROC Curve GLM", x="Specificity", y="Sensitivity")+geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color="darkgrey", linetype="dashed")+ annotate("text", x = 0.25, y = .25, label = paste("AUC =", round(ROC_glm$auc, 2)))

#Variable Importance
logregVarImp<-varImp(logregFit1)

#Accuracy
Accuracy_glm<-as.vector(confusionmatrix_glm$overall['Accuracy'])

#Precision
glm_dt<-as.data.table(confusionmatrix_glm$table)
precision_glm<-glm_dt[Predicted==1&Actual==1][,sum(N)]/glm_dt[Predicted==1][,sum(N)]

#Recall
recall_glm<-glm_dt[Predicted==1&Actual==1][,sum(N)]/glm_dt[Actual==1][,sum(N)]

#F1-score
F1_GLM<-2*precision_glm*recall_glm/(precision_glm+recall_glm)

###################################Random Forest######################################
set.seed(123)

trees<-c(5,10,20,50,100,500)
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(training))))

rf_row_vec<-seq(1,length(trees),1)
rf_train_accuracy_scores<-c()
rf_predictions_list<-list()
rf_prediction_prob_list<-list()
for (row in rf_row_vec){
  prediction_fit<- train(hd_indicator~., data = training, 
                         method = "rf", 
                         trControl = fitControl,
                         tuneGrid = tunegrid,
                         ntree=trees[row],
                         verbose = FALSE)  
  training_accuracy_scores<-as.vector(prediction_fit$results['Accuracy'][1,])
  pred<-predict(prediction_fit,newdata=testing) 
  pred_prob<-predict(prediction_fit,newdata=testing,type = "prob")
  rf_train_accuracy_scores[row]<-training_accuracy_scores
  rf_predictions_list[[row]]<-pred
  rf_prediction_prob_list[[row]]<-pred_prob
}

rf_Test_Accuracy_Scores<-c()
i<-0
for (predictions in rf_predictions_list){
  i<-i+1
  confusion<-confusionMatrix(testing$hd_indicator, predictions, positive = NULL, dnn = c("Predicted", "Actual"))  
  Accuracy<-confusion$overall['Accuracy']  
  rf_Test_Accuracy_Scores[i]<-Accuracy
}

#Creating a training/test comparison table

rf_tunegrid <- expand.grid(.mtry = c(sqrt(ncol(training))),.ntree=trees)
setDT(rf_tunegrid,keep.rownames = TRUE)

rf_tunegrid$Training_Accuracy<-rf_train_accuracy_scores
rf_tunegrid$Test_Accuracy<-rf_Test_Accuracy_Scores

#Plotting avg.train/test accuracy by number of trees:

dd_rf<-melt(rf_tunegrid[,c(".ntree","Training_Accuracy","Test_Accuracy")],id=c(".ntree"))

plots_training_Test_rf<-ggplot(dd_rf) + geom_line(aes(x=.ntree, y=value, colour=variable)) +scale_colour_manual(values=c("red","blue"))+labs(title="Average Random Forest accuracy across n.trees", x="n.trees", y="Average Accuracy")

#Finding optimal Hyperparameters 
best_test_rows_Rf<-rf_tunegrid[Test_Accuracy==max(rf_tunegrid$Test_Accuracy)]
best_training_test_rows_Rf<-best_test_rows_Rf[Training_Accuracy==max(Training_Accuracy)]

#Evaluating this: 
best_predictions_rf<-rf_predictions_list[[as.numeric(best_training_test_rows_Rf$rn)]]
best_predictionsprob_rf<-rf_prediction_prob_list[[as.numeric(best_training_test_rows$rn)]]
best_confusionmatrix_rf<-confusionMatrix(testing$hd_indicator, best_predictions_rf, positive = NULL, dnn = c("Predicted", "Actual"))
best_ROC_rf<-roc(testing$hd_indicator,best_predictionsprob_rf$`1`)
PLot_ROC_rf<-ggroc(best_ROC_rf,alpha = 0.5, colour = "red", linetype = "solid", size = 2) +labs(title="ROC Curve Random Forest", x="Specificity", y="Sensitivity")+geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color="darkgrey", linetype="dashed")+ annotate("text", x = 0.25, y = .25, label = paste("AUC =", round(best_ROC_rf$auc, 2)))

#Accuracy
Accuracy_rf<-as.vector(best_confusionmatrix_rf$overall['Accuracy'])

#Precision
rf_dt<-as.data.table(best_confusionmatrix_rf$table)
precision_rf<-rf_dt[Predicted==1&Actual==1][,sum(N)]/rf_dt[Predicted==1][,sum(N)]

#Recall
recall_rf<-rf_dt[Predicted==1&Actual==1][,sum(N)]/rf_dt[Actual==1][,sum(N)]

#F1-score
F1_rf<-2*precision_rf*recall_rf/(precision_rf+recall_rf)

###################################XG boost ######################################
set.seed(123)
Rounds <- c(100) # Rounds to iterate
MaxDepth <- c(6) # Tree Max Depth
Eta <- c(0.01,0.05,0.1) # Learning Rate
Gamma <- c(0,1,5,10) # Regularizations (to prevent overfitting)

xgbGrid <- expand.grid(nrounds = Rounds, gamma = Gamma, max_depth = MaxDepth,
                       min_child_weight = 1, colsample_bytree = c(1, 0.05),
                       eta = Eta, subsample = 1)

xgb_row_vec<-seq(1,nrow(xgbGrid),1)
xgb_train_accuracy_scores<-c()
xgb_predictions_list<-list()
xgb_prediction_prob_list<-list()
xgb_models<-list()
for (row in xgb_row_vec){
  prediction_fit <- train(hd_indicator~., data = training,
                       method = "xgbTree",
                       trControl = fitControl,
                       verbose = FALSE,
                       tuneGrid = xgbGrid[row,],
                       allowParallel = TRUE)
  training_accuracy_scores<-as.vector(prediction_fit$results['Accuracy'][1,])
  pred<-predict(prediction_fit,newdata=testing) 
  pred_prob<-predict(prediction_fit,newdata=testing,type = "prob")
  xgb_models[[row]]<-prediction_fit
  xgb_train_accuracy_scores[row]<-training_accuracy_scores
  xgb_predictions_list[[row]]<-pred
  xgb_prediction_prob_list[[row]]<-pred_prob
}

#To prevent overfitting: find the parameters from the training set that generalise best on the test set. 
xgb_Test_Accuracy_Scores<-c()
i<-0
for (predictions in xgb_predictions_list){
  i<-i+1
  confusion<-confusionMatrix(testing$hd_indicator, predictions, positive = NULL, dnn = c("Predicted", "Actual"))  
  Accuracy<-confusion$overall['Accuracy']  
  xgb_Test_Accuracy_Scores[i]<-Accuracy}

#Creating a training/test comparison table

setDT(xgbGrid,keep.rownames = TRUE)

xgbGrid$Training_Accuracy<-xgb_train_accuracy_scores
xgbGrid$Test_Accuracy<-xgb_Test_Accuracy_Scores

#Plotting avg.train/test accuracy by eta:

xgb_training_Test_tree_dt<-xgbGrid[, lapply(.SD,mean), by=eta,
                               .SDcols = c("Training_Accuracy","Test_Accuracy")]

dd_xgb<-melt(xgb_training_Test_tree_dt,id=c("eta"))

plots_training_Test_xgb<-ggplot(dd_xgb) + geom_line(aes(x=eta, y=value, colour=variable)) +scale_colour_manual(values=c("red","blue"))+labs(title="Average XGBoost accuracy across eta", x="eta", y="Average Accuracy")

#Finding optimal Hyperparameters 
best_test_rows_xgb<-xgbGrid[Test_Accuracy==max(xgbGrid$Test_Accuracy)]
best_training_test_rows_xgb<-best_test_rows_xgb[Training_Accuracy==max(Training_Accuracy)]

#Evaluating this: 
best_predictions_xgb<-xgb_predictions_list[[as.numeric(best_training_test_rows_xgb$rn)]]
best_predictionsprob_xgb<-xgb_prediction_prob_list[[as.numeric(best_training_test_rows_xgb$rn)]]
best_confusionmatrix_xgb<-confusionMatrix(testing$hd_indicator, best_predictions_xgb, positive = NULL, dnn = c("Predicted", "Actual"))
best_ROC_xgb<-roc(testing$hd_indicator,best_predictionsprob_xgb$`1`)
PLot_ROC_xgb<-ggroc(best_ROC_xgb,alpha = 0.5, colour = "red", linetype = "solid", size = 2) +labs(title="ROC Curve XGBoost", x="Specificity", y="Sensitivity")+geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color="darkgrey", linetype="dashed")+ annotate("text", x = 0.25, y = .25, label = paste("AUC =", round(best_ROC_xgb$auc, 2)))
options(scipen=999)

#Accuracy
Accuracy_xgb<-as.vector(best_confusionmatrix_xgb$overall['Accuracy'])

#Precision
xgb_dt<-as.data.table(best_confusionmatrix_xgb$table)
precision_xgb<-xgb_dt[Predicted==1&Actual==1][,sum(N)]/xgb_dt[Predicted==1][,sum(N)]

#Recall
recall_xgb<-xgb_dt[Predicted==1&Actual==1][,sum(N)]/xgb_dt[Actual==1][,sum(N)]

#F1-score
F1_xgb<-2*precision_xgb*recall_xgb/(precision_xgb+recall_xgb)

XGBVarImp<-varImp(xgb_models[[as.numeric(best_training_test_rows_xgb$rn)]])

###################################Support Vector Machines######################################
set.seed(123)
training_svm<-training

training_svm$hd_indicator<-as.factor(ifelse(training_svm$hd_indicator==1,"X1","X0"))

testing_svm<-testing

testing_svm$hd_indicator<-as.factor(ifelse(testing_svm$hd_indicator==1,"X1","X0"))

svmFit1 <- train(hd_indicator~., data = training_svm, 
                          method = "svmRadial", 
                          trControl=trainControl(
                          method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          classProbs = TRUE),
                          )
predictions_svm<-predict(svmFit1, newdata = testing_svm)

predictionsprob_svm<-predict(svmFit1, newdata= testing_svm, type = "prob")

confusionmatrix_svm<-confusionMatrix(testing_svm$hd_indicator, predictions_svm, positive = NULL, dnn = c("Predicted", "Actual"))

ROC_svm<-roc(testing_svm$hd_indicator,predictionsprob_svm$X1)

PLot_ROC_svm<-ggroc(ROC_svm,alpha = 0.5, colour = "red", linetype = "solid", size = 2) +labs(title="ROC Curve SVM", x="Specificity", y="Sensitivity")+geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color="darkgrey", linetype="dashed")+ annotate("text", x = 0.25, y = .25, label = paste("AUC =", round(ROC_svm$auc, 2)))

#Variable Importance
SVMVarImp<-varImp(svmFit1)

#Accuracy
Accuracy_svm<-as.vector(confusionmatrix_svm$overall['Accuracy'])

#Precision
SVM_dt<-as.data.table(confusionmatrix_svm$table)
precision_SVM<-SVM_dt[Predicted=="X1"&Actual=="X1"][,sum(N)]/SVM_dt[Predicted=="X1"][,sum(N)]

#Recall
recall_SVM<-SVM_dt[Predicted=="X1"&Actual=="X1"][,sum(N)]/SVM_dt[Actual=="X1"][,sum(N)]

#F1-score
F1_SVM<-2*precision_SVM*recall_SVM/(precision_SVM+recall_SVM)

######################### Model Evaluation Comparison #########################

Methods<-c("GBM","GLM","RF","XGB","SVM")

Accuracy_Scores<-c(Accuracy_gbm,Accuracy_glm,Accuracy_rf,Accuracy_xgb,Accuracy_svm)

Precision_Scores<-c(precision_gbm,precision_glm,precision_rf,precision_xgb,precision_SVM)

Recall_Scores<-c(recall_gbm,recall_glm,recall_rf,recall_xgb,recall_SVM)

F1_Scores<-c(F1_GBM,F1_GLM,F1_rf,F1_xgb,F1_SVM)

AUC_Scores<-c(best_ROC_gbm$auc,ROC_glm$auc,best_ROC_rf$auc,best_ROC_xgb$auc,ROC_svm$auc)

Model_Comparison<-data.table(
Methods=Methods,
Accuracy_Scores=Accuracy_Scores,
Precision_Scores=Precision_Scores,
Recall_Scores=Recall_Scores,
F1_Scores= F1_Scores,
AUC_Scores= AUC_Scores
)

#Evaluation Plots
Accuracy_comparison<-ggplot(Model_Comparison,aes(x=reorder(Methods, -Accuracy_Scores),Accuracy_Scores)) +geom_bar(stat = "identity",color="dark green",fill="dark green",width = 0.25)+geom_text(aes(label = round(Accuracy_Scores,2)), color = "dark green", vjust = -0.5, show.legend = FALSE,size=3.5)+labs(title="Accuracy Across Different Algorithms", x="Method", y="Accuracy")+ scale_y_continuous(limits=c(0.65,1),oob=rescale_none)
#GBM/NN wins
Precision_comparison<-ggplot(Model_Comparison,aes(x=reorder(Methods, -Precision_Scores),Precision_Scores)) +geom_bar(stat = "identity",color="dark green",fill="dark green",width=0.25)+geom_text(aes(label = round(Precision_Scores,2)), color = "dark green", vjust = -0.5, show.legend = FALSE,size=3.5)+labs(title="Precision Across Different Algorithms", x="Method", y="Precision")+ scale_y_continuous(limits=c(0.65,1),oob=rescale_none)
#GBM/GLM/NN/SVM wins
Recall_comparison<-ggplot(Model_Comparison,aes(x=reorder(Methods, -Recall_Scores),Recall_Scores)) +geom_bar(stat = "identity",color="dark green",fill="dark green",width=0.25)+geom_text(aes(label = round(Recall_Scores,2)), color = "dark green", vjust = -0.5, show.legend = FALSE,size=3.5)+labs(title="Recall Across Different Algorithms", x="Method", y="Recall")+ scale_y_continuous(limits=c(0.65,1),oob=rescale_none)
#GBM/NN/RF wins
F1_Score_comparison<-ggplot(Model_Comparison,aes(x=reorder(Methods, -F1_Scores),F1_Scores)) +geom_bar(stat = "identity",color="dark green",fill="dark green",width=0.25)+geom_text(aes(label = round(F1_Scores,2)), color = "dark green", vjust = -0.5, show.legend = FALSE,size=3.5)+labs(title="F1 Score Across Different Algorithms", x="Method", y="F1 Score")+ scale_y_continuous(limits=c(0.65,1),oob=rescale_none)
#GBM/NN Wins
AUC_comparison<-ggplot(Model_Comparison,aes(x=reorder(Methods, -AUC_Scores),AUC_Scores)) +geom_bar(stat = "identity",color="dark green",fill="dark green",width=0.25)+geom_text(aes(label = round(AUC_Scores,3)), color = "dark green", vjust = -0.5, show.legend = FALSE,size=3.5)+labs(title="AUC Score Across Different Algorithms", x="Method", y="AUC")+ scale_y_continuous(limits=c(0.65,1),oob=rescale_none)
#Random forest Wins, Followed by Logistic,XG,NN,SVM,GBM

grid.arrange(Accuracy_comparison, Precision_comparison,Recall_comparison,F1_Score_comparison,AUC_comparison,nrow = 3,ncol=2)

#ROC Plots: 
grid.arrange(PLot_ROC_glm,PLot_ROC_xgb,PLot_ROC_rf,PLot_ROC_gbm,PLot_ROC_svm,nrow=3,ncol=2)

#Training test plots: 
grid.arrange(plots_training_Test_gbm,plots_training_Test_rf,plots_training_Test_xgb,nrow=2,ncol=2)


