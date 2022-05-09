install.packages('readr')
install.packages('caret')
library(class)
library(caret)
library(readr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")

#please put the 'cancer_testset.csv' and 'cancer_trainset.csv' in your working directory at first
#This two datasets have been processed by Ben
test <- read.csv('cancer_testset.csv',header=TRUE)
test <- test[,-1]; test <- test[,-1]; 
str(test)
#names(test) <- c("Clump_Thickness", "Uniformity_CellSize","Uniformity_Cellshape", 
#                 "Marginal_Adhesion","Single_Epithelial_Cell_Size", "Bare_Nuclei", 
#                "Bland_Chromatin","Normal_Nucleoli","Mitoses",'class')

train <- read.csv('cancer_trainset.csv',header=TRUE)
train <- train[,-1]; train <- train[,-1]
str(train)

as.factor(train$Bare_Nuclei);as.factor(test$Bare_Nuclei);
train_df <- as.data.frame(train)
test_df <- as.data.frame(test)
#Convert outcome variable to type factor
train_df$Class <- as.factor(train_df$Class); test_df$Class <- as.factor(test_df$Class)
class(train_df$Class); class(test_df$Class)

ctrlspecs <- trainControl(method='cv', number=10,
                         savePredictions='all',
                         classProbs=TRUE)
#set random seed
set.seed(388)

#Specify Logistic Regression model
model_LR <- train(Class ~ Clump_Thickness + Uniformity_CellSize + Uniformity_Cellshape
               + Marginal_Adhesion + Single_Epithelial_Cell_Size + Bare_Nuclei                
               + Bland_Chromatin + Normal_Nucleoli + Mitoses, 
               data=train_df, method='glm', family=binomial, trControl=ctrlspecs)

print(model_LR)

#Kappa 'Rules of thumb' for interpretation
# .81-1.00 Almost perfect
# .61-.80 Substantial
# .41-60 Moderate
# .21-40 Fair
# .00-20 Slight
# >.00 Poor

# output in terms of Logistic Regression Coefficients
summary(model_LR)

#Variable Importance (predictor variables)
varImp(model_LR)

#Can write out the regression equation here if necessary,Benign=2,Malignant=4
#Class=-9.5988+0.4356F1+0.1226F2+0.3150F3+0.2535F4+0.1241F5+0.3787F6+0.3985F7+0.1688F8+0.4969F9
#

#Apply model to test_df


#Predict outcome using model from train_df applid to test_df
predictions <- predict(model_LR,newdata = test_df)

#Creat Confusion Matrix
confusionMatrix(data=predictions, test_df$Class)
#Balanced Accuracy is Sensitivity+Specificity/2
#Here 'Positive' Class is benign but detecting malignant is more importang. 95.24% who got a bad cancer were correctly predicted
#Prevalence is actual positives, Detection Rate is predicted positives


#train the model and see how much the curve correctly fits the trainset
predicted.data <- data.frame(probability.of.cl=glm.fit$fitted.values,cl=train_df$Class)
predicted.data <- predicted.data[order(predicted.data$probability.of.cl,decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.cl)) +
  geom_point(aes(color=cl), alpha=1, shape=4, stroke=1) +
  xlab('probability rank from low to high') +
  ylab('Predicted probability if getting a bad cancer')
#save the LR training curve
ggsave('Maglignant_cancer_probabilities.pdf')

set.seed(388)
#training once again
glm.fit=glm(Class ~ Clump_Thickness + Uniformity_CellSize + Uniformity_Cellshape
            + Marginal_Adhesion + Single_Epithelial_Cell_Size + Bare_Nuclei                
            + Bland_Chromatin + Normal_Nucleoli + Mitoses,data=train_df,family=binomial)
summary(glm.fit)
#Drawing roc curve of LR model
par(pty='s')
#repete to adjust the graph make it looks good
roc(train_df$Class,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
    xlab='False Positive',ylab='True Positive',
    col='#377eb8',lwd=4,print.auc=TRUE)

roc.info <- roc(train_df$Class,glm.fit$fitted.values,legacy.axes=TRUE)

roc.df <- data.frame(
 tpp=roc.info$sensitivities*100,
 fpp=(1-roc.info$specificities)*100,
 thresholds=roc.info$thresholds)

head(roc.df)
tail(roc.df)
#choosing thresholds needed from a range
roc.df[roc.df$tpp>80 & roc.df$fpp<10,]




#This was my preprocessing without using my teammate's dataset, delete '##'

#please put the 'breast-cancer-wisconsin.data' in your working directory at first
##cancer <- read.csv('breast-cancer-wisconsin.data',header=FALSE)

##names(cancer) <- c("Sample_Code_Number", "Clump_Thickness", "Uniformity_CellSize","Uniformity_Cellshape", "Marginal_Adhesion",
##                   "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin","Normal_Nucleoli","Mitoses", "Class")

#Bare_Nuclei data missing is denoted as '?', sort these rows out and put them in a list.
##missing_rows=c(as.integer(rownames(cancer[cancer$Bare_Nuclei=='?',])))

#delete them from original dataset
##cancer_processed=cancer[-missing_rows,]

#We don't need to analyze the Sample_Code_Number, delete first column
##df=cancer_processed[,-1]
##str(df)
##as.factor(df$Class); as.factor(df$Bare_Nuclei)
##df <- as.data.frame(df)

#Set Randoms seeds so the result will be the same with the same algorithm
##set.seed(666)

##index <- createDataPartition(df$Class,p=.8,list=FALSE,times=1)

#Creat train set and test set
##train_df <- df[index,]; test_df <- df[-index,]

#Re-label values of outcome
##train_df$Class[train_df$Class==2] <- 'benign'; train_df$Class[train_df$Class==4] <- 'malignant'

##test_df$Class[test_df$Class==2] <- 'benign'; test_df$Class[test_df$Class==4] <- 'malignant'

#Convert outcome variable to type factor
##train_df$Class <- as.factor(train_df$Class); test_df$Class <- as.factor(test_df$Class)
##class(train_df$Class); class(test_df$Class)

##ctrlspecs <- trainControl(method='cv', number=10,
##                          savePredictions='all',
##                          classProbs=TRUE)


#Useless these code

#lines(train_df$Class, glm.fit$fitted.values)
