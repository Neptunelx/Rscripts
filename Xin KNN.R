install.packages('readr')
install.packages('caret')
library(class)
library(caret)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(purrr)
library(kknn)
library(e1071)

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

#Set random seeds
set.seed(555)
knn_pred <- knn(train = train_df[,1:9], test = test_df[,1:9], cl = train_df$Class, k=12)
mean(knn_pred == test_df$Class)
table(test$Class, knn_pred)
summary(knn_pred)

#Creat Confusion Matrix
confusionMatrix(data=knn_pred, test_df$Class)
#mean(knn_pred == test_df$Class) equals to accuracy and error is 1-accuracy
error <- c()

for (i in 1:15)
{
  knn.fit <- knn(train = train_df[,1:9], test = test_df[,1:9], cl = train_df$Class, k = i)
  error[i] = 1- mean(knn.fit == test_df$Class)
}
### this graph then show the error for each of the different values of k
### so lower is better for this graph
### however every time the model above is ran it will show a different graph
### due to the randomness of generating models

ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
geom_line(color = "Blue")

#Now try K-NN with Tidymodels
## This is just a different package to train the knn model with
### a different way to do the same thing as above

knn_spec <- nearest_neighbor(neighbors = 12) %>%
  set_mode("classification") %>%
  set_engine("kknn")

knn_fit <- knn_spec %>%
  fit(Class ~., data = train_df)

knn_fit 

knn_fit %>% 
  predict(test_df) %>% 
  bind_cols(test_df) %>% 
  metrics(truth = Class, estimate = .pred_class)

augment(knn_fit, new_data = test_df) %>%
  conf_mat(truth = Class, estimate = .pred_class) 

augment(knn_fit, new_data = test_df) %>%
  accuracy(truth = Class, estimate = .pred_class) 



#ROC Curve
knn_fit %>%
  #probability for each possible predicted value
  predict(test_df, type = "prob") %>% 
  bind_cols(test_df) %>%
  #graph
  roc_curve(Class, .pred_Benign:.pred_Malignant) %>%
  autoplot()












### This was my preprocessing without using my teammate's dataset, please delete '##'       ###


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


