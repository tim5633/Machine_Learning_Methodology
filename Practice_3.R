library(caret)
library(ROSE) #for ROC plot
library(rgr)
library(dplyr) # for merging two ROC record
library(pROC)
################################################################################
##### Download the newthyroid.txt data #########################################
################################################################################
# randomly split the data to a training set (70%) and a test set (30%) 
# and repeat the random split 10 times.
newthyroid <- read.delim("newthyroid.txt", sep =",")

#find average value in each columns of different classes
# this is to detect whether the data is well seperated for betweeness
newthyroid_h <- newthyroid[newthyroid$class == "h",]
newthyroid_n <- newthyroid[newthyroid$class == "n",]
mean(newthyroid_h$feature1) #[1] 95.28571
mean(newthyroid_n$feature1) #[1] 110.5133
mean(newthyroid_h$feature2) #[1] 17.74571
mean(newthyroid_n$feature2) #[1] 9.192667
mean(newthyroid_h$feature3) #[1] 4.262857
mean(newthyroid_n$feature3) #[1] 1.731333
mean(newthyroid_h$feature4) #[1] 0.9742857
mean(newthyroid_n$feature4) #[1] 1.316667
mean(newthyroid_h$feature5) #[1] -0.02
mean(newthyroid_n$feature5) #[1] 2.516667


seed_rep <- list(331,332,333,334,335,336,337,338,339,330) 
# discarded the original argument of time= 20 
train_index <- createDataPartition(newthyroid$class,p=0.7,list=FALSE)  
# for loop to record both train and test with 10 times randomly spilts
train <- list()
test <- list()
for(i in  seq(seed_rep)){
  set.seed(seed_rep[[i]])
  train_index <- createDataPartition(newthyroid$class,p=0.7,list=FALSE) 
  train[[i]] <-newthyroid[train_index,]
  test[[i]] <- newthyroid[-train_index,]
  }

################################################################################
##### Use the lda function in caret ############################################
################################################################################
ROC_iteration_LDA <- list()
AUC_record_LDA <- list()
for(i in  seq(seed_rep)){
  # Setup for cross validation
  fitControl=trainControl(
    method = "repeatedcv",
    number = 5, # 5-fold cross-validation
    repeats = 1
    )
  # fit the lda model
  set.seed(866) 
  ldaFit=train(class ~.,
               data = train[[i]],
               method="lda",
               trControl=fitControl
               )
  #make the predict and record the AUC
  pred <- predict(ldaFit,test[[i]][,-1])
  ROC_iteration_LDA[[i]] <- roc(test[[i]]$class, as.numeric(pred), plotit = FALSE)
  AUC_record_LDA[[i]] <- ROC_iteration_LDA[[i]][[9]] #inside the roc function list, index 9 is the AUC
}

################################################################################
##### Use the kNN function in caret ############################################
################################################################################
#For kNN, use 5-fold cross-validation to choose k from (3, 5, 7, 9, 11, 13, 15). 
#Use AUC as the metric to choose k, i.e. choose k with the largest AUC.

ROC_iteration_kNN <- list()
AUC_record_kNN <- list()
for(i in  seq(seed_rep)){
  
  # Tune the parameter by repeated CV
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 1,
    classProbs=TRUE,
    summaryFunction = twoClassSummary
  )
  
  #tune
  kNNGrid = expand.grid(k= c(3, 5, 7, 9, 11, 13, 15))
  
  # fit the lda model
  set.seed(48) 
  kNNFit=train(class ~.,
               data = train[[i]],
               method="knn",
               preProcess = c("center","scale"),
               tuneLength = 10,
               trControl=fitControl,
               tuneGrid=kNNGrid,
               metric = "ROC"
               )
  #make the predict and record the AUC
  pred <- predict(kNNFit,test[[i]][,-1])
  ROC_iteration_kNN[[i]] <- roc(test[[i]]$class, as.numeric(pred), plotit = FALSE)
  AUC_record_kNN[[i]] <- ROC_iteration_kNN[[i]][[9]] #inside the roc function list, index 9 is the AUC
}


################################################################################
##### boxplot for LDA and kNN###################################################
################################################################################
AUC_record_LDA <- do.call(rbind, AUC_record_LDA)
AUC_record_kNN <- do.call(rbind, AUC_record_kNN)

AUC_record <- data.frame(AUC_record_LDA, AUC_record_kNN)

boxplot(AUC_record, main="10-times splitting AUC values")




