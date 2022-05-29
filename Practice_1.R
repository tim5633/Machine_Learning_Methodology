library(caret) # for GermanCredit data
library(rattle) #for fancyRpartPlot
library(pROC) # for ROC caculating 
library(ggplot2) #for plotting the variables of impaotancce
################################################################################
##### use GermanCredit to split the train with 70% and test with 30% ###########
################################################################################

#When viewing the dataset, we coud found that variables of Purpose.
#Vacation and Personal.Female.Single are with all the same value of 0. 
#We drop those two columns since it would not helping to identify the classes in the model.
#We further use createDataPartition function to spilt the train 70% and test 30%  
#from the GermanCredit dataset of caret package.  

# import data
data("GermanCredit")
# Delete two variables where all values are the same for both classes 
unique(GermanCredit$Purpose.Vacation) # [1] 0
unique(GermanCredit$Personal.Female.Single)# [1] 0
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] = list(NULL)
# Split train and test dataset
set.seed(48) 
index <- createDataPartition(GermanCredit$Class, p=0.7, list = FALSE)
train <- GermanCredit[index, ]
test <- GermanCredit[-index, ]

################################################################################
##### the optimal pruned decision tree #########################################
################################################################################

#For the decision tree, we are using caret package to output the optimal tree. 
#Before creating the tree model, we set the trainControl, with the (1.)repeatedcv method, 
#(2.)number as 5-fold cross-validation and 
#(3.)one repeat since the claiming form tasks: 
#we don’t need to repeat cross-validation several times to tune the parameters. 
#One round of cross-validation is enough.

# setting the control option before create model
trControl=trainControl( method = "repeatedcv", 
                        number = 5, # 5-fold cross-validation
                        repeats = 1) # claiming form tasks document: one round is enough
# create model with fix random seed
set.seed(48) 
dtFit=train(Class~.,
            data=train,
            method = "rpart", 
            tuneLength=10, 
            trControl = trControl)

# check the optimal model setting
dtFit  # the final value used for the model was cp = 0.01666667 and with train data of accuracy0.7300000

# predict with test data to final model and output the test error rate
pred_dt = predict(dtFit$finalModel,
                  newdata=test[,-which(colnames(test)=="Class")],
                  type="class")
print(1- mean(pred_dt==test[,which(colnames(test)=="Class")])) # error rate = 0.2866667

# plotting a tree
fancyRpartPlot(dtFit$finalModel, 
               main= "Optimal pruned tree of train model",
               caption = "") 

################################################################################
##### the optimal random forest ################################################
################################################################################

#For the random forest, we are also using caret package to output the optimal model.
#Inside the train random forest model, we set with number of cross-validation to5-fold 
#and ntree=1000 to output of tuneLength=10 to choose the best mtry based on the accuracy.

# setting the control option before create model
trControl=trainControl( method = "repeatedcv",
                        number = 5, # 5-fold cross-validation
                        repeats = 1 # claiming form tasks document: one round is enough
                        )
# create model with fix random seed
set.seed(48)
rfFit=train(Class~.,
            data=train,
            method="rf",
            tuneLength=10,
            metric="Accuracy",
            trControl=trControl,
            ntree = 1000, # Set the number of trees to 1000
            #localImp = TRUE
            )

# check the optimal model setting
rfFit # The final value used for the model was mtry = 14 with accuracy of 0.7500000

# predict with test data to final model
pred_rf = predict(rfFit$finalModel,
                  newdata=test[,-which(colnames(test)=="Class")],
                  type="class")
print(1-mean(pred_rf==test[,which(colnames(test)=="Class")])) # error rate = 0.2233333


#Create a plot showing variable importance
varaible_importance = varImp(rfFit, scale = FALSE)
ggplot(varaible_importance)+ ggtitle("Plot of Train RF Model")

################################################################################
##### the ROC curve ############################################################
################################################################################
# prob in random forest 
pred_rf_prob= predict(rfFit,
                 test[,-10],
                 type = 'prob'
                 )
# prob in decision tree (#positive class =1 is Bad)
pred_dt_prob= predict(dtFit,
                      test[,-10],
                      type = 'prob'
                      )

# plotting ROC curve (both decision tree and random forest)
par(pty="s")
rf_ROC = roc(predictor=pred_rf_prob$Bad, 
             response=GermanCredit$Class[-index],
             plot = TRUE,
             print.auc=FALSE,
             col="forestgreen",
             legacy.axes=TRUE,
             main="ROC Curves"
             )
dt_ROC = roc(predictor=pred_dt_prob$Bad, 
             response=GermanCredit$Class[-index],
             plot=TRUE,
             print.auc=FALSE,
             col="dodgerblue4",
             legacy.axes=TRUE,
             add = TRUE
             ) 
legend("bottomright",
       legend=c("Random Forest","Decision Tree"),
       col=c("forestgreen","dodgerblue4"),
       lwd=2,border = T
       )
text(0.8,
     0.75,
     col = 'forestgreen',
     paste("rf_ROC$auc ", round(rf_ROC$auc, 3))
     )
text(0.65,
     0.45,
     col = 'dodgerblue4',
     paste("dt_ROC$auc ", round(dt_ROC$auc, 3))
     )

# print out the AUC number of both decision tree and random forest  
dt_ROC$auc # Area under the curve: 0.682
rf_ROC$auc # Area under the curve: 0.7556
