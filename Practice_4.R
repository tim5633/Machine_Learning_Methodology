#Step 1: specify out the classes value.
#Step 2: add another column of index for the purpose of recording when  randomly selecting train and valiadation set, and it is more easy to call out from the output.
#Step 3: creating empty list to record each steps, which would be easy to track and fix when getting error.
#Step 4: The first iteration would be the main body to iterate steps. To be more specific: 
  #Valiadation from fold[[0]], the fold[[1 to 9]] would be training index;
  #Valiadation from fold[[1]], the fold[[2 to 9]] and fold[[0]] would be training index;
  #Valiadation from fold[[2]], the fold[[3 to 9]] and fold[[0 to 1]] would be training index, so on and so forth.
#Step 5: The second iteration: would be for the usage of separating into different classes dataframes, this is to make every train and valiadation dataset to be evenly distributed with classes from Step 1.
#Step 6: another for loop inside the second iteration: with the input fixed random seed, the iteration spilt the dataset to K fold and record the result.
#Step 7: bind the classes into a single dataframe per fold. 
#Step 8: repeat the steps with K times and output the index.


################################################################################
##### import the library and GermanCredit data #################################
################################################################################
library(dplyr)
library(caret)
data("GermanCredit")

################################################################################
##### user-defined function to provide each fold training indexes ##############
################################################################################
# containing input with data label factor, K folds, and random seed number
Kfold_training_indexes <- function(label_vector, K, random_seed ){  
  
########## Step 1: specify out the classes value.
  class <- unique(label_vector) #####---> here is our function input for label_vector
  
########## Step 2: add another column of index for the purpose of recording when randomly selecting 
  # the reason why we create the dataframe here is that
  # (1) not all the data outside would contain with the same column name and might get error if we directly using
  # (2) to have better structure for engineering, clear and direct.
  df_vector <- as.data.frame(list(label_vector)) #####---> here is our function input for label_vector 
  names(df_vector)[1] <- "Class" #rename the column to be uniformed when using
  # we also create a new columns to add the index form original label_vector, for usage of printing out in the end 
  index <- data.frame(c(seq(from=1, to=length(df_vector$Class), by=1)))
  df_vector['index'] <- index
  
########## Step 3: creating empty list to record each steps, which would be easy to track and fix when getting error.
  #for the first iteration (the big loop): the iteration to generate each steps records
  output <-list()
  # for the second iteration 
  df_list <- list() # outside layer
  class_K_rolls_list<- list() # bridge of both inside and outside layer
  trn_list_class <- list() # inside layer and output
  # for recording the the first iteration
  trn_list <- list()
  # for the third iteration 
  final_train <- list() # rows: classes, columns: train and val dataset (dataframes inside dataframe)
  final_train_merge_class <- list() # binding classes together for "one dataframe" after randomly, and evenly selecting train and vla data out
  final_train_index <- list() # collecting all the folds dataframe together and become the function output
  
########### Step 4: The first iteration would be the main body to iterate steps. c
  for(j in  seq(K)){  
########### Step 5: The second iteration: would be for the usage of separating into different classes dataframes
    for(i in  seq(class)){
      df_list[[i]] <- df_vector[df_vector$Class == class[i],] # to make each train evenly distributed, we need to take each class dataset out
      class_K_rolls_list[[i]] <- nrow(df_list[[i]])/K # calculate the overall dataset rolls to decided how many rolls in each train fold 
########### Step 6: another for loop inside the second iteration: with the input fixed random seed, the iteration spilt the dataset to K fold and record the result.
      for(k in seq(K)){
        set.seed(random_seed)  #####---> here is our function input for random_seed 
        trn_list_class[[k]] <- sample_n(df_list[[i]], class_K_rolls_list[[i]])
        df_list[[i]] <- df_list[[i]][!df_list[[i]]$index%in%trn_list_class[[k]]$index,]
      } 
      trn_list[[i]] <- trn_list_class
    }
########### Step 7: The third iteration
    #Binding and store the dataset from first iteration to become rows: classes, columns: train and val dataset
    final_train = do.call(rbind, trn_list)

    for(k in seq(K)){
      final_train_merge_class = do.call(rbind, final_train[,k])
      final_train_index[[k]] = final_train_merge_class
      final_train_index[[k]] <-final_train_index[[k]][order(final_train_index[[k]]$index),]
    }
    
    #if we want to check whether the validation set is (could un-comment and change the return to output_val)
    #output_val<-list()
    #output_val[[j]] = final_train_index[[j]]
    
########### Step 8: repeat the steps with K times and output the index.
    final_train_index[[j]] <- NULL
    output_trn = do.call(rbind, final_train_index)
    output[[j]] = output_trn
    
  }    

########### Print out the train index for each step
    for(k in seq(K)){
      print(paste0("the train index of K =",k))
      print(output[[k]]$index)
    }

########### Return the data to store in golbal environment
  return(output)
}

################################################################################
##### set input and using user-defined function ################################
################################################################################
output_final_train_index <- Kfold_training_indexes(
  label_vector <- GermanCredit$Class,
  K <- 10,
  random_seed <- 12345
)


# Print out the train index for each fold
    for(k in seq(K-1)){
      print(paste0("the train index of K = ",k))
      print(output_final_train_index[[k]]$index)
    }
################################################################################
##### check the train classes distribution is the same as original one #########
################################################################################

#Finally we could apply the user-define function with the input of 
#label_vector, K, and random seed. If we would like to check the index per step,
#we could use the output_final_train_index[[here is the K fold you can enter]]$index. 
#Or simply check the define function, it has already print them out for you. 
#And we also check with the class distribution, 
#which would all return with true comparing with the original dataset.


original_distribution = nrow(GermanCredit[GermanCredit$Class == "Good",])/
  nrow(GermanCredit[GermanCredit$Class == "Bad",])
for(k in seq(K)){
  train_distribution = nrow(output_final_train_index[[k]][output_final_train_index[[k]]$Class == "Good",])/
  nrow(output_final_train_index[[k]][output_final_train_index[[k]]$Class == "Bad",])
  print(original_distribution == train_distribution)
}





