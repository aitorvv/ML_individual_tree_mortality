#!/usr/bin/Rscript

#------------------------------------------------------------------------------------------#
####                          Support Vector Machine analysis                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 12/05/2023                              #
#                              Last modification: 17/10/2023                               #
#------------------------------------------------------------------------------------------#


SVM_analysis_parallel <- function(df_study, my_combis, my_timer){
  
  # activate timer
  tic("SVM")
  
  # model number
  n_model = 0
  
  # lists: models and accuracy
  svm_model <- list()
  svm_accuracy <- list()
  svm_accuracy_dead <- list()
  svm_accuracy_alive <- list()
  svm_roc <- list()
  svm_auc <- list()
  svm_aucpr <- list()
  svm_mcc <- list()
  svm_kappa <- list()
  
  # parallelization
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  
  for (k in my_combis){
    
    # model name
    n_model = n_model + 1
    name <- paste('svm_model_', n_model, sep = '')
    
    # get my variables combination
    df <- df_study[, which((names(df_study) %in% k) == TRUE)]
    
    # normalize data
    dead_col <- df$dead
    df <- normalize(df[2:length(df)])
    df$dead <- dead_col
    
    # split data
    train_ind <- sample(seq_len(nrow(df)), size = smp_size)
    train <- df[train_ind, ]
    test <- df[-train_ind, ]
    labels <- ifelse(test$dead == 'yes', 1, 0)
    test <- dplyr::select(test, -dead)
    
    # fit control including parallelization
    fitControl <- trainControl(method = "cv", # cross-validation
                               number = 10, # 10 k-folds
                               allowParallel = TRUE) # parallelization
    
    # fit the model
    tic(name)
    svm_model[[name]] <- train(as.factor(dead) ~ ., 
                               data = train, 
                               method = 'svmLinearWeights2', # method = 'svmRadialWeights')
                               trControl = fitControl)
    my_timer <- rbind(my_timer, toc())
    # svm_model[[name]] <- svm(formula = as.factor(dead) ~ ., data = train,type = 'C-classification', kernel = 'radial') # package != caret
    print(paste('Model ', name, ' run succesfully.', sep = ''))
    
    # make a prediction
    pred = predict(svm_model[[name]], newdata = test, type = 'raw')  
    
    # binary classify predictions
    pred <- ifelse(as.numeric(pred) == 2, 1, 0)
    
    # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
    pred_vs_real <- ROCR::prediction(pred, labels)
    performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
    svm_roc[[name]] <- performance
    
    # AUC is the Area Under the Curve - TPR vs FPR 
    auc <- ROCR::performance(pred_vs_real, measure = "auc")
    auc <- auc@y.values[[1]]
    svm_auc[[name]] <- auc
    
    # AUCPR - TPR vs PPV - Precision/Recall
    aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
    aucpr <- aucpr@y.values[[1]]
    svm_aucpr[[name]] <- aucpr
    
    # MCC - Matthews correlation coefficient or phi coefficient
    mcc <- ROCR::performance(pred_vs_real, measure = "phi")
    mcc <- mcc@y.values[[1]]
    svm_mcc[[name]] <- mcc
    
    # Cohen's kappa coefficient 
    kappa <- kappa2(cbind(pred, labels))
    svm_kappa[[name]] <- kappa
    
    # check accuracy
    pred_errors <- mean(pred != labels)
    svm_accuracy[[name]] <- 1 - pred_errors
    
    # calculate accuracy splitting on dead and alive trees
    len_alive <- 0
    right_alive <- 0
    len_dead <- 0
    right_dead <- 0
    for (k in 1:length(labels)){
      # depending if the tree originally was alive (0) or dead (1)
      if(labels[k] == 0){
        # calculate prediction on alive trees: 1 ok, 0 error
        pred_alive <- ifelse(pred[k] == 0, 1, 0)
        right_alive <- right_alive + as.numeric(pred_alive)
        len_alive <- len_alive + 1
      } else {
        # calculate prediction on dead trees: 1 ok, 0 error
        pred_dead <- ifelse(pred[k] == 1, 1, 0)
        right_dead <- right_dead + as.numeric(pred_dead)
        len_dead <- len_dead + 1
      }
    }
    # add information to the main list 
    svm_accuracy_alive[[name]] <- right_alive / len_alive
    svm_accuracy_dead[[name]] <- right_dead / len_dead
  }
  
  # stop parallelization
  stopCluster(cluster)
  registerDoSEQ()
  
  # exclude temporal variables
  # rm(performance, pred_vs_real, test, train, auc, k, labels, len_alive, len_dead,
  #    n_model, name, pred, pred_alive, pred_dead, pred_errors, dead_col, right_alive,
  #    right_dead, train_ind, kappa, aucpr, mcc, cluster, fitControl)
  
  # save time data
  my_timer <- rbind(my_timer, toc())
  
  # return a list with the results
  results <- list(svm_model, svm_accuracy, svm_accuracy_dead, svm_accuracy_alive, svm_roc, svm_auc, svm_aucpr, 
                  svm_mcc, svm_kappa, my_timer) # svm_threshold not available
  return(results)
}


SVM_analysis <- function(df_study, my_combis, my_timer){
  
  # activate timer
  tic("SVM")
  
  # model number
  n_model = 0
  
  # lists: models and accuracy
  svm_model <- list()
  svm_accuracy <- list()
  svm_accuracy_dead <- list()
  svm_accuracy_alive <- list()
  svm_roc <- list()
  svm_auc <- list()
  svm_aucpr <- list()
  svm_mcc <- list()
  svm_kappa <- list()
  
  # parallelization
  #cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  #registerDoParallel(cluster)
  
  for (k in my_combis){
    
    # model name
    n_model = n_model + 1
    name <- paste('svm_model_', n_model, sep = '')
    
    # get my variables combination
    df <- df_study[, which((names(df_study) %in% k) == TRUE)]
    
    # normalize data
    dead_col <- df$dead
    df <- normalize(df[2:length(df)])
    df$dead <- dead_col
    
    # split data
    train_ind <- sample(seq_len(nrow(df)), size = smp_size)
    train <- df[train_ind, ]
    test <- df[-train_ind, ]
    labels <- ifelse(test$dead == 'yes', 1, 0)
    test <- dplyr::select(test, -dead)
    
    # fit control including parallelization
    fitControl <- trainControl(method = "cv", # cross-validation
                               number = 10, # 10 k-folds
                               allowParallel = TRUE) # parallelization
    
    # fit the model
    tic(name)
    svm_model[[name]] <- train(as.factor(dead) ~ ., 
                               data = train, 
                               method = 'svmLinearWeights2', # method = 'svmRadialWeights')
                               trControl = fitControl)
    my_timer <- rbind(my_timer, toc())
    # svm_model[[name]] <- svm(formula = as.factor(dead) ~ ., data = train,type = 'C-classification', kernel = 'radial') # package != caret
    print(paste('Model ', name, ' run succesfully.', sep = ''))
    
    # make a prediction
    pred = predict(svm_model[[name]], newdata = test, type = 'raw')  
    
    # binary classify predictions
    pred <- ifelse(as.numeric(pred) == 2, 1, 0)
    
    # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
    pred_vs_real <- ROCR::prediction(pred, labels)
    performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
    svm_roc[[name]] <- performance
    
    # AUC is the Area Under the Curve - TPR vs FPR 
    auc <- ROCR::performance(pred_vs_real, measure = "auc")
    auc <- auc@y.values[[1]]
    svm_auc[[name]] <- auc
    
    # AUCPR - TPR vs PPV - Precision/Recall
    aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
    aucpr <- aucpr@y.values[[1]]
    svm_aucpr[[name]] <- aucpr
    
    # MCC - Matthews correlation coefficient or phi coefficient
    mcc <- ROCR::performance(pred_vs_real, measure = "phi")
    mcc <- mcc@y.values[[1]]
    svm_mcc[[name]] <- mcc
    
    # Cohen's kappa coefficient 
    kappa <- kappa2(cbind(pred, labels))
    svm_kappa[[name]] <- kappa
    
    # check accuracy
    pred_errors <- mean(pred != labels)
    svm_accuracy[[name]] <- 1 - pred_errors
    
    # calculate accuracy splitting on dead and alive trees
    len_alive <- 0
    right_alive <- 0
    len_dead <- 0
    right_dead <- 0
    for (k in 1:length(labels)){
      # depending if the tree originally was alive (0) or dead (1)
      if(labels[k] == 0){
        # calculate prediction on alive trees: 1 ok, 0 error
        pred_alive <- ifelse(pred[k] == 0, 1, 0)
        right_alive <- right_alive + as.numeric(pred_alive)
        len_alive <- len_alive + 1
      } else {
        # calculate prediction on dead trees: 1 ok, 0 error
        pred_dead <- ifelse(pred[k] == 1, 1, 0)
        right_dead <- right_dead + as.numeric(pred_dead)
        len_dead <- len_dead + 1
      }
    }
    # add information to the main list 
    svm_accuracy_alive[[name]] <- right_alive / len_alive
    svm_accuracy_dead[[name]] <- right_dead / len_dead
  }
  
  # stop parallelization
  #stopCluster(cluster)
  #registerDoSEQ()
  
  # exclude temporal variables
  # rm(performance, pred_vs_real, test, train, auc, k, labels, len_alive, len_dead,
  #    n_model, name, pred, pred_alive, pred_dead, pred_errors, dead_col, right_alive,
  #    right_dead, train_ind, kappa, aucpr, mcc, cluster, fitControl)
  
  # save time data
  my_timer <- rbind(my_timer, toc())
  
  # return a list with the results
  results <- list(svm_model, svm_accuracy, svm_accuracy_dead, svm_accuracy_alive, svm_roc, svm_auc, svm_aucpr, 
                  svm_mcc, svm_kappa, my_timer) # svm_threshold not available
  return(results)
}
