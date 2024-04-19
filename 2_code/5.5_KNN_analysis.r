#------------------------------------------------------------------------------------------#
####                              K Nearest Neighbor analysis                                
#                                                                                          #
#                            Aitor Vázquez Veloso, 12/05/2023                              #
#                              Last modification: 18/01/2024                               #
#------------------------------------------------------------------------------------------#


KNN_analysis_parallel <- function(df_study, my_combis, my_timer){
  
  # activate timer
  tic("KNN")
  
  # model number
  n_model = 0
  
  # lists: models and accuracy
  knn_model <- list()
  knn_accuracy <- list()
  knn_accuracy_dead <- list()
  knn_accuracy_alive <- list()
  knn_roc <- list()
  knn_auc <- list()
  knn_aucpr <- list()
  knn_mcc <- list()
  knn_kappa <- list()
  knn_threshold <- list()
  
  # parallelization
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  
  for (k in my_combis){
    
    # model name
    n_model = n_model + 1
    name <- paste('knn_model_', n_model, sep = '')
    
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
    
    # finding the K value as sqrt(nrow(df)) - one way to calculate it
    k_value <- sqrt(nrow(train))
    
    # fit control including parallelization
    fitControl <- trainControl(method = "cv", # cross-validation
                               number = 10, # 10 k-folds
                               allowParallel = TRUE) # parallelization
    
    # fit the model
    tic(name)
    knn_model[[name]] <- train(as.factor(dead) ~ ., 
                               data = train, 
                               method = 'knn', 
                               tuneGrid = expand.grid(k = k_value), # k_value
                               trControl = fitControl)
    my_timer <- rbind(my_timer, toc())
    print(paste('Model ', name, ' run succesfully.', sep = ''))
    
    # make a prediction 
    pred <- predict(knn_model[[name]], newdata = test, type = 'prob')
    
    # choose the best threshold based on MCC
    t_mcc <- 0
    threshold <- 0
    for(t in seq(0.001, 0.999, by = 0.001)){
      t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
      t_pred_vs_real <- ROCR::prediction(t_pred, labels)
      t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
      if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
         t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
        t_mcc <- t_mcc_tmp@y.values[[1]][2]
        threshold <- t
      } 
    }
    
    # record threshold information
    knn_threshold[[name]] <- threshold
    
    # binary classify predictions
    pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
    
    # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
    pred_vs_real <- ROCR::prediction(pred, labels)
    performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
    knn_roc[[name]] <- performance
    
    # AUC is the Area Under the Curve - TPR vs FPR 
    auc <- ROCR::performance(pred_vs_real, measure = "auc")
    auc <- auc@y.values[[1]]
    knn_auc[[name]] <- auc
    
    # AUCPR - TPR vs PPV - Precision/Recall
    aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
    aucpr <- aucpr@y.values[[1]]
    knn_aucpr[[name]] <- aucpr
    
    # MCC - Matthews correlation coefficient or phi coefficient
    mcc <- ROCR::performance(pred_vs_real, measure = "phi")
    mcc <- mcc@y.values[[1]]
    knn_mcc[[name]] <- mcc
    
    # Cohen's kappa coefficient 
    kappa <- kappa2(cbind(pred, labels))
    knn_kappa[[name]] <- kappa
    
    # check accuracy
    pred_errors <- mean(pred != labels)
    knn_accuracy[[name]] <- 1 - pred_errors
    
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
    knn_accuracy_alive[[name]] <- right_alive / len_alive
    knn_accuracy_dead[[name]] <- right_dead / len_dead
  }
  
  # stop parallelization
  stopCluster(cluster)
  registerDoSEQ()
  
  # exclude temporal variables
  # rm(performance, pred_vs_real, test, train, auc, k, k_value, labels, len_alive, len_dead,
  #    n_model, name, pred, pred_alive, pred_dead, pred_errors, dead_col, right_alive,
  #    right_dead, train_ind, kappa, aucpr, mcc, threshold, t, t_mcc, t_pred,
  #    t_mcc_tmp, t_pred_vs_real, cluster, fitControl)
  
  # save time data
  my_timer <- rbind(my_timer, toc())
  
  # return a list with the results
  results <- list(knn_model, knn_accuracy, knn_accuracy_dead, knn_accuracy_alive, knn_roc, knn_auc, knn_aucpr, 
                  knn_mcc, knn_kappa, knn_threshold, my_timer)  
  return(results)
}


KNN_analysis <- function(df_study, my_combis, my_timer){
  
  # activate timer
  tic("KNN")
  
  # model number
  n_model = 0
  
  # lists: models and accuracy
  knn_model <- list()
  knn_accuracy <- list()
  knn_accuracy_dead <- list()
  knn_accuracy_alive <- list()
  knn_roc <- list()
  knn_auc <- list()
  knn_aucpr <- list()
  knn_mcc <- list()
  knn_kappa <- list()
  knn_threshold <- list()
  
  # parallelization
  #cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  #registerDoParallel(cluster)
  
  for (k in my_combis){
    
    # model name
    n_model = n_model + 1
    name <- paste('knn_model_', n_model, sep = '')
    
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
    
    # finding the K value as sqrt(nrow(df)) - one way to calculate it
    k_value <- sqrt(nrow(train))
    
    # fit control including parallelization
    fitControl <- trainControl(method = "cv", # cross-validation
                               number = 10, # 10 k-folds
                               allowParallel = TRUE) # parallelization
    
    # fit the model
    tic(name)
    knn_model[[name]] <- train(as.factor(dead) ~ ., 
                               data = train, 
                               method = 'knn', 
                               tuneGrid = expand.grid(k = k_value), # k_value
                               trControl = fitControl)
    my_timer <- rbind(my_timer, toc())
    print(paste('Model ', name, ' run succesfully.', sep = ''))
    
    # make a prediction 
    pred <- predict(knn_model[[name]], newdata = test, type = 'prob')
    
    # choose the best threshold based on MCC
    t_mcc <- 0
    threshold <- 0
    for(t in seq(0.001, 0.999, by = 0.001)){
      t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
      t_pred_vs_real <- ROCR::prediction(t_pred, labels)
      t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
      if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
         t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
        t_mcc <- t_mcc_tmp@y.values[[1]][2]
        threshold <- t
      } 
    }
    
    # record threshold information
    knn_threshold[[name]] <- threshold
    
    # binary classify predictions
    pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
    
    # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
    pred_vs_real <- ROCR::prediction(pred, labels)
    performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
    knn_roc[[name]] <- performance
    
    # AUC is the Area Under the Curve - TPR vs FPR 
    auc <- ROCR::performance(pred_vs_real, measure = "auc")
    auc <- auc@y.values[[1]]
    knn_auc[[name]] <- auc
    
    # AUCPR - TPR vs PPV - Precision/Recall
    aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
    aucpr <- aucpr@y.values[[1]]
    knn_aucpr[[name]] <- aucpr
    
    # MCC - Matthews correlation coefficient or phi coefficient
    mcc <- ROCR::performance(pred_vs_real, measure = "phi")
    mcc <- mcc@y.values[[1]]
    knn_mcc[[name]] <- mcc
    
    # Cohen's kappa coefficient 
    kappa <- kappa2(cbind(pred, labels))
    knn_kappa[[name]] <- kappa
    
    # check accuracy
    pred_errors <- mean(pred != labels)
    knn_accuracy[[name]] <- 1 - pred_errors
    
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
    knn_accuracy_alive[[name]] <- right_alive / len_alive
    knn_accuracy_dead[[name]] <- right_dead / len_dead
  }
  
  # stop parallelization
  #stopCluster(cluster)
  #registerDoSEQ()
  
  # exclude temporal variables
  # rm(performance, pred_vs_real, test, train, auc, k, k_value, labels, len_alive, len_dead,
  #    n_model, name, pred, pred_alive, pred_dead, pred_errors, dead_col, right_alive,
  #    right_dead, train_ind, kappa, aucpr, mcc, threshold, t, t_mcc, t_pred,
  #    t_mcc_tmp, t_pred_vs_real, cluster, fitControl)
  
  # save time data
  my_timer <- rbind(my_timer, toc())
  
  # return a list with the results
  results <- list(knn_model, knn_accuracy, knn_accuracy_dead, knn_accuracy_alive, knn_roc, knn_auc, knn_aucpr, 
                  knn_mcc, knn_kappa, knn_threshold, my_timer)  
  return(results)
}



KNN_analysis_timer <- function(df_study, my_combis, my_timer){
  
  # # activate timer
  # tic("KNN")
  # 
  # # model number
  # n_model = 0
  # 
  # # lists: models and accuracy
  # knn_model <- list()
  # knn_accuracy <- list()
  # knn_accuracy_dead <- list()
  # knn_accuracy_alive <- list()
  # knn_roc <- list()
  # knn_auc <- list()
  # knn_aucpr <- list()
  # knn_mcc <- list()
  # knn_kappa <- list()
  # knn_threshold <- list()
  # 
  # # parallelization
  # #cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  # #registerDoParallel(cluster)
  
  for (k in my_combis){
    
    # # model name
    # n_model = n_model + 1
    # name <- paste('knn_model_', n_model, sep = '')
    
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
    
    # finding the K value as sqrt(nrow(df)) - one way to calculate it
    k_value <- sqrt(nrow(train))
    
    # fit control including parallelization
    fitControl <- trainControl(method = "cv", # cross-validation
                               number = 10, # 10 k-folds
                               allowParallel = TRUE) # parallelization
    
    # fit the model
    tic(paste('KNN_', length(k), '_', nrow(df), sep = ''))
    train(as.factor(dead) ~ ., 
    # knn_model[[name]] <- train(as.factor(dead) ~ ., 
                               data = train, 
                               method = 'knn', 
                               tuneGrid = expand.grid(k = k_value), # k_value
                               trControl = fitControl)
    my_timer <- rbind(my_timer, toc())
  }
  #   print(paste('Model ', name, ' run succesfully.', sep = ''))
  #   
  #   # make a prediction 
  #   pred <- predict(knn_model[[name]], newdata = test, type = 'prob')
  #   
  #   # choose the best threshold based on MCC
  #   t_mcc <- 0
  #   threshold <- 0
  #   for(t in seq(0.001, 0.999, by = 0.001)){
  #     t_pred <- ifelse(pred[, 'yes'] > t, 1, 0)
  #     t_pred_vs_real <- ROCR::prediction(t_pred, labels)
  #     t_mcc_tmp <- ROCR::performance(t_pred_vs_real, measure = "phi")
  #     if(t_mcc_tmp@y.values[[1]][2] != 'NaN' &
  #        t_mcc < t_mcc_tmp@y.values[[1]][2]){ # save the value closer to 1
  #       t_mcc <- t_mcc_tmp@y.values[[1]][2]
  #       threshold <- t
  #     } 
  #   }
  #   
  #   # record threshold information
  #   knn_threshold[[name]] <- threshold
  #   
  #   # binary classify predictions
  #   pred <- ifelse(pred[, 'yes'] > threshold, 1, 0)
  #   
  #   # ROC curve, plotting TPR (True Positive Rate) against the false positive rate (FPR) 
  #   pred_vs_real <- ROCR::prediction(pred, labels)
  #   performance <- ROCR::performance(pred_vs_real, measure = "tpr", x.measure = "fpr")
  #   knn_roc[[name]] <- performance
  #   
  #   # AUC is the Area Under the Curve - TPR vs FPR 
  #   auc <- ROCR::performance(pred_vs_real, measure = "auc")
  #   auc <- auc@y.values[[1]]
  #   knn_auc[[name]] <- auc
  #   
  #   # AUCPR - TPR vs PPV - Precision/Recall
  #   aucpr <- ROCR::performance(pred_vs_real, measure = "aucpr")
  #   aucpr <- aucpr@y.values[[1]]
  #   knn_aucpr[[name]] <- aucpr
  #   
  #   # MCC - Matthews correlation coefficient or phi coefficient
  #   mcc <- ROCR::performance(pred_vs_real, measure = "phi")
  #   mcc <- mcc@y.values[[1]]
  #   knn_mcc[[name]] <- mcc
  #   
  #   # Cohen's kappa coefficient 
  #   kappa <- kappa2(cbind(pred, labels))
  #   knn_kappa[[name]] <- kappa
  #   
  #   # check accuracy
  #   pred_errors <- mean(pred != labels)
  #   knn_accuracy[[name]] <- 1 - pred_errors
  #   
  #   # calculate accuracy splitting on dead and alive trees
  #   len_alive <- 0
  #   right_alive <- 0
  #   len_dead <- 0
  #   right_dead <- 0
  #   for (k in 1:length(labels)){
  #     # depending if the tree originally was alive (0) or dead (1)
  #     if(labels[k] == 0){
  #       # calculate prediction on alive trees: 1 ok, 0 error
  #       pred_alive <- ifelse(pred[k] == 0, 1, 0)
  #       right_alive <- right_alive + as.numeric(pred_alive)
  #       len_alive <- len_alive + 1
  #     } else {
  #       # calculate prediction on dead trees: 1 ok, 0 error
  #       pred_dead <- ifelse(pred[k] == 1, 1, 0)
  #       right_dead <- right_dead + as.numeric(pred_dead)
  #       len_dead <- len_dead + 1
  #     }
  #   }
  #   # add information to the main list 
  #   knn_accuracy_alive[[name]] <- right_alive / len_alive
  #   knn_accuracy_dead[[name]] <- right_dead / len_dead
  # }
  # 
  # # stop parallelization
  # #stopCluster(cluster)
  # #registerDoSEQ()
  # 
  # # exclude temporal variables
  # # rm(performance, pred_vs_real, test, train, auc, k, k_value, labels, len_alive, len_dead,
  # #    n_model, name, pred, pred_alive, pred_dead, pred_errors, dead_col, right_alive,
  # #    right_dead, train_ind, kappa, aucpr, mcc, threshold, t, t_mcc, t_pred,
  # #    t_mcc_tmp, t_pred_vs_real, cluster, fitControl)
  # 
  # # save time data
  # my_timer <- rbind(my_timer, toc())
  # 
  # # return a list with the results
  # results <- list(knn_model, knn_accuracy, knn_accuracy_dead, knn_accuracy_alive, knn_roc, knn_auc, knn_aucpr, 
  #                 knn_mcc, knn_kappa, knn_threshold, my_timer)  
  # return(results)
  return(my_timer)
}
