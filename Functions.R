Create_features_from_decision_forest = function(dat, K , N, dat_test){
  #this creates K  times N  decision trees from dat. The decision rules generated are evaluated
  #on dat and dat_test (which is not used to generate decision rules). 
  
  n= nrow(dat)
  dat=data.frame(dat)
  
  #initialise matrix to store new features in
  all_new_features = matrix(ncol=0, nrow=nrow(dat))
  all_new_features_test = matrix(ncol=0, nrow=nrow(dat_test))
  
  for (j in 1:N){
    
    #generate the random splits of data into K folds
    flds <- createFolds(1:n, k = K)
    
    for (k in 1:K){
      
      #randomly split the data
      train = dat[-flds[[k]],]
     
      
      #fit a decision tree on the training split
      fit1 = rpart(quality~., data=train)
      
      #extract the decision rules
      splits = fit1$splits
      
      new_features = matrix(nrow=nrow(dat), ncol=nrow(splits))
      new_features_test = matrix(nrow=nrow(dat_test), ncol=nrow(splits))
      
      for (i in 1:nrow(splits)){
        #evaluate the decision rules on each datapoint 
        new_features[,i] = dat[,rownames(splits)[i]]>=splits[i,"index"]
        new_features_test[,i] = dat_test[,rownames(splits)[i]]>=splits[i,"index"]
      }
      
      colnames(new_features) = paste0(rownames(splits),">=",round(splits[,"index"],2))
      colnames(new_features_test) = paste0(rownames(splits),">=",round(splits[,"index"],2))
      
      all_new_features = cbind(all_new_features, new_features)
      all_new_features_test = cbind(all_new_features_test, new_features_test)
    }
  }
  ;
 list( all_new_features=all_new_features, all_new_features_test=all_new_features_test)
}


cv.error = function(dat,x_vars,y_var, K , N, fit_and_predict){
  n= nrow(dat)
  sum_of_squared_diffs = matrix(nrow = N, ncol = K)
  sum_of_abs_diffs = matrix(nrow = N, ncol = K)
  sum_of_abs_diffs_rounded = matrix(nrow = N, ncol = K)
  sum_of_squared_diffs_rounded = matrix(nrow = N, ncol = K)
  
  #Repeat the splitting into test and training N times and take average of performance metrics
  for (j in 1:N){
    
    #split into K random folds of data
    flds <- createFolds(1:n, k = K)
    for (k in 1:K){
      
      #assign to test of training based on the fold
      train = dat[-flds[[k]],]
      test = dat[flds[[k]],]
      
      #fit model on training and evaluate on test
      predictions = fit_and_predict(train = train, test = test, x_vars = x_vars, y_var = y_var)
      
      #rounded predictions
      predictions_rounded = round(predictions)
      
      #model evaluation metrics, RMSE and MAE (looking at both rounded and non rounded predictions)
      sum_of_squared_diffs[j,k] = sum((predictions - test[,y_var])^2)
      sum_of_abs_diffs[j,k] = sum(abs(predictions - test[,y_var]))
      sum_of_squared_diffs_rounded[j,k] = sum((predictions_rounded - test[,y_var])^2)
      sum_of_abs_diffs_rounded[j,k] = sum(abs(predictions_rounded - test[,y_var]))
    }
  }
 
  average_RMSE = mean(sqrt(apply(sum_of_squared_diffs,1,sum)/n))
  average_MAE = mean(apply(sum_of_abs_diffs,1,sum)/n)
  average_RMSE_rounded = mean(sqrt(apply(sum_of_squared_diffs_rounded,1,sum)/n))
  average_MAE_rounded = mean(apply(sum_of_abs_diffs_rounded,1,sum)/n)
  ;
  list(average_RMSE = average_RMSE, average_MAE =average_MAE, average_RMSE_rounded =average_RMSE_rounded,  average_MAE_rounded= average_MAE_rounded )
}



lasso_fit_and_predict = function(train, test, x_vars, y_var){
  mod = fit_lasso(train[,x_vars], train[,y_var])
  pred = predict(mod, newx=test[,x_vars])
  ;
  round(pred)
}



random_Forest =function(train, test, x_vars, y_var){
  fit1 = randomForest(x=train[,x_vars], y=train[,y_var] , xtest = test[,x_vars], ytest=test[,y_var])
  pred =  fit1$test$predicted
  ;
  pred
}

add_interaction_terms = function(X){
  X1 = apply(X,2,scale)
  for ( i in 2:ncol(X)){
    for (j in 1:(i-1)){
      X_new = scale(X[,i])*scale(X[,j])
      X1 = cbind(X1, X_new)
      colnames(X1)[ncol(X1)] = paste0(colnames(X)[i],"_",colnames(X)[j])
    }
  } 
  ;
  X1
}

scale_if_numeric = function(x){
  if(is.numeric(x)){x = scale(x)}
  ;
  x
}

fit_lasso = function(X,y){
  X = apply(X,2,scale_if_numeric) #Centre and standardise all the predictors. 
  
  #K fold cross validation to estimate the value of the penalty variable
  cv.ft = cv.glmnet(x=X, y = y, nfolds=5)
  #plot(cv.ft)
  
  #extract the penalty lambda than minimises the cross validated mean squared error
  lambda = exp(log(cv.ft$lambda.1se))
  
  #fit the lasso
  ft = glmnet(x=X, y=y, lambda =lambda)
  ;
  ft
}






RuleFit = function(train, test, y_var,...){
  #create the features from the training data only
  Feature_matrix_red = Create_features_from_decision_forest(train, K=5 , N=2, test)

  #add the features to the X matrix for Lasso fitting
  X_test = cbind(Feature_matrix_red$all_new_features_test, test[,-y_var])
  X_train =cbind( Feature_matrix_red$all_new_features, train[,-y_var])
  
  #Fit a lasso
  ft.lasso.red = fit_lasso(X = X_train, y = train[,y_var])
  
  #Us the model to predict the wine quality scores of test data
  test_predictions = predict(ft.lasso.red, newx = X_test)
 
  ;
  test_predictions

}


add_logs = function(dat){
  dat = dat %>% mutate (log.chlorides = log (chlorides), log.residual.sugar =  log(residual.sugar), 
                        log.free.sulfur.dioxide = log(free.sulfur.dioxide))
}


