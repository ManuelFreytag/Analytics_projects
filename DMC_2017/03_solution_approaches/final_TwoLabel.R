rm(list = ls())
gc()
library("parallelMap")
library(mlr)
library(sqldf)
set.seed(42)
options(scipen = 999)

#Installations necessary?
if(FALSE){
  install.packages("penalized")
  install.packages("C50")
  install.packages("randomForest")
  install.packages("adabag")
}


#DATA IMPORT AND PREPARATION
originalDataImport = function(){
  
  train_org = read.csv2("joined_train_org_sample.csv")
  names(train_org)[names(train_org)=="revenue"] = "revenue_Clean"
  
  #Remove false predictors
  train_org = subset(train_org, select = -c(X, pid, lineID))
  
  #factor data
  train_org.factor = c("adFlag", "availability", "manufacturer", "group", "content", "unit", 
                       "genericProduct", "salesIndex", "category", "campaignIndex")
  
  for(i in train_org.factor){
    train_org[i] = as.factor(train_org[[i]])
  }
  
  return(train_org)
}
prepDataImport = function(){
  train = read.csv("mergedTrain_sample.csv")
  
  names(train)[names(train)=="order_Clean"] = "order"
  
  #Remove false predictors
  train = subset(train, select = -c(X, pid_Clean, lineID))
  
  #WEEKDAYS ARE MISSING
  train.factor_fix = c("adFlag_Clean", "availability_Clean", "pharmForm_Clean", 
                       "genericProduct_Clean", "salesIndex_Clean", "missingCompetitorPrice", "weekdays")
  
  train.factor_rem = c("manufacturer_Clean", "group_Clean", "content_Clean", "unit_Clean", "category_Clean", "campaignIndex_Clean")
  
  train.factor = c(train.factor_fix, train.factor_rem)
  
  for(i in train.factor){
    train[i] = as.factor(train[[i]])
  }
  
  return(train)
}
sets = list(originalDataImport(), prepDataImport())

removeIDLikeFactors = function(listSets, maxNrFactors){
  #Remove all factorial attributes with nr(factors) > 50
  for(i in c(1:length(listSets))){
    test = colnames(listSets[[i]][, sapply(listSets[[i]], is.factor)])

    rem = c(NULL)
    for(j in test){
      if(length(table(listSets[[i]][j])) > maxNrFactors)
        rem = c(rem, j)
    }
    
    listSets[[i]] = listSets[[i]][, (! colnames(listSets[[i]]) %in% rem)]
  }
  
  
  return(listSets)
}
sets = removeIDLikeFactors(sets, 50)

#MODEL PREPARATION
rdesc = makeResampleDesc("CV", iters = 3)
#learner selection
learners = list(makeLearner("classif.OneR"))

#makeFeatSelWrapper("classif.C50", resampling = rdesc, control = makeFeatSelControlGA(maxit = 10, mutation.rate = 0.1))

#makeLearner("classif.boosting")
#makeFeatSelWrapper("classif.randomForest", resampling = rdesc, control = makeFeatSelControlGA(maxit = 10, mutation.rate = 0.1))

#parameter_sets if necessary
#getParamSet("classif.penalized.lasso")
#lasso.learner = makeLearner("classif.penalized.lasso")
#lasso.learner = setHyperPars(lasso.learner, lambda1 = 10, trace = TRUE)
#lasso.wrapper = makeFeatSelWrapper(lasso.learner, resampling = rdesc, control = makeFeatSelControlGA(maxit = 10, mutation.rate = 0.1))

#learners = append(learners, list(lasso.wrapper))

#make tasks
makeClassifTasks = function(listSets, target, rem = NULL){
  tasks = list()
  
  for(i in c(1:length(listSets))){
    tasks = append(tasks, list(makeClassifTask(id = as.character(i), data = subset(listSets[[i]], select = -c(quantity, revenue_Clean)), target = target)))
  }
  
  return(tasks)
}

#Remove quantity for prediction
tasks = makeClassifTasks(sets, "order")

#RESAMPLING / TESTING DECSISION
rdesc2 = makeResampleDesc("CV", iters = 5)

#EVALUATION
#parralization
#parallelStartBatchJobs()
parallelStartSocket(2, level = "mlr.resample")
#Setting up a benchmarking experiment

bmr = benchmark(learners, tasks, rdesc2, list(acc))

parallelStop()

write.csv(bmr, "classification_results.csv")



#REGRESION OVER QUANTITY
reg_learners = list(makeLearner("regr.lm"))

makeRegrTasks = function(listSets, target){
  regTasks = list()
  
  for(i in c(1:length(listSets))){
    newTask = makeRegrTask(id = as.character(i), data = subset(listSets[[i]], select = -c(order, revenue_Clean)), target = target)
    regTasks = append(regTasks, list(newTask))
  }
  
  return(regTasks)
}

#make reduced sets for the model training
redSets = list()
for(i in c(1:length(sets))){
  #1) get all with quantity > 0
  
  tmp = createDummyFeatures(sets[[i]])
  
  tmp = tmp[which(tmp$quantity > 0),]
  
  redSets = append(redSets, list(tmp))
  
}

regTasks.learn = makeRegrTasks(redSets, "quantity")

parallelStartSocket(2, level = "mlr.resample")

bmr_reg = benchmark(learners = reg_learners, tasks = regTasks.learn)

parallelStop()



performance_combination = function(sets, bmr_class, bmr_rev){
  #create predict tasks
  tmp_sets = list()
  for(i in c(1:length(sets))){
    tmp = createDummyFeatures(sets[[i]])
    tmp_sets = append(tmp_sets, list(tmp))
    
  }
  regTasks.pred = makeRegrTasks(tmp_sets, "quantity")
  
  rev_predictions = list()
  
  for(i in bmr_class$results){
    
    #Iterating over the tasks
    for(j in length(i)){
      tmp = subset(sets[[j]], select = c(revenue_Clean))
      
      #reduce it to the size of regr
      for(k in bmr_reg$results[[j]]){
        
        tmp["ID"] = i[[j]]$pred$data$id
        tmp["numericPred"] = predict(k$models[[1]], regTasks.pred[[j]])$data$response
        tmp["classPred"] = as.numeric(i[[j]]$pred$data$response)
        
      }
      
      rev_predictions = append(rev_predictions, list(tmp))
    }
  }
  
  return(rev_predictions)
}
comb_preds = performance_combination(sets, bmr, bmr_reg)

revenue_calculation = function(comb_preds){
  ret = list()
  for(i in comb_preds){
    tmp = i
    tmp["revenuePrediction"] = tmp$numericPred * tmp$classPred
    ret = append(ret, list(tmp))
  }
  
  return(ret)
}

rev_preds = revenue_calculation(comb_preds)

rmse_calc = function()

#Final evaluation

rmse_own = function(actual, predicted){
  error <- actual - predicted
  rmse = sqrt(mean(error^2))
  
  return(rmse)
}

for(i in rev_preds){
  print(rmse_own(i$revenue_Clean, i$revenuePrediction))
}


final_ev = data.frame(classifier = character(), regression = character(), rmse = integer())

bmr_reg$results$`1`$regr.lm$models[[1]]

#Final evaluation of the models
#MSE

#Get confusion matrix of all learner
library(SDMTools)
confusion.matrix(getBMRPredictions(bmr, learner.ids = "classif.OneR", as.df = TRUE)$truth, getBMRPredictions(bmr, learner.ids = "classif.OneR", as.df = TRUE)$response)

prediction = getBMRPredictions(bmr, learner.ids = "classif.OneR", as.df = TRUE)
calculateConfusionMatrix(prediction)
table(as.numeric(getBMRPredictions(bmr, learner.ids = "classif.OneR", as.df = TRUE)$truth))
table(as.numeric(getBMRPredictions(bmr, learner.ids = "classif.OneR", as.df = TRUE)$response))

sqldf("select CAST(t1.tr AS float) / CAST(count(*) AS float) as acc  from prediction, (select count(*) as tr from prediction where truth = response) as t1")
