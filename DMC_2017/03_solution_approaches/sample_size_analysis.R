rm(list = ls())
gc()
library(mlr)
library(party)
library(parallelMap)
set.seed(42)
options(scipen = 999)

#Get all colnames except
library(sqldf)

prepDataImport = function(){
  train = read.csv("C:/Users/manuf/OneDrive/Project_Datascience/R/DMC_2017_Seminar/01_Data/mergedTrain.csv")
  
  #Remove complex attributes
  train = subset(train, select = -c(lineID, pid_Clean, click_Clean, basket_Clean))
  #Remove false predictors
  train = subset(train, select = -c(quantity, order_Clean))
  #Remove redundant information
  train = subset(train, select = -c(rrp_Clean, minPrice, maxPrice, avgPrice, mean_basket))
  
  #Remove perfect collinear attributes
  
  names(train)[names(train)=="order_Clean"] = "order"
  names(train)[names(train)=="price_Clean"] = "price"
  
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


data = prepDataImport()

removeIDLikeFactors = function(data, maxNrFactors){
  #Remove all factorial attributes with nr(factors) > 50
  
  coln = colnames(data[, sapply(data, is.factor)])
  rem = c(NULL)
  
  for(j in coln){
    if(length(table(data[j])) > maxNrFactors)
      rem = c(rem, j)
  }
  
  data = data[, (!colnames(data) %in% rem)]
  
  return(data)
}
data = removeIDLikeFactors(data, 50)

data = createDummyFeatures(data, target = ("revenue_Clean"), method = "reference")

#START NEW
data.test = sqldf("select * from data where day_Clean > 75")
data.train = sqldf("select * from data where day_Clean <= 75")
rm(data)

data = normalizeFeatures(data, method = "standardize", target = "revenue_Clean")

calculate_sample_preds = function(data.train, data.test, step_max, max_size){
  perf_list1 = c()
  perf_list2 = c()
  
  steps_nr = c(1:step_max)
  step_size = max_size/step_max
  steps = step_size*steps_nr
  
  for(i in steps){
    s_rows = sample(rownames(data.train), i)
    tmp.train = data.train[s_rows,]
    tmp.all = rbind(tmp.train, data.test)
    rownames(tmp.all) = c(1:length(tmp.all[[1]]))
    
    #create block to force a equal test set!
    desc = makeResampleDesc("Holdout", split = (length(tmp.train[[1]])/length(tmp.all[[1]])))
    task = makeRegrTask(id = as.character(i), data = tmp.all, target = "revenue_Clean")
    
    #lrn = makeLearner("regr.cforest", ntree = 50)
    #lrn = makeLearner("regr.randomForestSRC", ntree = 50)
    lrn = makeLearner("regr.rpart")
    #lrn = makeLearner("regr.lm")
    #lrn = makeLearner("regr.blackboost")
    
    model = train(lrn, task, subset = c(1:length(tmp.train[[1]])))
    pred = predict(model, task = task, subset = c(length(tmp.train[[1]]):length(tmp.all[[1]])))
    print(i)
    
    perf = performance(pred, model = model, measures = list(rmse, timetrain))
    print(perf)
    perf_list1 = c(perf_list1, perf[[1]])
    perf_list2 = c(perf_list2, perf[[2]])
    
  }
  
  return(list(steps, perf_list1, perf_list2))
}

?ctree

steps = 10
max_size =1000000
tasks_u_descs = calculate_sample_preds(data.train, data.test, steps, max_size = max_size)

#Bind train and test and create resample decision to remember the testing criterion
# lrn1 = makeLearner("regr.ctree")
# lrn2 = makeFilterWrapper(makeLearner("regr.lm"), fw.method = "chi.squared", fw.threshold = 1, fw.mandatory.feat = col.keep)
plot(tasks_u_descs[[1]], tasks_u_descs[[3]], xlab = "sample_size", ylab = "compuation time in seks", ylim = c(0,300))
par(new = T)
plot(tasks_u_descs[[1]], tasks_u_descs[[2]], type = "l", axes=F, xlab=NA, ylab=NA, ylim = c(9,11))
axis(side = 4)

export = data.frame(step = tasks_u_descs[[1]], rmse = tasks_u_descs[[2]], time = tasks_u_descs[[3]])

write.csv(export, "sample_size_analysis/sample_perf_ctree.csv")

#rm(data.test, data.train)

