#NAIVE SOLUTIONS
setwd("C:/Users/Manuel/OneDrive/Project_Datascience/R/DMC_2017_Seminar/01_Data/")

all.train = subset(read.csv("origin/train.csv", sep = "|"), select = c(pid, revenue))

train_train = head(all.train, (3/4)*length(all.train[[1]]))
test.train = tail(all.train, (1/4)*length(all.train[[1]]))


rmse_own = function(actual, predicted){
  error <- actual - predicted
  rmse = sqrt(mean(error^2))
  
  return(rmse)
}

#1) Impute with the most frequent value
#select the most frequent value
mfv = rownames(table(train_train$revenue))[[1]]
ns1 = rmse_own(test.train$revenue, rep(as.numeric(mfv), length(test.train[[1]])))


#2) Impute with the average
avg = mean(train_train$revenue)
ns2 = rmse_own(test.train$revenue, rep(avg, length(test.train[[1]])))

#3) Impute with the average of each pid
library(sqldf)
test = sqldf("select pid, avg(train_train.revenue) as avg_revenue from train_train group by pid")

avg_all = merge(train_train, test, by = "pid", all.x = TRUE)
ns3 = rmse_own(avg_all$revenue, avg_all$avg_revenue)

#Benchmarks for our sample
sample = read.csv("C:/Users/Manuel/OneDrive/Project_Datascience/R/DMC_2017_Seminar/01_Data/mergedTrain_sampleLarge.csv")
names(sample)[names(sample) == "pid_Clean"] = "pid"

ns1s = rmse_own(sample$revenue, rep(as.numeric(mfv),length(sample[[1]])))
ns2s = rmse_own(sample$revenue, rep(avg, length(sample[[1]])))

avg_sample = merge(sample, test, by = "pid", all.x = TRUE)
avg_sample[is.na(avg_sample)] = 0
ns3s = rmse_own(sample$revenue, avg_sample$avg_revenue)

