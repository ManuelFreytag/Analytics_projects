rm(list = ls())
library(sqldf)
library(plyr)
options(scipen=999)

#The working directory should be changed of course
train = read.csv("01_Data/train.csv", sep = "|", header = TRUE, dec = ".")
train = rename(train, c("order" = "label"))

#TREND ANALYSIS
#1) Filter price, pid, day
trend = sqldf("select lineID, pid, label, day from train")

#2) generate list of all pids, ordered by occurance
ord_pids = sqldf("select pid from (select count(pid) as cpid, pid 
                 from trend group by pid) as t1 order by t1.cpid desc")


#Definition of a short function for trend decomposition into a data.frame
decomposition = function(x, y, pid, freq = 7){
  ts_tmp = ts(y, frequency = freq)
  decompose_price = stl(ts_tmp, "periodic")
  
  #Cyclic component
  app = data.frame(day = x)
  app["pid"] = pid
  app["cyclic"] = as.numeric(decompose_price$time.series[,1])
  
  #initializing it with trend 0
  trend_series = c(0)
  
  for(j in c(2:max_day)){
    trend_series = c(trend_series, (decompose_price$time.series[j,2]-decompose_price$time.series[j-1,2]))
  }
  
  app["trend"] = mean(trend_series)
  
  return(app)
}


#OPERATIVE PART OF THE TREND ANALYISIS
#Defining important vectors and data.frames to be filled
trend_order_vars = c()
trend_order_perc = c()

max_day = max(trend$day)
day_df = sqldf("select day from trend group by day")

decomposition_order_df = data.frame(day = integer(), pid = character(), cyclic = double(), trend = double())

counter = 0
#Starting the training process
for(i in ord_pids$pid){
  counter = counter + 1
  print(counter)
  
  #Select next pid
  tmp = trend[trend$pid == i,]
  
  #3) Average the order values of each product sold on each day       
  trend_tmp = sqldf("select day_df.day, t1.avg_label
                    from day_df left join (select day, CAST(avg(label) as float) as avg_label from tmp group by day) as t1 on t1.day = day_df.day")
  
  #GENERAL TREND ANALYSIS
  #Check if the label is changing, else skip it
  if(sum(is.na(trend_tmp$avg_label)) == 0){
    #if(length(table(trend_tmp$)) > 1){}
    
    #Calculate the general trend variable for order

    app = decomposition(trend_tmp$day, trend_tmp$avg_label, i)
    
    #Append to final df
    decomposition_order_df = rbind(decomposition_order_df, app)

  }
  
  fm = lm(trend_tmp$avg_label ~ trend_tmp$day)
  trend_order_vars = c(trend_order_vars, fm$coefficients[2])
  trend_order_perc = c(trend_order_perc, fm$coefficients[2]/fm$coefficients[1])
}

#JOINING RESULTS INTO A DF
#Average the results
final_trend = sqldf("select trend.lineID, decomposition_order_df.pid, decomposition_order_df.day, decomposition_order_df.cyclic as decomp_cyclic_order_norm, decomposition_order_df.trend as decomp_trend_order_norm 
                    from trend join decomposition_order_df on (decomposition_order_df.pid = trend.pid and decomposition_order_df.day = trend.day)")

#Join the averaged results to it.
final_trend = sqldf("select final_trend.*, pids.trend_order, pids.trend_order_perc from final_trend join pids on final_trend.pid = pids.pid")

train_export = subset(sqldf("select train.lineID, final_trend.*
                     from train left join final_trend on train.lineID = final_trend.lineID"), select = -c(2,3,4))

train_export[is.na(train_export)] = 0

write.csv(round(train_export, digits = 5), "order_trend_train.csv", row.names = FALSE)

summary(decomposition_order_df)

#JOIN TO THE CLASS SET
class = subset(read.csv("01_Data/class.csv", sep = "|", header = TRUE, dec = "."), select = c(lineID, day, pid))
class["lineID"] = c(max(train$lineID)+1, (max(train$lineID)+length(class[[1]])))

reqStart = max(train$day)%%7
perZero = min(class$day)


#recalculate day for the joining possibility (which is day 2 at min)
class["day"] = class$day - perZero + reqStart + 1

class_export = sqldf("select class.lineID, cyclic as decomp_cyclic_order_norm, trend as decomp_trend_order_norm from class left join decomposition_order_df on (class.pid = decomposition_order_df.pid and class.day = decomposition_order_df.day)")
class_export[is.na(class_export)] = 0
write.csv(round(class_export, digits = 5), "order_trend_class.csv", row.names = FALSE)
