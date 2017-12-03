library(sqldf)
options(scipen=999)

#The working directory should be changed of course
train = read.csv("01_Data/train.csv", sep = "|", header = TRUE, dec = ".")
class = read.csv("01_Data/class.csv", sep = "|", header = TRUE, dec = ".")

class["lineID"] = c((max(train$lineID)+1):(max(train$lineID)+max(class$lineID)))

df_all = rbind(subset(train, select= colnames(class)), class)

#TREND ANALYSIS
#1) Filter price, pid, day

trend = sqldf("select lineID, pid, price, day from df_all")

#2) generate list of all pids, ordered by occurance
ord_pids = sqldf("select pid from (select count(pid)as cpid, pid 
                 from trend group by pid) as t1 order by t1.cpid desc")


#Definition of a short function for trend decomposition into a data.frame
decomposition = function(x, y, pid, freq = 7){
  ts_tmp = ts(y, frequency = freq)
  decompose_price = stl(ts_tmp, "periodic")
  
  #Cyclic component
  app = data.frame(day = x)
  app["pid"] = pid
  #Normalize to size of price and counts, in order to enable distance equal numbers.
  app["cyclic"] = as.numeric(decompose_price$time.series[,1]/mean(y))
  
  #initializing it with trend 0
  trend_series = c(0)
  
  for(j in c(2:max_day)){
    trend_series = c(trend_series, (decompose_price$time.series[j,2]-decompose_price$time.series[j-1,2]))
  }
  
  #Normalize to size of price and counts, in order to enable distance equal numbers.
  app["trend"] = trend_series/mean(y)
  
  return(app)
}

#OPERATIVE PART OF THE TREND ANALYISIS
#Defining important vectors and data.frames to be filled
trend_price_vars = c()
trend_price_perc = c()
trend_count_vars = c()
trend_count_perc = c()

max_day = max(trend$day)
day_df = sqldf("select day from trend group by day")

decomposition_price_df = data.frame(day = integer(), pid = character(), cyclic = double(), trend = double())
decomposition_counts_df = data.frame(day = integer(), pid = character(), cyclic = double(), trend = double())

#Starting the training process
for(i in ord_pids$pid){
  #Select next pid
  tmp = trend[trend$pid == i,]
  
  #3) Average the price values of each product sold on each day       
  trend_tmp = sqldf("select day_df.day, t1.price, t1.counts 
                    from day_df left join (select day, avg(price) as price, count(price) as counts from tmp group by day) as t1 on t1.day = day_df.day")
  
  #Impute missing counts with 0
  trend_tmp$counts[is.na(trend_tmp$counts)] = 0
  
  #GENERAL TREND ANALYSIS
  #Calculate the general trend variable for price and counts
  fm = lm(trend_tmp$price ~ trend_tmp$day)
  trend_price_vars = c(trend_price_vars, fm$coefficients[2])
  trend_price_perc = c(trend_price_perc, fm$coefficients[2]/fm$coefficients[1])
  
  fm2 = lm(trend_tmp$counts ~ trend_tmp$day)
  trend_count_vars = c(trend_count_vars, fm2$coefficients[2])
  trend_count_perc = c(trend_count_perc, fm2$coefficients[2]/fm$coefficients[1])
  
  
  #TREND DECOMPOSITION
  #Trend decomposition price:
  #Check if it is complete (has entry for each day), else skip it
  if(sum(is.na(trend_tmp$price)) == 0){
    
    app = decomposition(trend_tmp$day, trend_tmp$price, i)
    
    #Append to final df
    decomposition_price_df = rbind(decomposition_price_df, app)
  }
  
  #Trend decomposition counts
  app = decomposition(trend_tmp$day, trend_tmp$counts, i)
  decomposition_counts_df = rbind(decomposition_counts_df, app)
  
}

#Free memory
rm(app, day_df, tmp, trend_tmp)

#JOINING RESULTS INTO A DF
final_trend = sqldf("select trend.lineID, decomposition_counts_df.pid, decomposition_counts_df.day, decomposition_counts_df.cyclic as decomp_cyclic_counts_norm, decomposition_counts_df.trend as decomp_trend_counts_norm 
                    from trend join decomposition_counts_df on (decomposition_counts_df.pid = trend.pid and decomposition_counts_df.day = trend.day)")

final_trend = sqldf("select final_trend.*, decomposition_price_df.cyclic as decomp_cyclic_price_norm, decomposition_price_df.trend as decomp_trend_price_norm
                    from final_trend left join decomposition_price_df on (final_trend.pid = decomposition_price_df.pid and final_trend.day = decomposition_price_df.day)")

#Add the columns to the overall df

linear_trend_names = c("trend_price", "trend_price_perc", "trend_count", "trend_count_perc")
linear_trend_sum = list(trend_price_vars, trend_price_perc, trend_count_vars, trend_count_perc)

for(i in c(1:length(linear_trend_names))){
  ord_pids[linear_trend_names[i]] = linear_trend_sum[i]
}

final_trend = merge(final_trend, ord_pids, by = "pid")

#-> 511 Missing values, because the products only ocure once, therefore impute with 0.
#Value imputation with 0
final_trend[is.na(final_trend)] = 0
final_trend = subset(final_trend, select = -c(pid, day))

#ABC Analysis
#General sql statement for data generation
abc = sqldf("select *
            from (select pid, count(pid) as demand, sum(revenue) as sumrevenue from train group by pid) as t1
            order by sumrevenue desc")

#ABC analysis revenue
#generate cummulated sum
cs_rev = cumsum(abc$sumrevenue)

abc["cs_rev"] = cs_rev
abc["ABC_rev"] = "A"
abc$ABC_rev[abc$cs_rev > sum(abc$sumrevenue)*0.80] = "B"
abc$ABC_rev[abc$cs_rev > sum(abc$sumrevenue)*0.95] = "C"

table(abc$ABC_rev)
#Frequency occurance checks out


#ABC analysis number of sales
#Order by counts first
abc = abc[order(-abc$demand),]

#generate cummulated sum
cs_dem = cumsum(abc$demand)

abc["cs_dem"] = cs_dem
abc["ABC_dem"] = "A"
abc$ABC_dem[abc$cs_dem > sum(abc$demand)*0.80] = "B"
abc$ABC_dem[abc$cs_dem > sum(abc$demand)*0.95] = "C"


#Match the classification to the train and class data
final_ABC = sqldf("select df_all.lineID, abc.ABC_rev, abc.ABC_dem from df_all left join abc on df_all.pid = abc.pid")
rm(abc)

#ADDITINAL FEATURE ENGINEERING
#Order may be missinterpreted
names(train)[names(train) == 'order'] = 'ordered'

additonal_features = sqldf("select pid, avg(price) as mean_price, avg(click) as mean_click, avg(basket) as mean_basket, avg(ordered) as mean_order 
                           from train group by pid")

#EXPORT
final = sqldf("select df_all.lineID, (df_all.price -  af.mean_price) as price_fluctuation, af.mean_click, af.mean_basket, af.mean_order 
              from df_all join additonal_features as af on df_all.pid = af.pid")

df_list = list(round(final_trend, digits = 5), final_ABC, round(final, digits = 5))
final = Reduce(function(x, y) merge(x, y, by = "lineID", all=TRUE), df_list, accumulate=FALSE)

#Imputation for price_fluctuation, mean_click, mean_basket, mean_order
#Imputation is necessary for all product that have no occurance in the training set only in the class set.

final[is.na(final)] = 0

final_train = head(final, nrow(train))
final_class = tail(final, nrow(class))
final_class["lineID"] = c(1:length(class$lineID))
?write.csv

write.csv(head(final, nrow(train)), "train_ABC_Trend_AVGActions.csv", row.names = FALSE)
write.csv(tail(final, nrow(class)), "class_ABC_Trend_AVGActions.csv", row.names = FALSE )