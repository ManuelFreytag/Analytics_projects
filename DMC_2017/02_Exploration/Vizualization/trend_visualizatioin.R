library(sqldf)

train = read.csv("01_Data/train.csv", sep = "|", header = TRUE, dec = ".")
class = read.csv("01_Data/class.csv", sep = "|", header = TRUE, dec = ".")
trend_train = read.csv("02_NewFeatures/train_ABC_Trend_AVGActions.csv")
trend_class = read.csv("02_NewFeatures/class_ABC_Trend_AVGActions.csv")

trend_order = read.csv("02_NewFeatures/order_trend_train.csv")

train = merge(train, trend_train, by = "lineID")
class = merge(class, trend_class, by = "lineID")

trend_train = rbind(trend_train, trend_class)

trend = rbind(subset(train, select= colnames(class)), class)

plot_order_trend = sqldf("select (avg(decomp_cyclic_order_norm) + avg(decomp_trend_order_norm)) as trend, stdev(decomp_cyclic_order_norm + decomp_trend_order_norm) as sd, day from trend_order group by day")
plot_count_trend = sqldf("select (avg(decomp_cyclic_counts_norm) + avg(decomp_trend_counts_norm)) as trend, stdev(decomp_cyclic_counts_norm + decomp_trend_counts_norm) as sd, day from trend group by day")
plot_price_trend = sqldf("select (avg(decomp_cyclic_price_norm) + avg(decomp_trend_price_norm)) as trend, stdev(decomp_cyclic_price_norm + decomp_trend_price_norm) as sd, day from trend group by day")

par(mfrow=c(5,2)) # all plots on one page

for(i in c(1:4)){
  tmp = tail(head(plot_count_trend, i*30),30)
  
  #Plot the datapoints +/- std for counts
  heading = paste("combined trend for counts day ", (i-1)*30, "-", (i*30))
  plot(tmp$day, tmp$trend, type = "o", main = heading
       , xlab="days", ylab="trend", ylim=c(-1, 1), col = "red", pch = 20)
  lines(tmp$day, y = (tmp$trend+tmp$sd))
  lines(tmp$day, y= (tmp$trend-tmp$sd))
  
  tmp = tail(head(plot_price_trend, i*30),30)
  
  #Plot the datapoints +/- std for price
  heading = paste("combined trend for price day ", (i-1)*30, "-", (i*30))
  plot(tmp$day, tmp$trend, type = "o", main = heading
       , xlab="days", ylab="trend", ylim=c(-0.02, 0.02), col = "red", pch = 20)
  lines(tmp$day, y = (tmp$trend+tmp$sd))
  lines(tmp$day, y= (tmp$trend-tmp$sd))
}

#Plot overall graph at the end
heading = paste("combined trend for count day 0-120")
plot(plot_count_trend$day, plot_count_trend$trend, type = "o", main = heading
     , xlab="days", ylab="trend", ylim=c(-1, 1), col = "red", pch = 20)
lines(plot_count_trend$day, y = (plot_count_trend$trend+plot_count_trend$sd))
lines(plot_count_trend$day, y= (plot_count_trend$trend-plot_count_trend$sd))


#Plot overall graph at the end
heading = paste("combined trend for price day 0-120")
plot(plot_price_trend$day, plot_price_trend$trend, type = "o", main = heading
     , xlab="days", ylab="trend", ylim=c(-0.02, 0.02), col = "red", pch = 20)
lines(plot_price_trend$day, y = (plot_price_trend$trend + plot_price_trend$sd))
lines(plot_price_trend$day, y= (plot_price_trend$trend-plot_price_trend$sd))