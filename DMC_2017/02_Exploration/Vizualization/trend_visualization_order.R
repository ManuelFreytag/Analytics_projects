library(sqldf)

train = read.csv("01_Data/train.csv", sep = "|", header = TRUE, dec = ".")

trend_order = read.csv("02_NewFeatures/order_trend_train.csv")
train = merge(train, trend_order, by = "lineID")


plot_order_trend = sqldf("select (avg(decomp_cyclic_order_norm) + avg(decomp_trend_order_norm)) as trend, stdev(decomp_cyclic_order_norm + decomp_trend_order_norm) as sd, day from train group by day")

par(mfrow=c(4,1)) # all plots on one page

for(i in c(1:3)){
  tmp = tail(head(plot_order_trend, i*30),30)
  
  #Plot the datapoints +/- std for counts
  heading = paste("combined trend for order rate day ", (i-1)*30, "-", (i*30))
  plot(tmp$day, tmp$trend, type = "o", main = heading
       , xlab="days", ylab="trend", ylim=c(-0.1, 0.1), col = "red", pch = 20)
  lines(tmp$day, y = (tmp$trend+tmp$sd))
  lines(tmp$day, y= (tmp$trend-tmp$sd))

}

#Plot overall graph at the end
heading = paste("combined trend for order rate day 0-90")
plot(plot_order_trend$day, plot_order_trend$trend, type = "o", main = heading
     , xlab="days", ylab="trend", ylim=c(-0.1, 0.1), col = "red", pch = 20)
lines(plot_order_trend$day, y = (plot_order_trend$trend+plot_order_trend$sd))
lines(plot_order_trend$day, y= (plot_order_trend$trend-plot_order_trend$sd))