library(sqldf)

trend_order = read.csv("02_NewFeatures/order_trend_train.csv")
trend_train = read.csv("02_NewFeatures/train_ABC_Trend_AVGActions.csv")

trend = merge(trend_order, trend_train, by = "lineID")
trend_perc = unique(subset(trend, select = c(trend_order_perc, trend_price_perc, trend_count_perc)))

par(mfrow=c(1,1))
#Plot the datapoints +/- std for counts
heading = paste("Trend plotting")
plot(main = heading, xlab="ordered", ylab="trend", ylim=c(-0.1, 0.1), col = "red", pch = 10)
lines(c(1:length(trend_perc[[1]])), sort(trend_perc$trend_order_perc), col = "red", pch = 10)
lines(c(1:length(trend_perc[[1]])), y = sort(trend_perc$trend_price_perc), col = "green", pch = 10)
lines(c(1:length(trend_perc[[1]])), y= sort(trend_perc$trend_count_perc), col = "blue", pch = 10)
