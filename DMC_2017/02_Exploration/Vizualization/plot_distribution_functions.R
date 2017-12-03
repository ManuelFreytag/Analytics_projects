#train = read.csv("train_ABC_Trend_AVGActions.csv")



train = final_train

coln = colnames(train)
par(mfrow=c(7,2))

tmp = train[train[i] > quantile(train[[i]], c(0.05)) || train[i] > quantile(train[[i]], c(0.95))]

for(i in coln[2:length(coln)]){
  nbreaks = 20
  
  if(length(table(train[[i]])) < nbreaks){
    tmp = table(train[[i]])
    barplot(tmp,
            width = length(tmp),
            main = paste("Histogram of ", i),
            border = "blue")
  }else{
    data = train[[i]]
    
    tmp = data[(data > quantile(data, c(0.05)))]
    tmp = tmp[(tmp < quantile(data, c(0.95)))]
    
    hist(tmp, 
         main = paste("Histogram of ", i),
         xlab= i, 
         border="blue",
         las=1, 
         breaks=nbreaks, 
         prob = TRUE)
  }
}