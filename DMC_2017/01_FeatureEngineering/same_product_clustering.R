# Clear environment variables
rm(list=ls())
library(sqldf)

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed

# Load the data
items_CleanRaw = read.csv("01_Data/items_Clean.csv", sep=",")
train_CleanRaw = read.csv("01_Data/train_Clean.csv", sep=",")
class_CleanRaw = read.csv("01_Data/class_Clean.csv", sep=",")

####### Set point
items_Clean = items_CleanRaw
train_Clean = train_CleanRaw
class_Clean = class_CleanRaw

# Create product cluster
items_Clean$productCluster = paste(items_Clean$manufacturer_Clean, items_Clean$salesIndex_Clean, items_Clean$category_Clean, items_Clean$pharmForm_Clean, items_Clean$group, sep = ", ")
items_Clean$productCluster = factor(items_Clean$productCluster)
str(items_Clean$productCluster)

# Merge data sets 
train_Clean = merge (x = train_Clean, y = items_Clean, by = "pid_Clean", all.x = TRUE)
class_Clean = merge (x = class_Clean, y = items_Clean, by = "pid_Clean", all.x = TRUE)


#Generate counts of manufacturer of all data
class_Clean = class_Clean[with(class_Clean, order(lineID_Clean)), ]
train_Clean = train_Clean[with(train_Clean, order(lineID_Clean)), ]


class_Clean["lineID_Clean"] = c((max(train_Clean$lineID_Clean)+1):(max(train_Clean$lineID_Clean)+length(class_Clean[[1]])))
all_Clean = rbind(subset(train_Clean, select = colnames(class_Clean)), class_Clean)


# Price per content
train_Clean$pricePerContent = train_Clean$price_Clean / train_Clean$content_Clean

all_Clean$productClusterWithDay = paste(all_Clean$productCluster, train_Clean$day, sep = ", ")
all_Clean$productClusterWithDay = as.factor(all_Clean$productClusterWithDay)
str(all_Clean$productClusterWithDay)



price_excess = sqldf("select all_Clean.lineID_Clean, ((all_Clean.price_Clean-t1.minPrice_comp)/t1.minPrice_comp) as price_excess_comp_sp
                     from all_Clean join 
                     (select productClusterWithDay, min(price_Clean, competitorPrice_Clean) as minPrice_comp from all_Clean group by productClusterWithDay) as t1
                     on all_Clean.productClusterWithDay = t1.productClusterWithDay")

all_Clean = merge(all_Clean, price_excess, by = "lineID_Clean")

all_Clean = all_Clean[with(all_Clean, order(lineID_Clean)), ]

all_Clean_export = subset(all_Clean, select = c("lineID_Clean", "price_excess_comp_sp"))

write.csv(subset(round(head(all_Clean_export, length(train_Clean[[1]])), digits = 5)), "train_cluster_single_prod.csv")
write.csv(subset(round(tail(all_Clean_export, length(class_Clean[[1]])), digits = 5)), "class_cluster_single_prod.csv")