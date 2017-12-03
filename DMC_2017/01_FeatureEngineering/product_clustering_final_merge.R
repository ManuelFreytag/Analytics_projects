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
items_Clean$productCluster = paste(items_Clean$salesIndex_Clean, items_Clean$category_Clean, items_Clean$pharmForm_Clean, items_Clean$group, sep = ", ")
items_Clean$productCluster = factor(items_Clean$productCluster)
str(items_Clean$productCluster)

# Merge data sets 
train_Clean = merge (x = train_Clean, y = items_Clean, by = "pid_Clean", all.x = TRUE)
class_Clean = merge (x = class_Clean, y = items_Clean, by = "pid_Clean", all.x = TRUE)

#Add other attributes
#Generate revenue of manufacturer
revenue_man = sqldf("select manufacturer_Clean, sum(revenue_Clean) as sum_rev_man from train_Clean group by manufacturer_Clean")
revenue = sqldf("select pid_Clean, sum(revenue_Clean) as sum_rev from train_Clean group by pid_Clean")

#Generate counts of manufacturer of all data
class_Clean = class_Clean[with(class_Clean, order(lineID_Clean)), ]
train_Clean = train_Clean[with(train_Clean, order(lineID_Clean)), ]


class_Clean["lineID_Clean"] = c((max(train_Clean$lineID_Clean)+1):(max(train_Clean$lineID_Clean)+length(class_Clean[[1]])))

all_Clean = rbind(subset(train_Clean, select = colnames(class_Clean)), class_Clean)

counts = sqldf("select count(pid_Clean) as actions_pid, pid_Clean from all_Clean group by pid_Clean")
counts_man = sqldf("select count(pid_Clean) as actions_man, manufacturer_Clean from all_Clean group by manufacturer_Clean")

all_Clean = merge(all_Clean, revenue, by = "pid_Clean", all.x = TRUE)
all_Clean = merge(all_Clean, revenue_man, by = "manufacturer_Clean", all.x = TRUE)

all_Clean = merge(all_Clean, counts, by = "pid_Clean", all.x = TRUE)
all_Clean = merge(all_Clean, counts_man, by = "manufacturer_Clean", all.x = TRUE)


# Price per content
train_Clean$pricePerContent = train_Clean$price_Clean / train_Clean$content_Clean

all_Clean$productClusterWithDay = paste(all_Clean$productCluster, train_Clean$day, sep = ", ")
all_Clean$productClusterWithDay = as.factor(all_Clean$productClusterWithDay)
str(all_Clean$productClusterWithDay)



price_excess = sqldf("select all_Clean.lineID_Clean, ((all_Clean.price_Clean-t1.minPrice_comp)/(t1.minPrice_comp)) as price_excess_comp
                     from all_Clean join 
                     (select productClusterWithDay, min(price_Clean) as minPrice, min(price_Clean, competitorPrice_Clean) as minPrice_comp from all_Clean group by productClusterWithDay) as t1
                     on all_Clean.productClusterWithDay = t1.productClusterWithDay")

all_Clean = merge(all_Clean, price_excess, by = "lineID_Clean")

all_Clean = all_Clean[with(all_Clean, order(lineID_Clean)), ]
all_Clean_export = subset(all_Clean, select = c("lineID_Clean", "sum_rev", "actions_pid", "sum_rev_man", "actions_man", "price_excess_comp"))


write.csv(subset(round(head(all_Clean_export, length(train_Clean[[1]])), digits = 5)), "train_clustering_revenue_sum.csv")
write.csv(subset(round(tail(all_Clean_export, length(class_Clean[[1]])), digits = 5)), "class_clustering_revenue_sum.csv")