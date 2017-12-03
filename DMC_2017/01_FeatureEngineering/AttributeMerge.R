trend = subset(read.csv("02_NewFeatures/train_ABC_Trend_AVGActions.csv"), select = c(lineID,decomp_cyclic_counts_norm,decomp_trend_counts_norm,decomp_cyclic_price_norm,decomp_trend_price_norm,mean_click,mean_basket,mean_order))
clustering_similar = subset(read.csv("train_clustering_revenue_sum.csv"), select = c(lineID_Clean,sum_rev,actions_pid,sum_rev_man,actions_man,price_excess_comp))
names(clustering_similar)[names(clustering_similar)=="lineID_Clean"] = "lineID"

order_trend = read.csv("order_trend_train.csv")
cluster_ident = subset(read.csv("train_cluster_single_prod.csv"), select = -c(X))
names(cluster_ident)[names(cluster_ident)=="lineID_Clean"] = "lineID"

trend = merge(trend, clustering_similar, by = "lineID")
trend = merge(trend, order_trend, by = "lineID")
trend = merge(trend, cluster_ident, by = "lineID")
write.csv(trend, "trainManu.csv")
summary(trend)

#CLASS JOIN
trend = subset(read.csv("02_NewFeatures/class_ABC_Trend_AVGActions.csv"), select = c(lineID,decomp_cyclic_counts_norm,decomp_trend_counts_norm,decomp_cyclic_price_norm,decomp_trend_price_norm,mean_click,mean_basket,mean_order))
clustering_similar = subset(read.csv("class_clustering_revenue_sum.csv"), select = c(lineID_Clean,sum_rev,actions_pid,sum_rev_man,actions_man,price_excess_comp))
names(clustering_similar)[names(clustering_similar)=="lineID_Clean"] = "lineID"

order_trend = read.csv("order_trend_class.csv")
order_trend["lineID"] = clustering_similar$lineID
cluster_ident = subset(read.csv("class_cluster_single_prod.csv"), select = -c(X))
names(cluster_ident)[names(cluster_ident)=="lineID_Clean"] = "lineID"

class_final = merge(trend, clustering_similar, by = "lineID")
class_final = merge(class_final, order_trend, by = "lineID")
class_final = merge(class_final, cluster_ident, by = "lineID")
write.csv(class_final, "classManu.csv")
summary(class_final)