library(sqldf)

setwd("C:/Users/manuf/OneDrive/Project Datascience/R/DMC_2017_task")
train = read.csv("train.csv", sep = "|", header = TRUE, dec = ".")
class = read.csv("class.csv", sep = "|", header = TRUE, dec = ".")

factors = c("lineID","pid","adFlag","availability","click","")

summary(train)
summary(class)

#Histogram of missing vs not missing

#Missing values in Job, EducaTION; Outcome
missing = train[is.na(train["competitorPrice"]),]
not_missing = train[!is.na(train["competitorPrice"]),]

hist(missing$revenue, breaks = 1000, freq = FALSE, xlim = c(0,40))
hist(not_missing$revenue, breaks = 1000, freq = FALSE, xlim = c(0,40))

#testing for statistical significance
t.test(missing$revenue, not_missing$revenue)

summary(missing$day)
summary(not_missing$day)

items_missing = "select distinct pid from missing"
items_nmissing = "select distinct pid from not_missing"

#get items that occur in missing
items_missing = "select distinct pid from missing"
items_nmissing = "select distinct pid from not_missing"

#get items that occur in missing
missing_total =  nrow(sqldf("select distinct pid from missing"))

#Get the items that occur both in missing and not missing
missing_both = nrow(sqldf(paste("select t1.pid from (", items_missing, ") as t1 JOIN (", items_nmissing,") as t2 on t1.pid = t2.pid")))

missing_only = missing_total - missing_both

#Summarized output.
data.frame(NR_ITEMS = c(missing_total, missing_both, missing_only), row.names = c("MISSING TOTAL", "MISSING BOTH", "MISSING ONLY"))

summary(missing$price)
summary(not_missing$price)

rm(missing, not_missing)

#get number of products
sqldf("select count(t1.pid) from (select distinct pid from train) as t1")

#General sql statement for data generation
library(sqldf)
abc = sqldf("select *
            from (select pid, count(pid), sum(revenue) as sumrevenue, avg(revenue) from train group by pid) as t1
            order by sumrevenue desc")

#ABC analysis revenue
#generate cummulated sum
cs_rev = cumsum(abc$sumrevenue)

#normalize the y axis
norm_cs_rev = cs_rev/sum(abc$sumrevenue)

#Plot the
plot(c(1:nrow(abc))/nrow(abc), norm_cs_rev, xlim = c(0, 1), ylim = c(0, 1), xlab = "% of pIDs", ylab = "% of overall revenue")

#ABC analysis number of sales