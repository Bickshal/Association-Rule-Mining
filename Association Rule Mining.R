# Associaiton Rule Mining is used for Market Based Data Analysis.

rm(list=ls())

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

setwd("/Users/Deepika/Desktop/Learning/R Bloggers Exercise")

retail <- read_excel('Online Retail.xlsx')
retail <- retail[complete.cases(retail), ]

str(retail)
dim(retail)
names(retail)
any(is.na(retail))
head(retail)

retail$Description <- as.factor(retail$Description)
retail$Country <- as.factor(retail$Country)
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Time <- hms(retail$Time)
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
str(retail)
glimpse(retail)

# to find out at what hour customers did maximum purchase
retail %>% mutate(hour = as.factor(hour(Time))) %>% ggplot(aes(x = hour)) + geom_histogram(stat = 'count',fill = "indianred")

# how many items each customer purchased

retail %>% summarize(Number_of_Invoices = n_distinct(retail$InvoiceNo)) # Number of Invoices

retail %>% select(InvoiceNo,Quantity,CustomerID) %>% group_by(InvoiceNo) %>% summarise_all(sum) %>%
             ggplot(aes(x=InvoiceNo,y= Quantity)) + geom_histogram()

retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity))


retail %>% select(InvoiceNo,Quantity,CustomerID) %>% group_by(InvoiceNo) %>% summarise_all(sum)


retail %>% ggplot(aes(x=Time)) + geom_histogram(stat = "count")


retail %>% ggplot(aes(x=Time)) + geom_histogram(aes(y=density),stat = "count",bins = 20)

#Top 10 best sellers

retail %>% summarize(Number_of_Items = n_distinct(retail$Description)) # Number of Descriptors
retail %>% summarize(Number_of_Stock= n_distinct(retail$StockCode)) # Number of Stock codes

library(dplyr)

retail %>% select(Description,StockCode,Quantity) %>% group_by(Description,StockCode) %>% dplyr::summarize(count= n()) %>%
             arrange(desc(count)) %>% head(10) %>% mutate(Description = reorder(Description,count)) %>%
             ggplot(aes(Description,count)) + geom_bar(stat='identity') +coord_flip()


# Before applying rule mining, we need to break down data frame to check what item was purchased in conjunction with what

retail_sorted <- retail[order(retail$CustomerID),]


library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), function(df1)paste(df1$Description,collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")

plot(topRules, method = "grouped")


