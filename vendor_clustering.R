library(tidyverse)
library(lubridate)
library(cluster)

# read in dataset
data <- read.csv("transaction_data.csv")
# fix variable name
data$ObjectID <- data$ï..OBJECTID
data$ï..OBJECTID <- NULL
# convert transaction date from character to date
data$TRANSACTION_DATE <- ymd(substr(data$TRANSACTION_DATE, 1, 10))

# only want observations with above 750 purchases
top_vendor_frequencies <- sort(count(data, VENDOR_NAME)$n, decreasing = T)[1:56]
top_vendors <- filter(count(data, VENDOR_NAME), n %in% top_vendor_frequencies)$VENDOR_NAME
data_vendor_subset <- filter(data, VENDOR_NAME %in% top_vendors)

# summarises vendors
vendor_summary <- data_vendor_subset %>% 
  group_by(vendor = VENDOR_NAME) %>% 
  summarise(net2009 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2009)$TRANSACTION_AMOUNT), # net amount received in 2009
            net2010 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2010)$TRANSACTION_AMOUNT), # net amount received in 2010
            net2011 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2011)$TRANSACTION_AMOUNT), # net amount received in 2011
            net2012 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2012)$TRANSACTION_AMOUNT), # net amount received in 2012
            net2013 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2013)$TRANSACTION_AMOUNT), # net amount received in 2013
            net2014 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2014)$TRANSACTION_AMOUNT), # net amount received in 2014
            net2015 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2015)$TRANSACTION_AMOUNT), # net amount received in 2015
            net2016 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2016)$TRANSACTION_AMOUNT), # net amount received in 2016
            net2017 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2017)$TRANSACTION_AMOUNT), # net amount received in 2017
            net2018 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2018)$TRANSACTION_AMOUNT), # net amount received in 2018
            net2019 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2019)$TRANSACTION_AMOUNT), # net amount received in 2019
            net2020 = sum(filter(data_vendor_subset, vendor == VENDOR_NAME, year(TRANSACTION_DATE) == 2020)$TRANSACTION_AMOUNT), # net amount received in 2020
            agency1 = data_vendor_subset[which((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n == sort((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n, decreasing = T)[1])[1], 1], # 1st most common agency
            agency2 = data_vendor_subset[which((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n == sort((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n, decreasing = T)[2])[1], 1], # 2nd most common agency
            agency3 = data_vendor_subset[which((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n == sort((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n, decreasing = T)[3])[1], 1], # 3rd most common agency
            agency4 = data_vendor_subset[which((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n == sort((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n, decreasing = T)[4])[1], 1], # 4th most common agency
            agency5 = data_vendor_subset[which((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n == sort((filter(data_vendor_subset, vendor == VENDOR_NAME) %>% count(AGENCY))$n, decreasing = T)[5])[1], 1]) # 5th most common agency

# change the following variables from character to vector
vendor_summary$agency1 <- as.factor(vendor_summary$agency1)
vendor_summary$agency2 <- as.factor(vendor_summary$agency2)
vendor_summary$agency3 <- as.factor(vendor_summary$agency3)
vendor_summary$agency4 <- as.factor(vendor_summary$agency4)
vendor_summary$agency5 <- as.factor(vendor_summary$agency5)

# scales the numeric variables in agency_summary
vendor_summary[, 2:13] <- scale(vendor_summary[, 2:13])

# computes dissimilarity matrix
gower.dist <- daisy(vendor_summary[, 2:18], metric = c("gower"))

# run the clustering algorithm with complete linkage
hc.complete <- hclust(gower.dist, method = "complete")

# plot dendrogram
plot(hc.complete, main = "Complete Linkage w/ Scaled Features", xlab="", sub ="", labels = vendor_summary$vendor, cex = 0.9)

# cuts the dendrogram to decide how many clusters we will have
#vendor_summary$cluster <- cutree(hc.complete, 15)
