library(tidyverse)
library(lubridate)
library(cluster)

# read in dataset
data <- read.csv("~/Datathon 2021/transaction_data.csv")
# fix variable name
data$ObjectID <- data$ï..OBJECTID
data$ï..OBJECTID <- NULL
# convert transaction date from character to date
data$TRANSACTION_DATE <- lubridate::ymd(substr(data$TRANSACTION_DATE, 1, 10))

# summarises agencies
agency_summary <- data %>% 
                    group_by(agency = AGENCY) %>% 
                      summarise(net2009 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2009)$TRANSACTION_AMOUNT), # net amount spent in 2009
                                net2010 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2010)$TRANSACTION_AMOUNT), # net amount spent in 2010
                                net2011 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2011)$TRANSACTION_AMOUNT), # net amount spent in 2011
                                net2012 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2012)$TRANSACTION_AMOUNT), # net amount spent in 2012
                                net2013 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2013)$TRANSACTION_AMOUNT), # net amount spent in 2013
                                net2014 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2014)$TRANSACTION_AMOUNT), # net amount spent in 2014
                                net2015 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2015)$TRANSACTION_AMOUNT), # net amount spent in 2015
                                net2016 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2016)$TRANSACTION_AMOUNT), # net amount spent in 2016
                                net2017 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2017)$TRANSACTION_AMOUNT), # net amount spent in 2017
                                net2018 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2018)$TRANSACTION_AMOUNT), # net amount spent in 2018
                                net2019 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2019)$TRANSACTION_AMOUNT), # net amount spent in 2019
                                net2020 = sum(filter(data, agency == AGENCY, year(TRANSACTION_DATE) == 2020)$TRANSACTION_AMOUNT), # net amount spent in 2020
                                vendor1 = data[which((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n == sort((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n, decreasing = T)[1])[1], 4], # 1st most common vendor
                                vendor2 = data[which((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n == sort((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n, decreasing = T)[2])[1], 4], # 2nd most common vendor
                                vendor3 = data[which((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n == sort((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n, decreasing = T)[3])[1], 4], # 3rd most common vendor
                                vendor4 = data[which((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n == sort((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n, decreasing = T)[4])[1], 4], # 4th most common vendor
                                vendor5 = data[which((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n == sort((filter(data, agency == AGENCY) %>% count(VENDOR_NAME))$n, decreasing = T)[5])[1], 4], # 5th most common vendor
                                category1 = data[which((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n == sort((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n, decreasing = T)[1])[1], 6], # 1st most common purchase category
                                category2 = data[which((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n == sort((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n, decreasing = T)[2])[1], 6], # 2nd most common purchase category
                                category3 = data[which((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n == sort((filter(data, agency == AGENCY) %>% count(MCC_DESCRIPTION))$n, decreasing = T)[3])[1], 6]) # 3rd most common purchase category

# change the following variables from character to vector
agency_summary$vendor1 <- as.factor(agency_summary$vendor1)
agency_summary$vendor2 <- as.factor(agency_summary$vendor2)
agency_summary$vendor3 <- as.factor(agency_summary$vendor3)
agency_summary$vendor4 <- as.factor(agency_summary$vendor4)
agency_summary$vendor5 <- as.factor(agency_summary$vendor5)
agency_summary$category1 <- as.factor(agency_summary$category1)
agency_summary$category2 <- as.factor(agency_summary$category2)
agency_summary$category3 <- as.factor(agency_summary$category3)

# scales the numeric variables in agency_summary
agency_summary[, 2:13] <- scale(agency_summary[, 2:13])

# computes dissimilarity matrix
gower.dist <- daisy(agency_summary[, 2:21], metric = c("gower"))

# run the clustering algorithm with complete linkage
hc.complete <- hclust(gower.dist, method = "complete")

# plot dendrogram
plot(hc.complete, main = "Complete Linkage w/ Scaled Features", xlab="", sub ="", labels = agency_summary$agency, cex = 0.9)

# cuts the dendrogram to decide how many clusters we will have
#agency_summary$cluster <- cutree(hc.complete, 14)

