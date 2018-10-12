#Assessing Host-Selection Phenotypes in Culex pipiens and Culex molestus
#Day 1 Response Rates with Bootstrapped 95% CIs (n=5000)
#to chick and human hosts during two-choice landing assay  
#Anna Noreuil
#July 26, 2018

#Functions for data analysis
# Bootstrapping function --------------------------------------------------
boot.fn <- function(x, N=5000) {
  Int.1 <- replicate(N, mean(sample(x, size= length(x), replace=T)))
  Int.CI <- quantile(Int.1, probs=c(0.025,0.975))
  Int.CI
}


#Loading datasets

#full_ba_set <- read.csv("~/Desktop/R/hostchoice7data2018.csv")#Anna uses this
full_ba_set <- read.csv("~/Downloads/hostchoice7data2018.csv")#Megan uses this

head(full_ba_set)
tail(full_ba_set)

#subset - only data from responding females - taking out NA values, 
#manually changed data set - to get only responders
resp_only_full_ba_set <- subset(full_ba_set, host != "nr")

head(resp_only_full_ba_set)
tail(resp_only_full_ba_set)

#recoding the host choices to make binary variable
resp_only_full_ba_set$host <- ifelse(resp_only_full_ba_set$host == "c", 0, 1)

#subset - Day 1 response-only data 
day1_data_resp_only <- subset(resp_only_full_ba_set, time==1)
str(day1_data_resp_only)

head(day1_data_resp_only)
tail(day1_data_resp_only)

#getting the 95% CIs surrounding observed prob of human seeking
Bootstrap_CI_day1_responders <- with(day1_data_resp_only, tapply(host,strain, boot.fn))
Bootstrap_CI_day1_responders


#Human Response Rate - CIs with h scored as 1; and c scored as 0 


