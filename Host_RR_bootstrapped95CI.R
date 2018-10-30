#Assessing Host-Selection Phenotypes in Culex pipiens and Culex molestus
#Day 1 Response Rates with Bootstrapped 95% CIs (n=5000)
#to chick and human hosts during two-choice landing assay  
#Anna Noreuil
#July 26, 2018

#Functions for data analysis
#Bootstrapping function --------------------------------------------------
boot.fn <- function(x, N=5000) {
  Int.1 <- replicate(N, mean(sample(x, size= length(x), replace=T)))
  Int.CI <- quantile(Int.1, probs=c(0.025,0.975))
  Int.CI
}

#Loading libraries
library(ggplot2)

#Loading datasets

#full_ba_set <- read.csv("~/Desktop/R/hostchoice7data2018.csv")#Anna uses this
full_ba_set <- read.csv("~/Desktop/Anna_Culex_BA/data/hostchoice7data2018.csv")#Megan uses this

head(full_ba_set)
tail(full_ba_set)

#subset - only data from responding females - taking out NA values, 
#manually changed data set - to get only responders
resp_only_full_ba_set <- subset(full_ba_set, host != "nr")

head(resp_only_full_ba_set)
tail(resp_only_full_ba_set)

#####To get host-specific response rates.
#recoding the host choices to make binary variable
resp_only_full_ba_set$host_hum <- ifelse(resp_only_full_ba_set$host == "c", 0, 1)
resp_only_full_ba_set$host_chick <- ifelse(resp_only_full_ba_set$host == "c", 1, 0)

#subset - Day 1 response-only data 
day1_data_resp_only <- subset(resp_only_full_ba_set, time==1)
str(day1_data_resp_only)

head(day1_data_resp_only)
tail(day1_data_resp_only)

#getting observed human response rate
obs_t1_human <- tapply(day1_data_resp_only$host_hum, day1_data_resp_only$strain, mean)
obs_t1_human

#getting the 95% CIs surrounding observed prob of human seeking
Bootstrap_CI_day1_hum <- with(day1_data_resp_only, tapply(host_hum,strain, boot.fn))

#just pulling upper for plot
lower_upper_CI_hum <- unlist(Bootstrap_CI_day1_hum)
upper_CI_hum <-lower_upper_CI_hum[c(FALSE, TRUE)]
upper_CI_hum

#getting observed chick response rate
obs_t1_chick <- tapply(day1_data_resp_only$host_chick, day1_data_resp_only$strain, mean)
obs_t1_chick

#getting the 95% CIs surrounding observed prob of chick seeking
Bootstrap_CI_day1_chick <- with(day1_data_resp_only, tapply(host_chick,strain, boot.fn))

#just pulling upper for plot
lower_upper_CI_chick <- unlist(Bootstrap_CI_day1_chick)
upper_CI_chick <-lower_upper_CI_chick[c(FALSE, TRUE)]
upper_CI_chick


#setting up data for plot
strain_names <- c("cal1", "cal1_2w", "cal2", "chi", "eva", "lau", "mol_ca", "new", "nor")
strain_order <- c(1,2,3,6,7,8,4,5,9)

plotting_dataset <- cbind(strain_names,strain_order,obs_t1_chick,obs_t1_human, upper_CI_chick, upper_CI_hum) 

write.csv(plotting_dataset, file = "~/Desktop/Anna_Culex_BA/data/forplotting.csv")

