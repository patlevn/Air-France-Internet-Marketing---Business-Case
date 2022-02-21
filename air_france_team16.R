###############################################################################
# Created by Team 16
# On 2/13/2022
# A1: Air France Business Case
###############################################################################


##################
# Import the Data Set
library(readxl)
air_france <- read_excel("Desktop/R/Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick")
View(air_france)


##################
# Rename All Variables
names(air_france) <- c("publisher_id", "publisher_name", "keyword_id","keyword",
                       "match_type","campaign","keyword_group","category",
                       "bid_strategy","keyword_type","status","search_engine_bid",
                       "clicks","click_charges","avg_cost_click","impressions",
                       "engine_click_thru","avg_pos","trans_conv","total_cost_trans",
                       "amount","total_cost","total_vol_booking")


##################
# Missing Value
air_france[is.na(air_france)] <- "Unassigned"
# check if have NAs
any(is.na(air_france))


##################
# Split 'Publisher Name'
# install.packages('stringr')
library(stringr)
my_split <- str_split_fixed(air_france$publisher_name, "-", 2)
air_france$publisher <- my_split[,1]
air_france$region <- my_split[,2]
# checking two new cols
table(air_france$publisher)
table(air_france$region)


##################
# Converting Variables into Numeric
# Publisher
air_france$publisher_num <- gsub("Google", "1", air_france$publisher)
air_france$publisher_num <- gsub("MSN", "2", air_france$publisher_num)
air_france$publisher_num <- gsub("Overture", "3", air_france$publisher_num)
air_france$publisher_num <- gsub("Yahoo", "4", air_france$publisher_num)
air_france$publisher_num <- as.numeric(air_france$publisher_num)
# Region
air_france$region_num <- gsub("US", "1", air_france$region)
air_france$region_num <- gsub("Global", "2", air_france$region_num)
air_france$region_num <- as.numeric(air_france$region_num)
# Match Type
air_france$match_typ_num <- gsub("Advanced", "1", air_france$match_type)
air_france$match_typ_num <- gsub("Broad", "2", air_france$match_typ_num)
air_france$match_typ_num <- gsub("Standard", "3", air_france$match_typ_num)
air_france$match_typ_num <- gsub("Exact", "4", air_france$match_typ_num)
air_france$match_typ_num <- gsub("N/A", "4", air_france$match_typ_num)
air_france$match_typ_num <- as.numeric(air_france$match_typ_num)
# Campaign
# 1:Air France, 2:Geo Targeted, 3:Unassigned, 4:Other
air_france$campaign_num <- gsub(".*Air France.*","1",air_france$campaign) 
air_france$campaign_num <- gsub(".*Geo Targeted.*","2",air_france$campaign_num)
air_france$campaign_num <- gsub("Unassigned","3",air_france$campaign_num)
air_france$campaign_num <- as.numeric(air_france$campaign_num)
air_france[is.na(air_france)] <- 4 # assign others to 4
# Keyword Group
# sort the group by order to check
# key_group <- table(air_france$keyword_group)
# sort(key_group,decreasing = TRUE, na.last = NA)
air_france$keyword_group_num <- gsub(".*Air France.*", "1", air_france$keyword_group)
air_france$keyword_group_num <- gsub(".*Airfrance Deal.*", "1", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub(".*Sale.*", "2", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub("Paris", "3", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub("France", "4", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub(".*Google.*", "5", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub(".*International.*", "6", air_france$keyword_group_num)
air_france$keyword_group_num <- gsub("Unassigned", "7", air_france$keyword_group_num)
air_france$keyword_group_num <- as.numeric(air_france$keyword_group_num)
air_france[is.na(air_france)] <- 8
# Status
air_france$status_num <- gsub("Live", "1", air_france$status)
air_france$status_num <- gsub("Paused", "2", air_france$status_num)
air_france$status_num <- gsub("Sent", "3", air_france$status_num)
air_france$status_num <- gsub("Deactivated", "4", air_france$status_num)
air_france$status_num <- gsub("Unavailable", "4", air_france$status_num)
air_france$status_num <- as.numeric(air_france$status_num)
# Bid Strategy
# 1:Pos 1-2 target, 2:Pos 1-6, 3:Pos 1-4 strategy, 4:Pos 2-5 strategy
# 5:Pos 5-10 strategy, 6:Unassigned
air_france$bid_str_num <- gsub(".*Target.*", "1", air_france$bid_strategy)
air_france$bid_str_num <- gsub("Position 1- 3", "2", air_france$bid_str_num)
air_france$bid_str_num <- gsub("Pos 3-6", "2", air_france$bid_str_num)
air_france$bid_str_num <- gsub(".*1-4.*", "3", air_france$bid_str_num)
air_france$bid_str_num <- gsub("Position 2-5 Bid Strategy", "4", air_france$bid_str_num)
air_france$bid_str_num <- gsub("Position 5-10 Bid Strategy", "5", air_france$bid_str_num)
air_france$bid_str_num <- gsub("Unassigned", "6", air_france$bid_str_num)
air_france$bid_str_num <- as.numeric(air_france$bid_str_num)


##################
# If Booked (as business success)
air_france$if_booked <- c()
for (i in 1:nrow(air_france)) {
  if (air_france$total_vol_booking[i] > 0){
    air_france$if_booked[i] <- 1
  } else {
    air_france$if_booked[i] <- 0
  }
} #closing the i-loop
table(air_france$if_booked)


##################
# Creating the UDF for normalize function
normalize <- function(var){
  my_norm <- (var-min(var))/(max(var)-min(var))
  return(my_norm)
} # closing the normalize UDF

# apply the normalize UDF
air_france$search_engine_bid_norm <- normalize(var=air_france$search_engine_bid)
air_france$clicks_norm <- normalize(var=air_france$clicks)
air_france$click_char_norm <- normalize(var=air_france$click_charges)
air_france$avg_cost_click_norm <- normalize(var=air_france$avg_cost_click)
air_france$impression_norm <- normalize(var=air_france$impressions)
air_france$engine_click_thru_norm <- normalize(var=air_france$engine_click_thru)
air_france$avg_pos_norm <- normalize(var=air_france$avg_pos)
air_france$total_cost_trans_norm <- normalize(var=air_france$total_cost_trans)
air_france$amount_norm <- normalize(var=air_france$amount)
air_france$total_cost_norm <- normalize(var=air_france$total_cost)
air_france$total_vol_book_norm <- normalize(var=air_france$total_vol_booking)

##################
# Creating UDF for Calculation：
# Net Revenue
net_rev_fun <- function(x,y,z){
  revenue <- sum(y)
  cost <- sum(z)
  net_rev <- revenue - cost
  return(net_rev)
}
# ROA
roa_fun <- function(x,y,z){
  revenue <- sum(y)
  cost <- sum(z)
  roa <- (revenue-cost)/cost
  return(roa)
}
# Avg. ticket
avg_tic_fun <- function(x,y,z){
  revenue <- sum(y)
  booking <- sum(z) 
  avg_tic <- revenue/booking
  return(avg_tic)
}


##################
# Creating New Data Frames
# booked df (succeeded cases)
booked_df <- air_france[which(air_france$if_booked==1), ]
# us df
us_df <- air_france[which(air_france$region_num==1), ]
# google df
google_df <- air_france[which(air_france$publisher_num==1), ]
google_us_idx <- which(us_df$publisher_num == 1)
google_us_df <- us_df[which(us_df$publisher_num == 1), ]
# MSN df
msn_df <- air_france[which(air_france$publisher_num==2), ]
msn_us_df <- us_df[which(us_df$publisher_num == 2), ]
msn_us_idx <- which(us_df$publisher_num == 2)
# overture
overture_df <- air_france[which(air_france$publisher_num==3), ]
overture_us_df <- us_df[which(us_df$publisher_num == 3), ]
overture_us_idx <- which(us_df$publisher_num == 3)
# yahoo df
yahoo_df <- air_france[which(air_france$publisher_num==4), ]
yahoo_us_df <- us_df[which(us_df$publisher_num == 4), ]
yahoo_us_idx <- which(us_df$publisher_num == 4)


##################
# Calculation
# Total Rev by Publisher
total_rev_google <- sum(us_df$amount[google_us_idx], na.rm = TRUE)
total_rev_msn <- sum(us_df$amount[msn_us_idx], na.rm = TRUE)
total_rev_ot <- sum(us_df$amount[overture_us_idx], na.rm = TRUE)
total_rev_yahoo <- sum(us_df$amount[yahoo_us_idx], na.rm = TRUE)

# Net Rev calculation
tot_net_rev <- net_rev_fun(air_france, air_france$amount, air_france$total_cost)
gg_tot_net_rev <- net_rev_fun(google_df, google_df$amount, google_df$total_cost)
gg_us_net_rev <- net_rev_fun(google_us_df, google_us_df$amount, google_us_df$total_cost)
msn_tot_net_rev <- net_rev_fun(msn_df, msn_df$amount, msn_df$total_cost)
msn_us_net_rev <- net_rev_fun(msn_us_df, msn_us_df$amount, msn_us_df$total_cost)
ot_tot_net_rev <- net_rev_fun(overture_df, overture_df$amount, overture_df$total_cost) 
ot_us_net_rev <- net_rev_fun(overture_us_df, overture_us_df$amount, overture_us_df$total_cost)
yh_tot_net_rev <- net_rev_fun(yahoo_df, yahoo_df$amount, yahoo_df$total_cost)
yh_us_net_rev <- net_rev_fun(yahoo_us_df, yahoo_us_df$amount, yahoo_us_df$total_cost)
# ROA calculation
tot_roa <- roa_fun(air_france, air_france$amount, air_france$total_cost)
gg_tot_roa <- roa_fun(google_df, google_df$amount, google_df$total_cost) #google total
gg_us_roa <- roa_fun(google_us_df, google_us_df$amount, google_us_df$total_cost) #google us
msn_tot_roa <- roa_fun(msn_df, msn_df$amount, msn_df$total_cost) #msn total
msn_us_roa <- roa_fun(msn_us_df, msn_us_df$amount, msn_us_df$total_cost) #msn us
ot_tot_roa <- roa_fun(overture_df, overture_df$amount, overture_df$total_cost) #overturn total
ot_us_roa <- roa_fun(overture_us_df, overture_us_df$amount, overture_us_df$total_cost) #overturn us
yh_tot_roa <- roa_fun(yahoo_df, yahoo_df$amount, yahoo_df$total_cost) #yahoo total
yh_us_roa <- roa_fun(yahoo_us_df, yahoo_us_df$amount, yahoo_us_df$total_cost) #yahoo us
# Avg. ticket calculation
tot_avg_tic <- avg_tic_fun(air_france, air_france$amount, air_france$total_vol_booking)
gg_tot_avg_tic <- avg_tic_fun(google_df, google_df$amount, google_df$total_vol_booking) #google total
gg_us_avg_tic <- avg_tic_fun(google_us_df, google_us_df$amount, google_us_df$total_vol_booking) #google us
msn_tot_avg_tic <- avg_tic_fun(msn_df, msn_df$amount, msn_df$total_vol_booking) #msn total
msn_us_avg_tic <- avg_tic_fun(msn_us_df, msn_us_df$amount, msn_us_df$total_vol_booking) #msn us
ot_tot_avg_tic <- avg_tic_fun(overture_df, overture_df$amount, overture_df$total_vol_booking) #overturn total
ot_us_avg_tic <- avg_tic_fun(overture_us_df, overture_us_df$amount, overture_us_df$total_vol_booking) #overturn us
yh_tot_avg_tic <- avg_tic_fun(yahoo_df, yahoo_df$amount, yahoo_df$total_vol_booking) #yahoo total
yh_us_avg_tic <- avg_tic_fun(yahoo_us_df, yahoo_us_df$amount, yahoo_us_df$total_vol_booking) #yahoo us

# average booked by publishers
tapply(air_france$total_vol_booking, air_france[,c('publisher', 'region')], mean)
tapply(booked_df$total_vol_booking, booked_df[,c('publisher', 'region')], mean)
tapply(air_france$total_vol_booking, air_france$publisher, sum)
# average cost by publishers
tapply(air_france$total_cost, air_france[,c('publisher', 'region')], mean)
tapply(booked_df$total_cost, booked_df[,c('publisher', 'region')], mean)
tapply(us_df$total_cost, us_df$publisher, mean)
# average amount by publishers
tapply(air_france$amount, air_france[,c('publisher', 'region')], sum)
tapply(booked_df$amount, booked_df[,c('publisher', 'region')], mean)
tapply(us_df$amount, us_df$publisher, mean)
# Performance by match type
tapply(us_df$total_vol_booking, us_df$match_type, sum)
tapply(us_df$amount, us_df$match_type, sum)
tapply(us_df$total_cost, us_df$match_type, sum)
tapply(us_df$clicks, us_df$match_type, sum)
# conversion rate by publisher
tapply(air_france$trans_conv, air_france$publisher, mean)

##################
# Creating plots
library(ggplot2)
ggplot(data=booked_df, aes(x=avg_cost_click, y=total_vol_booking, color=publisher))+
  geom_jitter()
ggplot(data=us_df, aes(x=amount, y=total_vol_booking, color=publisher))+
  geom_jitter()
## 
ggplot(data=us_df, aes(x=match_type, y=total_vol_booking, color=publisher))+
  geom_jitter()

##
ggplot(data = booked_df, aes(x=total_vol_booking, y=avg_cost_click, color=match_type))+
  geom_point() +
  coord_flip()
## 
ggplot(data = booked_df) + 
  geom_bar(mapping = aes(x = keyword_group_num, fill = region))

##################
# Regression
# split into training and testing
training_idx <- sample(1:nrow(air_france), size = 0.8*nrow(air_france))
air_france_train <- air_france[training_idx,]
air_france_test <- air_france[-training_idx,]

# build the logistic regression
my_logit_3 <- glm(formula = if_booked ~ keyword_group_num + campaign_num + 
                    bid_str_num + publisher_num + status_num + match_typ_num, 
                  family = "binomial", data = air_france_train)
summary(my_logit_3)

# deleted ones that are not significant
my_logit_1 <- glm(if_booked ~ region_num + keyword_group_num + bid_str_num +
                  status_num, 
                  data = air_france_train, family  = "binomial")
summary(my_logit_1)

exp(0.86509)-1 # = 1.37522
exp(-0.32977)-1 # = -0.2809109
exp(-0.40825)-1 # = -0.3351873

my_logit_2 <- glm(if_booked ~ clicks_norm + click_char_norm + avg_cost_click_norm +
                  engine_click_thru_norm + avg_pos_norm + total_cost_trans_norm +
                  amount_norm,
                  data = air_france_train, family  = "binomial")
summary(my_logit_2)

# creating confusion matrix for testing
library(caret)
my_pred <- predict(my_logit_1, air_france_test, type = "response")
confusionMatrix(data=as.factor(as.numeric(my_pred > 0.5)), 
                reference = as.factor(as.numeric(air_france_test$if_booked)))

# creating AUC and ROC
library(ROCR)
pred_val_logit_1 <- prediction(my_pred, air_france_test$if_booked)
perf_logit_1 <- performance(pred_val_logit_1, "tpr", "fpr")
plot(perf_logit_1)

# build the Gini Trees
library(rpart)
library(rpart.plot)
my_tree <- rpart(if_booked ~ region_num + keyword_group_num + bid_str_num +
                status_num,
                 data = air_france_train, method = "class", cp = 0.01)
rpart.plot(my_tree, type=1, extra=1)

# creating AUC and ROC for both model
my_tree_pred <- predict(my_tree, air_france_test, type = "prob")
my_tree_prediction <- prediction(my_tree_pred[,2],
                                 air_france_test$if_booked)
my_tree_perf <- performance(my_tree_prediction, "tpr", "fpr")

plot(perf_logit_1, col="blue")
plot(my_tree_perf, col="green4", add=TRUE)


