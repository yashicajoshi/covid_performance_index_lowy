# install.packages("dplyr")
library(dplyr)

# install.packages("reshape")
library(reshape)

# install.packages("tidyverse")
library(tidyverse)

# install.packages("magrittr")
library(magrittr)

library(plyr)

#install.packages("zoo")
library(zoo)

getwd()

index_data <- read.csv("input_data.csv")
head(index_data)
sapply(index_data,class)
nrow(index_data)

#converting date column to date format
index_data$Date_YMD %<>% parse_datetime("%Y-%m-%d")


# state_list <- unique(index_data$State_name)#[c(1,2)]
#excluding states which have far too less cases - for example Lakshadweep only has 151 cases in total
total_confirmed_cases <- aggregate(index_data$Confirmed, by=list(Category=index_data$State_name), FUN=sum)
total_confirmed_cases <- total_confirmed_cases %>% filter(x>=1000)
state_list <- unique(total_confirmed_cases$Category)#[c(1,2)]

#min date of hitting 100th case
sample_dt_all=data.frame()
for (state in state_list){
    
  sample <- index_data %>% filter(State_name==state)
  
  # state="Bihar"
  #sorting in ascending order of date
  sample <- sample %>% arrange(Date_YMD)
  
  #filtering data post 100th case
  sample$Cum_confirmed <- cumsum(sample$Confirmed)
  sample <- sample %>% filter(Cum_confirmed>=100)
  sample_min_dt=data.frame(sample %>% group_by('State_name') %>% summarise(case100_date=min(Date_YMD)))
  sample_min_dt$State_name <- state
  print(sample_min_dt)
  sample_dt_all=rbind(sample_dt_all,sample_min_dt)
}
write.csv(sample_dt_all,"case100_dt.csv",row.names=FALSE)

ravg_df=data.frame()
ravg_all_states <- data.frame()
for (state in state_list){
  
  sample <- index_data %>% filter(State_name==state)
  
  # state="Bihar"
  #sorting in ascending order of date
  sample <- sample %>% arrange(Date_YMD)
  
  #filtering data post 100th case
  sample$Cum_confirmed <- cumsum(sample$Confirmed)
  sample <- sample %>% filter(Cum_confirmed>=100)
  sample <- sample %>% select(-Cum_confirmed)
  
  #calculating moving average of each column
  numeric_cols <- unlist(lapply(sample,is.numeric))
  rolling_avg <- function(x){rollmean(x,k=14)}
  ravg_df <- data.frame(sapply(sample[,numeric_cols],rolling_avg))
  ravg_df["State_name"] = state
  ravg_df["Week_name"] <- paste("Fortnight",seq(1,nrow(ravg_df)),sep="-")
  
  #reordering columns
  column_order <- c("State_name","Week_name","Confirmed","Deaths","Confirmed_per_capita","Deaths_per_capita",
                    "Confirmed_per_tested","Tested_per_capita")
  ravg_df <- ravg_df[,column_order]
  
  #appending for each state
  ravg_all_states <- rbind(ravg_all_states,ravg_df)
}
write.csv(ravg_all_states,"chk_overlap.csv")


# #in case rolling averages have to be non overlapping, insert below code
# ravg_df=data.frame()
# ravg_all_states <- data.frame()
# for (state in state_list){
#   
#   
#   sample <- index_data %>% filter(State_name==state)
#   
#   # state="Bihar"
#   #sorting in ascending order of date
#   sample <- sample %>% arrange(Date_YMD)
#   
#   #filtering data post 100th case
#   sample$Cum_confirmed <- cumsum(sample$Confirmed)
#   sample <- sample %>% filter(Cum_confirmed>=100)
#   sample <- sample %>% select(-Cum_confirmed)
#   
#   #calculating moving average of each column
#   numeric_cols <- unlist(lapply(sample,is.numeric))
#   rolling_avg <- function(x){rollmean(x,k=14)}
#   ravg_df <- data.frame(sapply(sample[,numeric_cols],rolling_avg))
#   ravg_df["State_name"] = state
#   
#   #assigning week numbers
#   ravg_df$Week_num <- seq(1,nrow(ravg_df))
#   
#   #non overlapping windows will be week 15,29 and so on, calculating those numbers
#   no_overlap_week_id <- ifelse(ravg_df$Week_num %% 14==0, ravg_df$Week_num+1,NA)
#   #removing NA from above
#   no_overlap_wk_id <- na.exclude(no_overlap_week_id)
#   attributes(no_overlap_wk_id)$na.action <- NULL
#   
#   #selecting only those rows from the df
#   ravg_df <- ravg_df[c(1,no_overlap_wk_id),]
#   
#   #assigning week names on the basis of non overlapping windows
#   ravg_df["Week_name"] <- paste("Week",seq(1,nrow(ravg_df)),sep="-")
#   
#   #reordering columns
#   column_order <- c("State_name","Week_name","Confirmed","Deaths","Confirmed_per_capita","Deaths_per_capita",
#                     "Confirmed_per_tested","Tested_per_capita")
#   ravg_df <- ravg_df[,column_order]
#   
#   #appending for each state
#   ravg_all_states <- rbind(ravg_all_states,ravg_df)
# }
# 
# write.csv(ravg_all_states,"chk.csv")

#ranking
unique(ravg_all_states$Week_name)
colnames(ravg_all_states)

#selecting minimum number of weeks for which all states' data is available
week_list <- unique(ravg_all_states$Week_name)
statewise_week_nums <- ravg_all_states %>% group_by(Week_name) %>% count("State_name")
filtered_week_list <- paste("Fortnight",seq(1,min(statewise_week_nums$freq)),sep="-")

rank_df_all=data.frame()
for (week_nm in filtered_week_list){

  #filtering for one week
  sample_rank_df <- ravg_all_states %>% filter(Week_name==week_nm) 
  
  #defining rank function
  rank_fn <- function(x){rank(x)} #in case of tie, assigns an average (sum of ranks/number of ties -run below to get intuitive sense)
  # rank(c(1,1,1,2,2,3,4))
  
  #applying rank function to all columns
  numeric_cols <- unlist(lapply(sample_rank_df,is.numeric))
  rank_df <- data.frame(sapply(sample_rank_df[,numeric_cols],rank_fn))
  
  #assigning rank in decreasing order to tests - overwriting column made above
  rank_df["Tested_per_capita"] <- rank(-rank_df$Tested_per_capita)
  rank_df <- cbind(sample_rank_df[,c("State_name","Week_name")],rank_df)
  
  #Averaging ranks
  rank_df$Avg_rank <- rowMeans(rank_df[,numeric_cols])
  
  #normalizing
  #changed up the formula from value-min/max-min because in this formula, minimum scoring state will be best performing and lowy has defined maximum scoring to be the best
  rank_df$Normalized_score <- (rank_df$Avg_rank - max(rank_df$Avg_rank))/(min(rank_df$Avg_rank)-max(rank_df$Avg_rank))
   
  rank_df_all <- rbind(rank_df_all,rank_df)
}

write.csv(rank_df_all,"lowy_index_statewise.csv",row.names=FALSE)
