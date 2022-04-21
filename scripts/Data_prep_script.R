#install.packages("dplyr")
library(dplyr)

#install.packages("reshape")
library(reshape)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("magrittr")
library(magrittr)

library(plyr)
#setting working directory
##Ayush -  change your working directory to wherever the data is and replace \ with \\ using find and replace
print(getwd())
setwd()
print(getwd())


#######################################################################
## Confirmed and deaths data 

#reading daily 
cases <- read.csv("state_wise_daily.csv")
#colnames(cases)
#head(cases)

#adding DN + DD
cases$DDN <- cases$DN + cases$DD

#removing extraneous column
cases <- cases %>% select(-Date,-TT,-UN,-DD,-DN)
colnames(cases)

unique(cases[c("Status")])
cases <- cases %>% filter(Status!="Recovered")
unique(cases[c("Status")])

#checking for missing values
sapply(cases, function(x) sum(is.na (x)))

#converting to date format
head(cases["Date_YMD"])
cases$Date_YMD %<>% parse_datetime("%Y-%m-%d")
head(cases["Date_YMD"])
class(cases$Date_YMD)
summary(cases) #14/3 to 6/2


positives <- cases %>% filter(Status=="Confirmed")
deaths <- cases %>% filter(Status=="Deceased")

#######################################################################
## Converting confirmed cases to proper format

#converting to long format
positives_long <- melt(positives, id=c("Date_YMD","Status"))

#converting negative values to positive
positives_long["value"] <- abs(positives_long["value"])
#positives_long %>% rename("State"=variable,"Confirmed"=value)
colnames(positives_long)[grep("variable", colnames(positives_long))] <-"State"
colnames(positives_long)[grep("value", colnames(positives_long))] <-"Confirmed"
colnames(positives_long)

#removing column Status
positives_long <- positives_long %>% select(-Status)


#######################################################################
##  Converting deaths to proper format

#converting to long format
deaths_long <- melt(deaths, id=c("Date_YMD","Status"))

#converting negative values to positive
deaths_long["value"] <- abs(deaths_long["value"])
#deaths_long %>% rename("State"=variable,"Confirmed"=value)
colnames(deaths_long)[grep("variable", colnames(deaths_long))] <-"State"
colnames(deaths_long)[grep("value", colnames(deaths_long))] <-"Deaths"
colnames(deaths_long)

#removing column Status
deaths_long <- deaths_long %>% select(-Status)

nrow(deaths_long)
nrow(positives_long)

#checking for missing dates
md_chk <- positives_long %>% group_by(Date_YMD) %>% summarize(n=n())
unique(md_chk$n) #36 means that all dates have data for 36 states

md_chk <- deaths_long %>% group_by(Date_YMD) %>% summarize(n=n())
unique(md_chk$n) #36 means that all dates have data for 36 states

summary(positives_long)  
#check if 11880 = number of days between 14 march and 6 feb - manual checking
difftime(as.Date("2021-02-06"),as.Date("2020-03-14"),units="days") +1 
330*36

final_df <- inner_join(positives_long,deaths_long,by=c("Date_YMD","State"))
nrow(final_df) # should be 11880
unique(final_df$State)
sapply(final_df,class)

#loading state codes
state_codes <- read.csv("State_codes.csv")
head(state_codes)
colnames(state_codes)
colnames(state_codes)[grep("ï..State_name", colnames(state_codes))] <-"State_name"

final_df_v2 <- inner_join(final_df,state_codes,by=c("State"))
nrow(final_df_v2)
head(final_df_v2)

#######################################################################
## Test data
tested <- read_csv("statewise_tested_numbers_data.csv")
colnames(tested)
tested <- tested %>% select("Updated On", "State","Total Tested" )
colnames(tested)
sapply(tested,class)
nrow(tested)

#checking for missing values
sapply(tested, function(x) sum(is.na (x))) # 70 mv in total tested


#renaming
colnames(tested)[grep("Updated On", colnames(tested))] <-"Date_YMD"
colnames(tested)[grep("Total Tested", colnames(tested))] <-"Tested"
colnames(tested)[grep("State", colnames(tested))] <-"State_name"

#converting to date format
head(tested["Date_YMD"])
tested$Date_YMD <- as.Date(tested$Date_YMD,"%d/%m/%Y")
head(tested["Date_YMD"])
class(tested$Date_YMD)
summary(tested) #1/4 to 8/2

#visually checking if number of states are 36 and match with cases data
unique(tested$State_name)
unique(final_df_v2$State_name)

#checking for missing dates
difftime(max(tested$Date_YMD),min(tested$Date_YMD),units="days") +1 
314*36
nrow(tested)
md_chk <- tested %>% group_by(Date_YMD) %>% summarize(n=n())
unique(md_chk$n) 

md_chk %>% group_by(n) %>% count() 

date_points_statewise <- tested %>% group_by(State_name) %>% count()
write_csv(date_points_statewise,"date_points_statewise.csv")


#creating a vector of all dates
all_dates <- data.frame(Date_YMD=seq(min(tested$Date_YMD),max(tested$Date_YMD), by="days"))
sapply(all_dates,class)
nrow(all_dates)

#cross product of all dates with all states
all_states <- data.frame(State_name=unique(tested$State_name))
all_state_dates <- merge(all_dates,all_states,all=TRUE)

nrow(all_state_dates) #should be 314*36=11304

tested_all_dates <- join(all_state_dates,tested,by=c("Date_YMD","State_name"))
nrow(tested_all_dates)
#filling zero for missing values for now - to be changed to 3 day averages later
tested_all_dates[is.na(tested_all_dates)] <- 0

md_chk <- tested_all_dates %>% group_by(State_name) %>% count("Date_YMD")
md_chk

#one duplicate is created for UP on 5 -Apr, don't know why, so deduplicating
tested_all_dates <- unique(tested_all_dates)
nrow(tested_all_dates)

nrow(tested_all_dates)

nrow(final_df_v2)

#merging test data with confirmed and deceased cases
final_df_v3 <- join(final_df_v2,tested_all_dates,by=c("Date_YMD","State_name"),type="inner")
head(final_df_v3)

final_df_v3 <- final_df_v3 %>% select(-State)
dt_chk <- final_df_v3 %>% group_by(Date_YMD) %>% count("Date_YMD")
unique(dt_chk$freq) 
difftime(max(final_df_v3$Date_YMD),min(final_df_v3$Date_YMD),units="days") +1 
312*36 
nrow(final_df_v3) #matches with above line product = 11232

###########################################################################
##  Loading population data

pop <- read.csv("population.csv")
colnames(pop)[grep("ï..State", colnames(pop))] <-"State_name"
head(pop)

final_df_v4 <- join(final_df_v3,pop,type="inner",by=c("State_name"))
nrow(final_df_v4)

###########################################################################
##   Making parameters

head(final_df_v4)
# final_df_v4$Confirmed_per_capita<- round(final_df_v4$Confirmed/1000000,2)
# final_df_v4$Deaths_per_ca
pita<- round(final_df_v4$Deaths/1000000,2)
# 
# final_df_v4$Confirmed_per_tested<- ifelse(final_df_v4$Confirmed>final_df_v4$Tested, 1 ,round(final_df_v4$Confirmed/(final_df_v4$Tested+0.000001) ,2))
# #added a very small number above to denominator to avoid division by zero
# final_df_v4$Tested_per_capita<- round(final_df_v4$Tested/1000,2)


#not rounding the figures to 2 decimal points because it will help in ranking
final_df_v4$Confirmed_per_capita<- final_df_v4$Confirmed/final_df_v4$Population
final_df_v4$Deaths_per_capita<- final_df_v4$Deaths/final_df_v4$Population

final_df_v4$Confirmed_per_tested<- ifelse(final_df_v4$Confirmed>final_df_v4$Tested, 1 ,final_df_v4$Confirmed/(final_df_v4$Tested+0.000001))
#added a very small number above to denominator to avoid division by zero
final_df_v4$Tested_per_capita<- final_df_v4$Tested/final_df_v4$Population


avg=aggregate(final_df_v4[, c("Confirmed", "Deaths" ,"Population" , "Confirmed_per_capita"
                              ,"Deaths_per_capita" ,"Confirmed_per_tested" , 
                              "Tested_per_capita")], list(final_df_v4$Date_YMD), mean)


colnames(avg)[grep("Group.1", colnames(avg))] <-"Date_YMD"
avg$State_name="India"


head(final_df_v4)
column_order <- c("Date_YMD","State_name", "Confirmed", "Deaths"  
                  ,"Population"       ,     "Confirmed_per_capita"
                  ,"Deaths_per_capita" ,"Confirmed_per_tested" , "Tested_per_capita"  )
final_df_v4 <- final_df_v4[,column_order]

write.csv(final_df_v4,"input_data_with_pop.csv",row.names = FALSE)

#saving without population
column_order <- c("Date_YMD","State_name", "Confirmed", "Deaths"  
                   ,     "Confirmed_per_capita"
                  ,"Deaths_per_capita" ,"Confirmed_per_tested" , "Tested_per_capita"  )
final_df_v5 <- final_df_v4[,column_order]
avg <- avg[,column_order]


final_df_v5=rbind(final_df_v5,avg)

write.csv(final_df_v5,"input_data.csv",row.names = FALSE)
#saving workspace
save.image()
