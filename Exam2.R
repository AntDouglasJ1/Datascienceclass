#clearing environment and setting working directory
rm(list=ls(all=TRUE))
setwd("C:/Users/antdo/Documents/R/Exam2")
#libraries
library(tidyverse)
library(rio)
library(doBy)
library(WDI)
library(labelled)
library(tinytex)
#loading inequality dataset
inequality_data <- import("inequality.xlsx")
summary(inequality_data)
#Answering question 3. Print in R Markdown before turning in
print("Answer to number 3 is this is a cross-sectional dataset. The data provides a number of variables within a specific snapshot of time.")
#subset for Denmark
subset(inequality_data$inequality_gini, inequality_data$country == "Denmark")
#Subset for Sweden
subset(inequality_data$inequality_gini, inequality_data$country == "Sweden")
#Subset for Brazil
subset(inequality_data$inequality_gini, inequality_data$country == "Brazil")
#Answering question 6, print in R markdown before turning in
print("It is better to have a lower gini score as that indicates less inequality.")
#head command
head(inequality_data)
#Change accent from Belarus w/ function
remove.accents <- function(s) {
  
  #one character substitution
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1,new1,s)
}
#remove accents
inequality_data$country <-  remove.accents(inequality_data$country)

#Checking Head
head(inequality_data)

#sorting
inequality_data <- inequality_data[order(inequality_data$inequality_gini),]
head(inequality_data)

#mean
inequality_data <- na.omit(inequality_data)
inequality_data_mean <- mean(inequality_data$inequality_gini)

#ifelse

inequality_data$low_inequality <- ifelse(inequality_data$inequality_gini <= inequality_data_mean, yes = "1", no = "0")
inequality_data$high_inequality <- ifelse(inequality_data$inequality_gini > inequality_data_mean, yes = "1", no = "0")
#crosstab
summaryBy(low_inequality ~ high_inequality, inequality_data, FUN=c(mean,length))

#for loop

names <- c("The World Bank", "The African Development Bank", "The Bill and Melinda GatesFoundation")
for (i in names){
  print(i)
}

print("I chose Poverty gap at $1.90 a day (2011 PPP) (%)), code:SI.POV.GAPS")
# Answer to question 14, put into R markdown
#import variable into R
ppp_data <- WDI(country = "all",indicator =c("SI.POV.GAPS"),
                     start = 2015, end = 2015, extra = FALSE, cache = NULL)
#merge datasets
merge_df <-  dplyr::full_join(inequality_data,ppp_data)
merge_df <-  na.omit(merge_df)
#filter
data_greater_30 <- 
  merge_df%>% 
  dplyr::filter((inequality_gini>30))
#sum of inequality_gini from data_greater_30
sumofineq <- function(x) {
 sum(data_greater_30$inequality_gini, na.rm=TRUE)
 data_greater_30<- sapply(data_greater_30, sumofineq)
data_greater_30
}

#label variables
var_label(merge_df) <- list(`country` = "country",
                               `year`= "year",
                               `inequality_gini`="Gini score",
                               `low_inequality` = "Score of 1 indicates low inequlality",
                               `high_inequality` = "Score of 1 indicates high inequlality",
                               `SI.POV.GAPS` = "Poverty gap at $1.90 a day (2011 PPP) (%)",
                               `iso2c`="ISO-3 country code")
#exporting
final_data <- export(merge_df, "finaldata.dta")

