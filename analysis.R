##################################################################################################################################
#LIBRARY & DATASET IMPORTING
##################################################################################################################################
library(ggplot2)
library(tidyverse)
library(dplyr)

#import the csv file
runkeeper <- read.csv("cardioActivities.csv", sep=",", na.strings="NA")

#first inspection after importing the data
as_tibble(runkeeper)

##################################################################################################################################
#PREPROCESSING DATA
##################################################################################################################################

#There are some column names that have the unit measures inside the name. Rather, it is better 
#Deleting units of measure and adding them  to the column description using comment() 

#Renaming -> [Distance / Average speed / Climb / Average heart rate]
runkeeper <- runkeeper %>% 
  rename("Distance"="Distance..km.", "Average.Speed"="Average.Speed..km.h.", "Climb"="Climb..m.", "Average.Heart.Rate"="Average.Heart.Rate..bpm.")

#Adding a comment to each column just renamed
comment(runkeeper$Distance) <- c("Expressed in km")
comment(runkeeper$Average.Speed) <- c("Expressed in km/h")
comment(runkeeper$Climb) <- c("Expressed in m")
comment(runkeeper$Average.Heart.Rate) <- c("Expressed in BPM")

#Inspection of NA values - Column names containing NA values
colnames(runkeeper)[sapply(runkeeper, function(x) any(is.na(x)))]

#As we can see from the previous line, the dataset has only 2 columns filled with NA-values
#By giving a close look to each column we see that Friend.s.Tagged is made of only NA-values, then we can remove it from the data

all(is.na(runkeeper$Friend.s.Tagged))

#delete Friend.s.Tagged col
runkeeper <- runkeeper %>% 
  select(-(Friend.s.Tagged))

#Print out the new dataframe to see the improvements
str(runkeeper)

#Average.Heart.Rate col inspection
runkeeper %>% filter(is.na((Average.Heart.Rate)))

sum(is.na(runkeeper$Average.Heart.Rate))
