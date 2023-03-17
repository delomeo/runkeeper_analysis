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

#As we can see from the previous line, the dataset has 214 NA-values in the Average.Heart.Rate column
#We can remove them from the dataset as they are not relevant for our analysis and they are not a lot of them 

#First we need to convert the column to date format
runkeeper$Date <- as.Date(runkeeper$Date)  #convert Date column to Date format

#Plotting the Average Heart Rate over time
ggplot(runkeeper, aes(x=Date, y=Average.Heart.Rate)) + 
    geom_point( size=2, alpha=0.5, color=c(unique(runkeeper$Type))) +
    scale_color_manual(values = c("red", "blue", "green", "orange")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle("Average Heart Rate over time")+
    xlab("Date")+
    ylab("Average Heart Rate")+ 
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+ 
    theme(plot.title = element_text(hjust = 0.5))+  
    theme(plot.title = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 15))+
    theme(axis.title.y = element_text(size = 15))+
    theme(axis.text.x = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 10)) +  
    theme(legend.title = element_text(size = 15))+
    theme(legend.text = element_text(size = 15))+ 
    theme(legend.position = "bottom")+ 
    theme(legend.background = element_rect(fill = "white"))+
    theme(legend.key = element_rect(fill = "white"))+
    theme(legend.key.size = unit(1, "cm"))+
    theme(legend.key.height = unit(1, "cm"))+
    theme(legend.key.width = unit(1, "cm"))+
    theme(legend.key.size = unit(1, "cm"))

