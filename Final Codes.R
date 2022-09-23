#Importing Library
library(ggplot2)
library(ggthemes)
library(lubridate)

library(dplyr)
library(tidyr)
library(DT)
library(scales)

#require every library needed for data frame manipulation
library(RColorBrewer)
require(RColorBrewer) 
library(plyr)
require(scales)

#Create Vector of colors to be implemented in our Plots
colorsData <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

setwd("/Users/anishashrivastava/Desktop/Anisha/Project Work_Data Science/CSV Original Files/Used")
aprilData <- read.csv("uber-raw-data-apr14.csv")
mayData <- read.csv("uber-raw-data-may14.csv")
juneData <- read.csv("uber-raw-data-jun14.csv")
julyData <- read.csv("uber-raw-data-jul14.csv")
augustData <- read.csv("uber-raw-data-aug14.csv")
septemberData <- read.csv("uber-raw-data-sep14.csv")

#Combine All Files in one Dataset
uberData <- rbind(aprilData,mayData,juneData,julyData,augustData,septemberData)
head(uberData)

dim(uberData)

#Formatting Of Date and Time Column 
uberData$Date.Time <- as.POSIXct(uberData$Date.Time,format="%m/%d/%Y%H:%M:%S")
uberData$Time <- format(as.POSIXct(uberData$Date.Time,format="%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
uberData$Date.Time <- ymd_hms(uberData$Date.Time)

#Create Foctors
uberData$day <- factor(day(uberData$Date.Time))
uberData$month <- factor(month(uberData$Date.Time,label = TRUE))
uberData$year <- factor(year(uberData$Date.Time))
uberData$daysofweek <- factor(wday(uberData$Date.Time,label = TRUE))
uberData$hour <- factor(hour(hms(uberData$Time)))
uberData$minute <- factor(minute(hms(uberData$Time)))
uberData$second <- factor(second(hms(uberData$Time)))

#Plotting the trips by the hours in a day
# Select the data needed for this task
daily_trend <- subset(uberData, select = c(Time, month))

#Change the time format to simply showing the hour so that it will be easier for regrouping the file and plotting (if name of x axis is too long, it will not be clear and pretty)
H<-format(as.POSIXct(strptime(daily_trend$Time, "%H:%M:%S", tz="")), format="%H")
daily_trend$Time <- H

#convert the time column into class time
daily_trend$Time <- as.character.Date(daily_trend$Time, format="%H")

#count the pickups by two cololumns: time and month
daily_trend<- ddply(daily_trend, .(daily_trend$Time, daily_trend$month), nrow)
names(daily_trend)<- c("Hour","Month","Pickups")

# plot the data - bar graph
ggplot(daily_trend, aes(Hour, Pickups, fill=Month))+geom_bar(stat = "identity")+ggtitle(label = "Trend Over Time of the Day")+theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+xlab("Hour")+ylab("Number of Pickups")


#Trips by Hour and month
monthHour <- uberData%>%group_by(month,hour)%>%
  dplyr::summarize(Total=n())

ggplot(monthHour,aes(hour,Total,fill=month))+
  geom_bar(stat = "identity")+ggtitle("Trips By Hour And Month")+
  theme_light()+scale_y_continuous(labels = comma)+xlab("Hour")

#Plotting Data By Trips During Every Day of the Month
dayGroup <- uberData%>%group_by(day)%>%
  dplyr::summarize(Total=n())

head(dayGroup,10)

ggplot(dayGroup,aes(day,Total))+
  geom_bar(stat = "identity",fill="purple",color="white")+
  ggtitle("Trips Every Day")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+theme_light()+xlab("Day")

#Monthly trend by Weekday

#Bar graph
ggplot(weekly_trend,aes(Month, Pickups)) + 
  geom_bar(aes(fill = Weekday),stat = "identity",position = "dodge")+scale_fill_brewer(palette = "Set2") +ggtitle(label = "Monthly Trend by Weekday")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+xlab("Month")+ylab("Number of Pickups")

#Number of Trips Taking place During months in A Year 
monthGroup <- uberData%>%
  group_by(month)%>%
  dplyr::summarize(Total=n())

head(monthGroup)

ggplot(monthGroup,aes(month,Total,fill=month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips By Month")+theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+theme_light()+xlab("Month")

monthWeekday <- uberData%>%group_by(month,daysofweek)%>%summarize(Total=n())

ggplot(monthWeekday,aes(month,Total,fill=daysofweek))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("Trips By Day And Month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colorsData)+xlab("Month")+
  theme_light()

#Number of trip by Bases
ggplot(uberData,aes(Base))+
  geom_bar(fill="Blue")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips By Bases")+ylab("Count")+
  theme_light()

ggplot(uberData,aes(Base,fill=month))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips By Bases And Month")+
  scale_fill_manual(values = colorsData)+ylab("Count")+
  theme_light()

ggplot(uberData,aes(Base,fill=daysofweek))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips By Bases And Day Of Week")+
  scale_fill_manual(values = colorsData)+ylab("Count")+
  theme_light()

#create another column weekdays to the end of the column fill column with the data converted from the column Date
uberData$Weekday <- weekdays(as.Date(uberData$Date.Time,format="%m/%d/%Y"))

#count the frequency of the value in column Weekday
pickup_frequency<- as.data.frame(table(uberData$Weekday))

#rename the header so that it's more conveninient to do step 4 visualization
names(pickup_frequency)<- c("Weekday","Pickups")

#reorder the table according to Weekdays, so that when it comes to data visualization the weekdays will be in the right order
pickup_frequency$Weekday<- factor(pickup_frequency$Weekday, levels=c("Monday",
                                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
pickup_frequency<-pickup_frequency[order(pickup_frequency$Weekday),]


#show the weekday with most pickups along with the frequency
pickup_frequency[which.max(pickup_frequency$Pickups),]

#show the weekday with least pickups along with the frequency
pickup_frequency[which.min(pickup_frequency$Pickups),]

ggplot(data=pickup_frequency, aes(x=Weekday, y=Pickups, fill=Weekday))+geom_bar(
  stat="identity")+ggtitle(label="Weekday Pickup Comparison")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+xlab("Weekdays")+ylab("Number of Pickups")

#add column Month to the master data frame
uberData$Month<-months(as.POSIXct(uberData$Date, format="%m/%d/%Y"))

#subset the data so that the size is smaller, and it is easier to plot
weekly_trend <- subset(uberData, select = c(Weekday, Month))

#count the pickups by two cololumns: weekday and month
weekly_trend<- ddply(weekly_trend, .(weekly_trend$Weekday, weekly_trend$Month), nrow)

#change the column name of the new data frame
names(weekly_trend) <- c("Weekday", "Month","Pickups")

#reorder the data frame according to two columns: weekday and month
weekly_trend$Weekday <- factor(weekly_trend$Weekday, levels=c("Monday",
                                                              "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
weekly_trend$Month<- factor(weekly_trend$Month, levels = c("April", "May", "June", "July","August","September"))
weekly_trend<-weekly_trend[with(weekly_trend, order(Month, Weekday)),]

#plot the data
ggplot(weekly_trend,aes(Weekday, Pickups)) + 
  geom_bar(aes(fill = Month),stat = "identity",position = "dodge")+scale_fill_brewer(palette = "Accent") +ggtitle(label = "Weekday Pickup Each Month")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold"))+xlab("Weekdays")+ylab("Number of Pickups")

#Count the frequency of each month
mpickups<-as.data.frame(table(uberData$Month))
names(mpickups)<- c("Month","Pickups")

#reorder the table accordiing to mouth so that it is easier to plot in the next step
mpickups$Month<- factor(mpickups$Month, levels = c("April", "May", "June", "July","August","September"))
mpickups<-mpickups[order(mpickups$Month),]

#Plot
ggplot(mpickups, aes(x=Month, y=Pickups, group=1))+geom_point(color="dark green")+geom_line(color="orange")+ggtitle(label = "Trend over the Months by Month")+theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+ylab("Number of Pickups")

#Trend Over Time of the Day - line graph
ggplot(daily_trend, aes(Hour, Pickups, group=Month))+geom_line(aes(color=Month))+ggtitle(label = "Trend Over Time of the Day")+theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+xlab("Hour")+ylab("Number of Pickups")

#Creating A Map Visualization of Rides in NEW YORK
minLat <- 40.5774
maxLat <- 40.9176
minLong <- -74.15
maxLong <- -73.7004

ggplot(uberData,aes(Lon,Lat))+
  geom_point(size=1,color="blue")+
  scale_x_continuous(limits = c(minLong,maxLong))+
  scale_y_continuous(limits = c(minLat,maxLat))+
  theme_map()+
  ggtitle("New York Map Based On Uber Rides During 2014 From April To September")

ggplot(uberData,aes(x=Lon,y=Lat,color=Base))+
  geom_point(size=1)+
  scale_x_continuous(limits = c(minLong,maxLong))+
  scale_y_continuous(limits = c(minLat,maxLat))+
  theme_map()+
  ggtitle("New York Map Based On Uber Rides During 2014 From April To September By Base")


#Customer Waiting Time vs Customer Review Analysis

setwd("/Users/anishashrivastava/Downloads/Customer Review Analysis")
apr_data <- read.csv("/Users/anishashrivastava/Downloads/Customer Review Analysis/Uber-Data V1.csv")
glimpse(apr_data)

#may_data <- read.csv("Uber-May.csv")
#glimpse(may_data)
#jun_data <- read.csv("Uber-Jun.csv")
#glimpse(jun_data)
#jul_data <- read.csv("Uber-Jul.csv")
#glimpse(jul_data)
#aug_data <- read.csv("Uber-Aug.csv")
#glimpse(aug_data)
#sep_data <- read.csv("Uber-Sep.csv")
#glimpse(sep_data)

data_2014 <- rbind(apr_data)
#may_data, jun_data, jul_data, aug_data, sep_data)
glimpse(data_2014)


df = subset(data_2014, select = -c(X,X.1,X.2,X.3,X.4,X.5,X.6) )
View(df)
glimpse(df)
#customerwaiting_data <- data_2014 %>%
#group_by(Customer.waiting.time) %>%
#dplyr::summarize(Count = n()) 
#datatable(customerwaiting_data)


#ggplot(data_2014, aes(x=Customer.waiting.time)) + 
#geom_histogram(binwidth = 5)
#ggtitle("Customer waiting time v/s Customer Review")+
#scale_fill_manual(values = colors)

#ggplot( customerwaiting_data, aes(x=factor(Customer.waiting.time), y=Count)) + 
#geom_col(color='black',fill='cyan3')+
#xlab('Customer Review')+
#ggtitle("Customer waiting time v/s Customer Review")

customerreview_customerwaitingtime_data <- df %>% group_by(Customer.waiting.time, Customer.review) %>%  dplyr::summarize(Total = n())

ggplot(customerreview_customerwaitingtime_data, aes(Customer.waiting.time, Total, fill=reorder(Customer.review)) + 
  geom_bar(stat = "identity",aes(fill = Customer.review)) + 
  ggtitle("Customer waiting time v/s Customer Review") + 
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

#Trips by Age Graph

setwd("C:\\Data Science\\Project\\Final")
uber_data <- read.csv("Uber-data.csv")
glimpse(uber_data)


age_data <- uber_data %>%
  group_by(Age) %>%
  dplyr::summarize(Count = n()) 
datatable(age_data)

ggplot( age_data, aes(x=factor(Age), y=Count)) + 
  geom_col(color='black',fill='cyan3')+
  xlab('Age')+
  ggtitle("Trips by Age")



