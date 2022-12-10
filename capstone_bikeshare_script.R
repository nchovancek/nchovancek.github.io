#install and load packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

#verify working directory
getwd()

#step 1, collect and load data
dec21<-read_csv("202112-divvy-tripdata.csv")
jan22<-read_csv("202201-divvy-tripdata.csv")
feb22<-read_csv("202202-divvy-tripdata.csv")
mar22<-read_csv("202203-divvy-tripdata.csv")
apr22<-read_csv("202204-divvy-tripdata.csv")
may22<-read_csv("202205-divvy-tripdata.csv")
jun22<-read_csv("202206-divvy-tripdata.csv")
jul22<-read_csv("202207-divvy-tripdata.csv")
aug22<-read_csv("202208-divvy-tripdata.csv")
sep22<-read_csv("202209-divvy-tripdata.csv")
oct22<-read_csv("202210-divvy-tripdata.csv")
nov22<-read_csv("202211-divvy-tripdata.csv")

#step 2, wrangle to a single file
#check colnames to see if any need adjusting, need consistency
colnames(dec21)
colnames(jan22)
colnames(feb22)
colnames(mar22)
colnames(apr22)
colnames(may22)
colnames(jun22)
colnames(jul22)
colnames(aug22)
colnames(sep22)
colnames(oct22)
colnames(nov22)

#inspect dataframes to check for inconsistencies
str(dec21)
str(jan22)
str(feb22)
str(mar22)
str(apr22)
str(may22)
str(jun22)
str(jul22)
str(aug22)
str(sep22)
str(oct22)
str(nov22)

#stack 'em up
all_trips<- bind_rows(dec21, jan22, feb22, mar22, apr22, may22, jun22, jul22, 
                      aug22, sep22, oct22, nov22)
str(all_trips)


#Clean data, ensure consistency
table(all_trips$member_casual)
all_trips<- all_trips %>% 
  mutate(member_casual=recode(member_casual,
                              "Subscriber"="member",
                              "Customer"="casual"))

#add columns for date, month, day, year of each ride
#allows for more useful aggregation of ride data
all_trips$date<-as.Date(all_trips$started_at)
all_trips$month<- format(as.Date(all_trips$date), "%m")
all_trips$day<- format(as.Date(all_trips$date), "%d")
all_trips$year<- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week<- format(as.Date(all_trips$date), "%A")

#add ride length calculation to all trips, in seconds
all_trips$ride_length<-difftime(all_trips$ended_at, all_trips$started_at)
#use str to inspect the structure of the new columns
#note that now, ride_length is a difftime num
#must convert ride_length from factor to numeric so we can calc on it
str(all_trips)
is.factor(all_trips$ride_length)
all_trips$ride_length<-as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#remove bad data (bikes taken out of service, negative ride length, etc)
all_trips_v2<-all_trips[!(all_trips$start_station_name=="HQ QR" | all_trips$ride_length<0),]
all_trips_v2<-drop_na(all_trips_v2)

#Step 4, descriptive analysis
summary(all_trips_v2$ride_length)
summary((all_trips_v2$ride_length)/60)

#use aggregate for further comparison of members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

#fix order of days of week
#see avaerage ride time per weekday for members v casual
all_trips_v2$day_of_week<- ordered(all_trips_v2$day_of_week, levels=c("Sunday",
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN=mean)
all_trips_v2 %>% aggregate(ride_length/60 ~ member_casual + day_of_week, FUN=mean)

#now the same in minutes
aggregate((all_trips_v2$ride_length)/60~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN=mean)

#analyze ridership data by type and weekday
#below creates weekday field, groups by user type and weekday,
#calculates and summarizes, sorts
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#visualize riders by type, take above code and pipe into ggplot. Cool!
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_rides, fill=member_casual)) +
  geom_col(position='dodge')+ labs(title="Number of Riders by Type and Day of Week")

#visualize average ride duration
all_trips_v2 %>% 
  mutate(weekday=wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides=n(), average_duration=mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual))+
  geom_col(position='dodge')+labs(title="Mean Ride Duration by Type, Day of Week")

#histogram of ride duration, facet wrap casual/member, fill bike type
?geom_histogram()
hist_data<-all_trips_v2[!(all_trips_v2$ride_length>3000),]
hist_data<- drop_na(hist_data)
ggplot(data=hist_data)+geom_histogram(mapping=aes(ride_length/60, fill=rideable_type))+
  facet_wrap(~member_casual)+labs(title="Distribution of Ride Length in Minutes",
                                  x="Ride Length, Minutes", y="Count of Rides")
