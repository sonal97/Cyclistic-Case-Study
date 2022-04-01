install.packages("tidyverse")
install.packages("lubridate")
install.packages("modeest")
library(modeest)
library("tidyverse")
library("lubridate")
library("ggplot2")
library("dplyr")
library(readxl)

getwd()
setwd("") #Set your working directory

y2020_04 <- read_xlsx("202004-divvy-tripdata.xlsx")
y2020_05 <- read_xlsx("202005-divvy-tripdata.xlsx")
y2020_06 <- read_xlsx("202006-divvy-tripdata.xlsx")
y2020_07 <- read_xlsx("202007-divvy-tripdata.xlsx")
y2020_08 <- read_xlsx("202008-divvy-tripdata.xlsx")
y2020_09 <- read_xlsx("202009-divvy-tripdata.xlsx")
y2020_10 <- read_xlsx("202010-divvy-tripdata.xlsx")
y2020_11 <- read_xlsx("202011-divvy-tripdata.xlsx")
y2020_12 <- read_xlsx("202012-divvy-tripdata.xlsx")
y2021_01 <- read_xlsx("202101-divvy-tripdata.xlsx")
y2021_02 <- read_xlsx("202102-divvy-tripdata.xlsx")
y2021_03 <- read_xlsx("202103-divvy-tripdata.xlsx")
y2021_04 <- read_xlsx("202104-divvy-tripdata.xlsx")
y2021_05 <- read_xlsx("202105-divvy-tripdata.xlsx")
#y2021_06 <- read_xlsx("202106-divvy-tripdata.xlsx")
y2021_07 <- read_xlsx("202107-divvy-tripdata.xlsx")

View(y2020_04)
View(y2020_12)
View(y2021_01)
View(y2021_02)
View(y2021_03)

#TRANSFORM DATA, data inconsistencies
options=(warn=-1)
y2020_12 <- mutate(y2020_12, start_station_id = as.numeric(start_station_id),
                   end_station_id = as.numeric(end_station_id))
y2021_01 <- mutate(y2021_01, start_station_id = as.numeric(start_station_id),
                   end_station_id = as.numeric(end_station_id))
y2021_02 <- mutate(y2021_02, start_station_id = as.numeric(start_station_id),
                   end_station_id = as.numeric(end_station_id))
y2021_03 <- mutate(y2021_03, start_station_id = as.numeric(start_station_id),
                   end_station_id = as.numeric(end_station_id))

#combine data
all_trips_data <- bind_rows(
  y2020_04, 
  y2020_05, 
  y2020_06, 
  y2020_07, 
  y2020_08, 
  y2020_09, 
  y2020_10, 
  y2020_11, 
  y2020_12, 
  y2021_01, 
  y2021_02, 
  y2021_03, 
)
View(all_trips_data)

str(all_trips_data)
glimpse(all_trips_data)
colnames(all_trips_data)

#add columns for date,month,day,year
all_trips_data$date <- as.Date(all_trips_data$started_at)
all_trips_data$month <- format(as.Date(all_trips_data$date),"%m")
all_trips_data$day_no <- format(as.Date(all_trips_data$date),"%d")
all_trips_data$year <- format(as.Date(all_trips_data$date),"%Y")
all_trips_data$day_of_week <- format(as.Date(all_trips_data$date),"%A")

#remove unncessary columns
all_trips_data <- all_trips_data %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng))

#calculate ride length in seconds
all_trips_data$ride_length <- as.numeric(difftime(all_trips_data$ended_at,all_trips_data$started_at))

#drop na rows
all_trips_no_na_data <- drop_na(all_trips_data)
View(all_trips_no_na_data)

#removing negative ride length
all_trips <- all_trips_no_na_data[!(all_trips_no_na_data$start_station_name == "HQ QR" | all_trips_no_na_data$ride_length<0),]
View(all_trips)


#perform calculations
summary(all_trips)
str(all_trips)

summary(all_trips$ride_length)
summary(all_trips$ride_length)/60   #avg=30 mins

#avg and median and mode of casual n members
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual, FUN = mean)
    # avg casual ride time = 46 mins, avg member ride time = 16 mins
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual, FUN = median)
    # casual median rides = 22 mins, member median rides = 12 mins
aggregate(all_trips$day_of_week ~ all_trips$member_casual, FUN = mfv)
    # most common day to rent bikes is Saturday


#min,max
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = min)
aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = max)

#avg ride time by day
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
View(all_trips$day_of_week)
aggregate(all_trips$ride_length/60 ~ all_trips$member_casual + all_trips$day_of_week, FUN = mean)
    #regardless of day of week, casual riders ride 2x-3x longer than member riders, both ride longer on weekends

###
ggplot()
ggplot(all_trips)
ggplot(all_trips) + geom_bar(mapping = aes(x=ride_length))
ggplot(all_trips) + geom_bar(mapping = aes(x=day_of_week))
ggplot(all_trips) + geom_bar(mapping = aes(x=day_of_week,fill=member_casual))
ggplot(all_trips) + geom_bar(mapping=aes(x=day_of_week)) + facet_wrap(~member_casual)
ggplot(all_trips) + geom_bar(mapping=aes(x=day_of_week,fill=member_casual)) + facet_wrap(~member_casual)
###


#viz
#1  ## ----Number of rides by rider type-----
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 1: Number of Rides by Day and Rider Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Day of Week")


#2  ----Average duration----
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 2: Average Ride Duration by Day and Rider Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")


#3 ----Number of rides by bike type and rider type-----
all_trips %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, rideable_type)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 3: Number of Rides by Bike Type and Rider Type") + 
  ylab("Number of Rides (5e+05 = 500,000)") + 
  xlab("Bike Type")


#4 ----Number of rides by day and bike type-----------------------------------
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(rideable_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 4: Number of Rides by Day and Bike Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Day of Week")


#5 ----Average duration by bike type------------------------------------------
all_trips %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(rideable_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length/60)) %>% 
  arrange(rideable_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Table 5: Average Ride Duration by Day and Bike Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")


#6 ----Number of rides by month and rider type-----
all_trips %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) + 
  geom_point() +
  labs(title = "Table 6: Number of Rides by Month and Rider Type") + 
  ylab("Number of Rides (1e+05 = 100,000)") + 
  xlab("Month")

