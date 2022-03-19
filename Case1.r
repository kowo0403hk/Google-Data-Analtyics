#This is for Google Data Analytics Capstone Project (Case Study 1)
#Load libraries
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)


#Step 1: Import raw data (12 months).
t202103 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202103-divvy-tripdata.csv")
t202104 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202104-divvy-tripdata.csv")
t202105 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202105-divvy-tripdata.csv")
t202106 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202106-divvy-tripdata.csv")
t202107 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202107-divvy-tripdata.csv")
t202108 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202108-divvy-tripdata.csv")
t202109 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202109-divvy-tripdata.csv")
t202110 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202110-divvy-tripdata.csv")
t202111 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202111-divvy-tripdata.csv")
t202112 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202112-divvy-tripdata.csv")
t202201 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202201-divvy-tripdata.csv")
t202202 <- read_csv("~/R Directory/Google Case Study/Case1/CSV/202202-divvy-tripdata.csv")


#Combine all data into one table.
annual <- rbind(t202103, t202104, t202105, t202106, t202107, t202108, t202109, t202110, t202111, t202112, t202201, t202202)


#Step 2: Data profiling - Inspect the combined table.
str(annual)
colnames(annual)
head(annual)
dim(annual)
summary(annual)


#Step 3: Data Cleaning
#3.1 to drop unnecessary data columns and create a new table.
all_trips <- annual %>% 
  select(-c(start_lat, end_lat, start_lng, end_lng))


#3.2 Add more columns to the data set, basically breaking down the date into smaller pieces
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#3.3 Add a column calculate the trip duration
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)


#3.4 Convert ride_length from factor to numeric for calculations
is.factor(all_trips$ride_length) ##is.factor is to check if the value is a type of factor, only returns boolean value
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length)) ##to coerce ride_length into characters, then coerce into number
is.numeric(all_trips$ride_length) ##to check again if the data has been converted to numeric properly


#3.5 Remove any data where ride_length is negative and create a new df
all_trips_v2 <- all_trips[(!all_trips$ride_length <= 0), ]


#P.S. We can do this to convert your data into HMS format, however, it will mess up the aggregate calculations later on.
all_trips_v3 <- all_trips_v2
all_trips_v3$ride_length <- seconds_to_period(all_trips_v3$ride_length)


#Step 4: Descriptive analysis
#4.1 Summary 
summary(all_trips_v2$ride_length) ##by seconds
summary(all_trips_v3$ride_length) ##by minutes


#4.2 Aggregate: Ride_length 
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)


#4.2.1 Ride_length during Day of Week (Average Duration)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
ride_duration_sum <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean) #assign df to the table above


#4.3 Aggregate: Usage Pattern during Day of Week (Number of rides per day of week)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), averager_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)


#4.3.1 assign df
pattern_sum <- all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), averager_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)


#4.4 Aggregate: Types of Bike 
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% ##group columns by first: user type, second: bike type
  summarise(Num_count = n()) %>% ##create a new column to count number of rows, the previous group function will separate the counts based on groups
  arrange(member_casual, rideable_type) ##sort first by user type, then bike type


#4.4.1 assign df  
types_of_bike <- all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(Num_count = n()) %>% 
  arrange(member_casual, rideable_type)



#Step 5: Visualization 

#5.1 visualize the numbers of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% ##use the same flow from 4.3
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + ##"dodge" makes the graph not stacked
  labs(title = "Number of Usage by Day of Week", subtitle = "Casual vs. Member",
       caption = "Data from March 2021 to February 2022")


#5.2 visualize for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% ##use the same flow from 4.3
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Bike Duration by Day of Week", subtitle = "Casual vs. Member",
       caption = "Data from March 2021 to February 2022")


#5.3 visualize for types of bike
types_of_bike %>% 
  ggplot(aes(x = rideable_type, y = Num_count, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Types of Bike Usage", subtitle = "Casual vs. Member",
       caption = "Data from March 2021 to February 2022")
  


##to export data for further visualization
write.csv(ride_duration_sum, "~/R Directory/Google Case Study/Case1/ride_duration_summary.csv")
write.csv(pattern_sum, "~/R Directory/Google Case Study/Case1/pattern_summary.csv")
write.csv(types_of_bike, "~/R Directory/Google Case Study/Case1/types_of_bike_summary.csv")
write.csv(all_trips_v2, "~/R Directory/Google Case Study/Case1/all_trips_v2.csv")
