# I shall use the conflicted package to manage conflicts.
library(conflicted)
# Setting dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(readxl)
library(RSQLite)
library(sqldf)
library(tidyverse)
library(dplyr)
# Accessing and drawing on Divvy (bike sharing data) data sets (Microsoft Excel worksheets)
 q1_2019<-read_excel('Divvy_Trips_2019_Q1.xlsx')
 q1_2020<-read_excel('Divvy_Trips_2020_Q1.xlsx')
# Seeing how merging the two data sets would look like
tentative_attempted_merged_sheet <- bind_rows(q1_2019,q1_2020)
str(tentative_attempted_merged_sheet)
head(tentative_attempted_merged_sheet)
tail(tentative_attempted_merged_sheet)
# Inspecting the data frames and looking for incongruities
head(q1_2019)
head(q1_2020)
str(q1_2019)
str(q1_2020) 

# Comparing the column names of the data frames in each of the files
colnames(q1_2019)
colnames(q1_2020)
# Looking for possible primary keys or identifiers for each trip
length(unique(q1_2019$bikeid))
anyDuplicated(q1_2019$bikeid)
anyDuplicated(q1_2019$trip_id)
anyDuplicated(q1_2020$ride_id)
anyDuplicated(q1_2019$tripduration)
unique(q1_2020$rideable_type)
table(q1_2020$rideable_type)
count(q1_2020,rideable_type,sort=TRUE)
count(q1_2020,rideable_type,sort=TRUE)[2]==table(q1_2020$rideable_type)[1]
sort(unique(q1_2019$day_of_week))
sort(unique(q1_2020$day_of_week))
all(sort(unique(q1_2020$day_of_week))==sort(unique(q1_2019$day_of_week)))
identical(length(unique(q1_2019$day_of_week)),length(unique(q1_2020$day_of_week)))
setequal(q1_2019$day_of_week,q1_2020$day_of_week)
length(q1_2019$day_of_week)<length(q1_2020$day_of_week)
nrow(q1_2019);nrow(q1_2020)
nrow(q1_2019)<nrow(q1_2020)
  tentative_attempted_merged_sheet  |> 
  summarise(
    across(where(is.numeric), list(
      num_non_na_rows = ~sum(!is.na(.)),
      distinct = ~n_distinct(.),
      max = ~max(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      mean = ~mean(., na.rm = TRUE)
    ))
  ) -> overall_summary
dim(overall_summary)
str(overall_summary)
class(overall_summary)
typeof(overall_summary)
summary(overall_summary)
overall_summary[1,1]
mode(overall_summary)
# Renaming the columns to make them consistent with q1_2020 
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
))
typeof(q1_2019$rideable_type);typeof(q1_2019$ride_id)

# Converting ride_id and rideable_type to character 
# so that they can stack correctly
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
typeof(q1_2019$rideable_type);typeof(q1_2019$ride_id)

# Stacking the individual quarters's data frames into one big data frame
all_trips <- bind_rows(q1_2019,q1_2020)
class(all_trips)
is_tibble(all_trips)

# Removing lat, long, birthyear, tripduration, and gender fields
# because these data were dropped beginning in 2020
all_trips <- select(all_trips, -c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,tripduration))
names(all_trips)
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
tail(all_trips)
class(all_trips$ride_length)
typeof(all_trips$ride_length)
mode(all_trips$ride_length)
is_vector(all_trips$ride_length)
str(all_trips)  # Looking at a list of columns and data types (numeric, character, etc.)
summary(all_trips) # Looking at a statistical summary of the data

# Looking at how many observations fall under each user type
table(all_trips$member_casual)

# Adding columns that list the date, month, day, and year of 
# each ride so that I can aggregate ride data for each month, day, 
# or year because, currently, I can aggregate only at the ride level.
all_trips$start_time <- ymd_hms(all_trips$started_at)
all_trips$hour <- hour(all_trips$started_at)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(all_trips$date, "%m")
all_trips$day <- format(all_trips$date, "%d")
all_trips$year <- format(all_trips$date, "%Y")
all_trips$day_of_week <- format(all_trips$date, "%A")
colnames(all_trips)
# Adding a calculation of the length of each bike ride to all_trips (in seconds)
all_trips$ride_duration <- difftime(all_trips$ended_at,all_trips$started_at,units="secs")

str(all_trips)
is.factor(all_trips$ride_duration)
typeof(all_trips$ride_duration)
all_trips$ride_duration <- as.numeric(all_trips$ride_duration)
is.numeric(all_trips$ride_duration)
typeof(all_trips$ride_duration)

# The data frame includes a few hundred entries for when bikes were taken out of docks and 
# checked for quality by Divvy or where ride_length was negative.
# Therefore, I shall create a new version of the data frame (v2) 
# because data is being removed.

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_duration<0),]
colnames(all_trips_v2)

# Doing a descriptive analysis on ride_duration (all figures are in seconds)
mean(all_trips_v2$ride_duration) 
median(all_trips_v2$ride_duration) # shows the midpoint number in the ascending array of durations of each ride
max(all_trips_v2$ride_duration)
min(all_trips_v2$ride_duration) 
summary(all_trips_v2$ride_duration)

# Comparing members and casual users
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = length)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual, FUN = sum)
# Looking at the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# The days of the week are out of order, so I put 
# their levels into my desired order so that 
# they appear in the correct and expected
# order in the plots and analysis results.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Looking at the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = median)


aggregate(all_trips_v2$ride_duration ~ all_trips_v2$day_of_week, FUN = median)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$day_of_week, FUN = mean)
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$day_of_week, FUN = sum) 
aggregate(all_trips_v2$ride_duration ~ all_trips_v2$day_of_week, FUN = length)

# Analyzing ridership data by type and weekday

  all_trips_v2  |>  
    mutate(weekday = wday(started_at, label = TRUE))  |>   #creates weekday field using wday()
    group_by(member_casual, weekday)  |>   #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
              ,average_duration = mean(ride_duration))  |>  		# calculates the average duration
  arrange(member_casual, weekday)

  all_trips_v2  |>  
    group_by(member_casual, month)  |>   
    summarise(number_of_rides = n()							
              ,average_duration = mean(ride_duration))  |>  		
    arrange(member_casual, month)
  
  all_trips_v2  |>  
    group_by(member_casual, hour)  |>   
    summarise(number_of_rides = n()							
              ,average_duration = mean(ride_duration))  |>  		
    arrange( desc(number_of_rides))
  
  all_trips_v2  |>  
    group_by(member_casual, hour)  |>   
    summarise(number_of_rides = n()							
              ,average_duration = mean(ride_duration))  |>  		
    arrange(desc(member_casual), desc(average_duration))
  
  all_trips_v2  |>  
    group_by(member_casual, hour)  |>   
    summarise(number_of_rides = n()							
              ,average_duration = mean(ride_duration))  |>  		
    arrange( desc(average_duration))
  
  all_trips_v2  |>  
    group_by(member_casual, rideable_type)  |>   
    summarise(number_of_rides = n()							
              ,average_duration = mean(ride_duration))  |>  		
    arrange(desc(member_casual), desc(average_duration))
  
  max(all_trips_v2$ride_duration)/60/60
 
  # Visualizing the number of rides by rider type	
  
    all_trips_v2  |>  
    mutate(weekday = wday(started_at, label = TRUE))  |>  
    group_by(member_casual, weekday)  |>  
  summarise(number_of_rides = n()
              ,average_duration = mean(ride_duration))  |>  
    arrange(member_casual, weekday)   |>  
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Creating a visualization for average duration
  
    all_trips_v2  |>  
    mutate(weekday = wday(started_at, label = TRUE))  |>  
    group_by(member_casual, weekday)  |>  
  summarise(number_of_rides = n()
              ,average_duration = mean(ride_duration))  |>  
    arrange(member_casual, weekday)   |>  
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
    
  colnames(all_trips_v2)
  
# Creating a csv file that I shall visualize in Tableau
counts <- aggregate(all_trips_v2$ride_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN=mean)
write.csv(counts,file='avg_ride_length.csv') 
