#1. Code for reading in the dataset and/or processing the data
library(dplyr)
library(lubridate)
library(ggplot2)
activity_data <- read.csv("activity.csv")
activity_data$date <- ymd(activity_data$date )

activity_data$weekday <- weekdays(activity_data$date)



#2. Histogram of the total number of steps taken each day
steps_perday <- activity_data %>% group_by(date) %>% 
    summarise(step_taken=sum(steps,na.rm = T))
g <- ggplot(steps_perday) + geom_histogram(aes(x=step_taken), binwidth = 5000)
print(g)

#3. Mean and median number of steps taken each day
#Mean
mean_steps <- round(mean(steps_perday$step_taken))
print(mean_steps)
#median
median_steps <- round(median(steps_perday$step_taken))
print(median_steps)

#4. Time series plot of the average number of steps taken
steps_trend <- activity_data %>% group_by(interval) %>% 
    summarise(tot_step=sum(steps,na.rm = T))

ggplot(steps_trend) + geom_line(aes(x=interval,y=tot_step)) + 
    ylab("Step Trend")
#5. The 5-minute interval that, on average, contains the maximum number of steps


max_step_row <- which.max(steps_trend$tot_step)

max_interval <- steps_trend$interval[max_step_row]
print(max_interval)

#6.Code to describe and show a strategy for imputing missing data

activity_data$steps <- ifelse(is.na(activity_data$steps),
                              ave(activity_data$steps, FUN = function(x) median(x, na.rm=T)),
                              activity_data$steps)

#7. Histogram of the total number of steps taken each day 
#after missing values are imputed
steps_perday_after <- activity_data %>% group_by(date) %>% 
    summarise(step_taken=sum(steps,na.rm = T))


g2 <- ggplot(steps_perday_after) + geom_histogram(aes(x=step_taken), binwidth = 5000)
print(g2)

#8. Panel plot comparing the average number of steps taken per 5-minute interval 
#across weekdays and weekends

activity_data$Weekends <- ifelse(activity_data$weekday %in% c("Saturday","Sunday"),
                                 "Weekend",
                                 "Weekdays")
weeks_trend <- activity_data %>% group_by(interval,Weekends) %>% 
    summarise(ave_step=mean(steps))

ggplot(weeks_trend) + geom_line(aes(x=interval,y=ave_step)) +
    facet_grid(.~Weekends)+
    xlab("Time in minutes")+
    ylab("Average Steps")



