# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

### Loading the dataset in R

data <- read.csv("activity.csv", 
                 colClasses = c("numeric", "character","numeric"))

### Checking the dataset variables

names(data)
[1] "steps"    "date"     "interval"

### Checking the dataset head and tail

head(data)
tail(data)

### Formatting the date field 

data$date <- as.Date(data$date,"%Y-%m-%d")


## What is mean total number of steps taken per day?

### Getting a summary of the dataset. 

summary(data)

### The summary above excludes the NA???s but the calculated mean of 37.38 steps is the mean at the discrete interval of 5 min and NOT the mean of the total number of steps take per day.

### Using the aggregate function to create a summary dataset by date and then calculating the means of the total number of steps per day

data_dailysummary <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)

mean(data_dailysummary$steps)


### Plotting a histogram to visualize the distribution of the total number of steps per day

library(lattice)

hist(data_dailysummary$steps, 
main = "Total steps per day", 
xlab = "Steps per Day", 
ylab="Freq", 
col = "white")


## What is the average daily activity pattern?

### Making a time series plot (i.e. type = ???l???) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### Getting the mean of steps and time series data

time_series_data <- tapply(data$steps, data$interval, mean, na.rm = TRUE)

### Plotting the mean of steps using the time series data
plot(row.names(time_series_data), time_series_data, 
type = "l", 
xlab = "5-min interval", 
ylab = "Average Steps per 5-min interval", 
main = "Time Series of the Average number of steps taken", 
col = "blue")

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_steps_interval <- which.max(time_series_data)

### then identifying the 5-minute interval of max_steps_interval

names(max_steps_interval)

## Imputing missing values

### Calculate and report the total number of missing values in the dataset

summary(data$steps)

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval,etc

### Using the approach to calculate the mean of the number of steps by 5-min interval

StepsAverageByInterval <- aggregate(steps ~ interval, data = data, FUN = mean)

### Then creating a function to fill the average of steps at the intervals with value NA of the dataset with all observations
NAFilling <- numeric()
for (i in 1:nrow(data)) {
          xi <- data[i, ]
          if (is.na(xi$steps)) {
                    steps <- subset(StepsAverageByInterval, interval == xi$interval)$steps
          } else {
                    steps <- xi$steps
          }
          NAFilling <- c(NAFilling, steps)
}

### And creating a new dataset that is equal to the original dataset but with the missing data filled in using the mean of the number of steps at the respective 5-min interval. By applying the summary function validates that there are no NAs and the length

data_NAfilled <- data

data_NAfilled$steps <- NAFilling

summary(data_NAfilled$steps)

### Both datasets have the same dimensions

dim(data)

dim(data_NAfilled)

### Making a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

TotalSteps <- aggregate(steps ~ date, data = data_NAfilled, sum, na.rm = TRUE)

hist(TotalSteps$steps, main = "Daily Total Steps", xlab = "Steps per Day", col = "white")

### Calculating the mean and median of the steps we conclude:

### (1) The mean is the same due to the fact that we filled the NAs with the mean (no impact on the mean calculation)

mean(TotalSteps$steps)


### (2) However the median is different in comparison to the original one and now it is equal to the mean.

median(TotalSteps$steps)


## Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

### Creating a new factor variable in the dataset with two levels: weekday and weekend indicating whether a given date is a weekday or weekend day.

DayType <- weekdays(data$date)
WeekDay <- vector()
for (i in 1:nrow(data)) {
          if (DayType[i] == "Saturday") {
                    WeekDay[i] <- "Weekend"
          } else if (DayType[i] == "Sunday") {
                    WeekDay[i] <- "Weekend"
          } else {
                    WeekDay[i] <- "Weekday"
          }
}
data$WeekDay <- WeekDay
data$WeekDay <- factor(data$WeekDay)
DailySteps <- aggregate(steps ~ interval + WeekDay, data = data, mean)

names(DailySteps) <- c("interval", "WeekDay", "steps")

### Make a panel plot containing a time series plot (i.e. type = ???l???) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


xyplot(steps ~ interval | WeekDay, DailySteps, type = "l", layout = c(1, 2), 
       panel = lattice.getOption("panel.xyplot"),
       xlab = "Interval", 
       ylab = "Number of steps",
)


#### Notes: It looks like this individual practices some activity during the morning due to spike during the weekdays (Monday Thru Friday) and have weekends with activity spread during the day.  
