# assumes activity data downloaded and unzipped into working directory 

# load packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)

# Loading and preprocessing the data (with initial assumption that we can ignore missing values)
# read activity data
file <- "activity.csv"
activity <- read.csv(file)
activity <- subset(activity, is.na(activity$steps) == FALSE)

# find mean total number of steps taken per day
# aggregate steps by day

DailySteps <- aggregate(activity$steps, by = list(activity$date), sum)
names(DailySteps) <- c("Day", "Steps")

# create histogram of daily steps
p <- ggplot(DailySteps, aes(x = Steps)) + geom_histogram(color = "black", fill = "grey") + ggtitle("Total Number of Steps Per Day")

# annotate histogram with mean and median data
p <- p + geom_vline(aes(xintercept = mean(Steps)), color = "red", linetype = "dashed", size = 1) 
p <- p + geom_vline(aes(xintercept = median(Steps)), color = "green", linetype = "dotted", size = 1) 
p <- p + labs(y = "Frequency (in days)", x = "Total Steps") 
# add subtitle explaining mean and median lines
p <- p + ggtitle(labs(subtitle = "Dashed Red Line ~ Average & Dotted Green Line ~ Median"))

#calculate mean and median
meanSteps <- mean(DailySteps$Steps)
print(meanSteps)
medianSteps <- median(DailySteps$Steps)
print(medianSteps)

#aggregate steps by 5-minute interval (x-axis) by the average number of steps taken, averaged across all days (y-axis)
StepsPer5 <- aggregate(activity$steps, by = list(activity$interval), mean)
names(StepsPer5) <- c("Interval", "AverageSteps")

# create time series plot
p2 <- ggplot(NULL, aes(y = StepsPer5$AverageSteps, x = StepsPer5$Interval)) 
p2 <- p2 + geom_line() + geom_point() 
p2 <- p2 + labs(y = "Average Steps per 5 Minute Interval", x = "5 Minute Time Interval Number") 
p2 <- p2 + ggtitle(label = "Time Series Plot of Average Steps Per 5-Min Time Interval", subtitle = "Time Intervals: 0 = 12:00-12:05AM to 2355 = 11:55PM - 12:00AM")

#display highest average step time interval
Max5MinSteps <- max(StepsPer5$AverageSteps)
print(Max5MinSteps)
#display interval in which the highest average occurs
max5MinInterval <- StepsPer5[StepsPer5$AverageSteps == Max5MinSteps,]
print(max5MinInterval)

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total 
# number of rows with NAs)

# reread in dataset & count number of missing values
activity2 <- read.csv(file)
count(is.na(activity2$steps))

# percentage of total
percentMissing <- sum(is.na(activity2$steps)) / (sum(!is.na(activity2$steps)) + sum(is.na(activity2$steps)))
print(percentMissing)

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy 
# does not need to be sophisticated. For example, you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.

#replace missing vales with average per 5 min interval (StepsPer5 data frame)
for (i in 1:length(activity2$steps)) {
  if (is.na(activity2$steps[i] == TRUE)) {
  activity2$steps[i] <- StepsPer5$AverageSteps[match(activity2$interval[i], StepsPer5$Interval)]
  }
}

# 3. Create a new dataset that is equal to the original dataset but with the missing data 
# filled in.

str(activity2)
summary(activity2$steps)

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the 
# mean and median total number of steps taken per day. Do these values differ from the estimates 
# from the first part of the assignment? What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?

#create new data frame of total daily steps with average data for missing values
DailySteps3 <- aggregate(activity2$steps, by = list(activity2$date), sum)
names(DailySteps3) <- c("Day", "FullSteps")

# create histogram of daily steps with substituted missing values
p3 <- ggplot(DailySteps3, aes(x = FullSteps)) + geom_histogram(color = "black", fill = "grey", binwidth = 1000) + ggtitle(label = "Total Number of Steps Per Day")

# annotate histogram with mean and median data
p3 <- p3 + geom_vline(aes(xintercept = mean(FullSteps)), color = "red", linetype = "dashed", size = 1) 
p3 <- p3 + geom_vline(aes(xintercept = median(FullSteps)), color = "green", linetype = "dotted", size = 1) 
p3 <- p3 + labs(y = "Frequency (in days)", x = "Total Steps") 

# add subtitle explaining mean and median lines
p3 <- p3 + ggtitle(labs(subtitle = "Simulated Data for Missing Values; Dashed Red Line ~ Average & Dotted Green Line ~ Median"))

#calculate mean and median
meanFullSteps <- mean(DailySteps3$FullSteps)
print(meanFullSteps)
medianFullSteps <- median(DailySteps3$FullSteps)
print(medianFullSteps)

#compare daily step calculations with missing vs simulated data
#the summary below shows the substitutions by interval worked very well, keeping the mean & median the same; the major change only comes in the sum of steps, which rises by 86000 (8 avg days)
DailyComp <- merge(DailySteps, DailySteps3, all.y = TRUE)
summary(DailyComp)
diffSteps <- sum(DailySteps3$FullSteps) - sum(DailySteps$Steps)
print(diffSteps)

# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

#reformat date variable as date (from factor)
activity2$date <- as.Date(as.character(activity2$date))
#append weekday var to data frame
activity2 <- mutate(activity2, Weekday = weekdays(activity2$date))
#adjust weekday variable (as factor) for weekday vs weekend
for (i in 1:length(activity2$steps)) {
  if (activity2$Weekday[i] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    activity2$Weekday[i] <- "Weekday"
  }  else {
    activity2$Weekday[i] <- "Weekend"
  }
}
activity2$Weekday <- as.factor(activity2$Weekday)
  
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:
# create 2 subsets and aggregate by average per interval once again 
weekdaySteps <- subset(activity2, activity2$Weekday == "Weekday")
weekendSteps <- subset(activity2, activity2$Weekday == "Weekend")

StepsPer5Weekday <- aggregate(weekdaySteps$steps, by = list(weekdaySteps$interval), mean)
StepsPer5Weekend <- aggregate(weekendSteps$steps, by = list(weekendSteps$interval), mean)
names(StepsPer5Weekday) <- c("Interval", "AverageWeekdaySteps")
names(StepsPer5Weekend) <- c("Interval", "AverageWeekendSteps")

#new data frame for 5 min intervals by weekday & weekend
StepsPer5Grouped <- merge(StepsPer5Weekday, StepsPer5Weekend)

#create stacked ggplots with labels on only selected charts and consistent y axis limits
p4 <- ggplot(NULL, aes(y = StepsPer5Grouped$AverageWeekdaySteps, x = StepsPer5Grouped$Interval)) 
p4 <- p4 + geom_line() + geom_point() + ylim(0, 235)
p4 <- p4 + ggtitle(labs(y = "Average Number of Steps", x = "")) 
p4 <- p4 + ggtitle(label = "Time Series Plot of Average Steps Per 5-Min Time Interval", subtitle = "Weekday")


p5 <- ggplot(NULL, aes(y = StepsPer5Grouped$AverageWeekendSteps, x = StepsPer5Grouped$Interval)) 
p5 <- p5 + geom_line(color = "grey") + geom_point(color = "purple") + ylim(0, 235) 
p5 <- p5 + ggtitle(labs(y = "", x = "5 Minute Time Interval")) 
p5 <- p5 + ggtitle(subtitle = "Weekend")

grid.arrange(p4, p5, ncol = 1)

#knitr html file
knit2html("RepDataProj1.Rmd")
