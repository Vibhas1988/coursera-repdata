## ----------- loading data -----------
activity<-read.csv(file='C:/Users/vibhas/R/activity.csv', sep=',' ,header=TRUE)


##----------- Mean total number of steps taken each day------------
library(ggplot2)

# Total no. of steps
tapply(activity$steps,activity$date,FUN=sum,na.rm=TRUE)->activity_msteps

# Plot of no. of steps
qplot(activity_msteps, binwidth=1000, xlab="total number of steps taken each day")

# Mean of steps taken each day
mean(activity_msteps,na.rm=TRUE) 

# Median of steps taken each day
median(activity_msteps,na.rm=TRUE) 


##-----------Avg daily activity pattern-------------------
library(ggplot2)

# Time series plot
activity_avg <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
 FUN=mean, na.rm=TRUE)
ggplot(data=activity_avg, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") 
  
  # Max no of steps
  activity_avg[which.max(activity_avg$steps),] 
  
  
##---------Imputing missing values-------------
missing <- is.na(activity$steps)

# How many missing
table(missing)

# Filling the missing values
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (activity_avg[activity_avg$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

# Plot of the steps taken
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


##-----------Activity pattern difference between weekdays and weekends----------
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

# Panel plot
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")





