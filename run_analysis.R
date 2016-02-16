run_analysis <- function () {
  
  #load the activity data
  activity <- read.csv('activity.csv', sep = ',', header = TRUE)
  #clean up date
  activity$date <- as.Date(activity$date)
  #str(activity)

  #Make a histogram of the total number of steps taken each day
  dailysteps <- aggregate(steps ~ date, data=activity, FUN="sum")
  png(filename='figures/steps_1.png',width=480,height=480,units='px')
  plot(activity$date,activity$steps,ylab='Number of Steps', xlab="", type="l")
  dev.off()
  #calculate mean of steps
  step_mean <- mean(dailysteps$steps)
  
  #calculate median of steps
  step_median <- median(dailysteps$steps)
  
  #steps by 5 mintue intervals
  dailysteps_interval <- aggregate(steps ~ interval, data=activity, FUN="sum")
  png(filename='figures/interval.png',width=480,height=480,units='px')
  plot(dailysteps_interval$interval,dailysteps_interval$steps,ylab='Number of Steps', xlab="5 minute intervals", type="l")
  #find the interval with the max number of steps recorded
  max_interval <- dailysteps_interval$interval[which.max(dailysteps_interval$steps)]
  #plot the interval to the max!
  abline(v=(max_interval), lty=3, col="blue")
  dev.off() 
  
  #total number of rows with na's
  total_na <- sum(is.na(activity))
  
  #new data with imputed values using the mean
  activity_new <- merge(activity, dailysteps, by="date", suffixes=c("", ".mean"))
  nas <- is.na(activity_new$steps)
  activity_new$steps[nas] <- activity_new$steps.mean[nas]
  activity_new <- activity_new[, c(1:3)]

  #plot new data
  dailysteps_new <- aggregate(steps ~ date, data=activity_new, FUN="sum")
  png(filename='figures/steps_new.png',width=480,height=480,units='px')
  plot(activity_new$date,activity_new$steps,ylab='Number of Steps', xlab="", type="l")
  dev.off()
  
  #calculate mean of steps with new data
  step_mean <- mean(dailysteps_new$steps)
  #10766.19
  #calculate median of steps with new data
  step_median <- median(dailysteps_new$steps)
  #10765
  #imputing data did not appear to change the results
  
  
  # functin to identify weekend or weekday
  daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
      "weekend"
    } else {
      "weekday"
    }
  }
  # convert daytime from character to factor
  activity_new$daytype <- as.factor(sapply(activity_new$date, daytype))
  
  # aggreagate new data and include weekend or weekday value based on mean
  dailysteps_new <- aggregate(steps ~ interval + daytype, activity_new, mean)
  library(lattice)
  
  #2 plots showing average from weekend and weekday
  dayofweek <- xyplot(steps ~ interval | daytype, data=dailysteps_new, layout=c(2,1), type='l')
  trellis.device(device="png", filename='figures/weekend_weekday.png')
  print(dayofweek)
  dev.off()

}