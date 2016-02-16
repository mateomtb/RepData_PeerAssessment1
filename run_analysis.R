run_analysis <- function () {
  
  #load the activity data
  activity <- read.csv('activity.csv', sep = ',', header = TRUE)
  #clean up date
  activity$date <- as.Date(activity$date)
  #str(activity)

  #Make a histogram of the total number of steps taken each day
  dailysteps <- aggregate(steps ~ date, data=activity, FUN="sum")
  png(filename='figures/steps.png',width=480,height=480,units='px')
  plot(activity$date,activity$steps,ylab='Number of Steps', xlab="", type="l")
 
  #calculate mean of steps
  step_mean <- mean(dailysteps$steps)
  
  #calculate median of steps
  step_median <- median(dailysteps$steps)
  
  #steps by 5 mintue intervals
  dailysteps_interval <- aggregate(steps ~ interval, data=activity, FUN="sum")
  png(filename='figures/interval.png',width=480,height=480,units='px')
  plot(dailysteps_interval$interval,dailysteps_interval$steps,ylab='Number of Steps', xlab="5 minute intervals", type="l")
  #find the interval with the max number of steps recorded
  max_int <- dailysteps_interval$interval[which.max(dailysteps_interval$steps)]
  #plot interval
  abline(v=(max_int), lty=3, col="blue")
  dev.off() 

}