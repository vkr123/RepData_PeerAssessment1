1) Loading and preprocessing the data
Show any code that is needed to

2) Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis
What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

3) Calculate the total number of steps taken per day
Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day
What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
Imputing missing values
Note that there are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data.

4) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Are there differences in activity patterns between weekdays and weekends?
For this part the \color{red}{\verb|weekdays()|}weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

5) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.




STEP 1
step<-read.csv('activity.csv')
step$date<-as.character(step$date,"%d/%m/%Y")

STEP 2
step_day<-aggregate(step$steps,by=list(step$date), FUN=sum, na.rm=TRUE)
hist(step_day$x, col='red', main='Total number of steps per day', xlab='Number of steps')
mean(step_day$x)
median(step_day$x)

STEP 3
step_avg<-aggregate(step$steps,by=list(step$interval), FUN=mean, na.rm=TRUE)
plot(step_avg$x, col='red', type='l', main='Time Series Plot', ylab='Average number of steps', xlab='5 minute interval')
step_avg[step_avg$x==max(step_avg$x),]$Group.1


STEP 4
sum(is.na(step$steps))
new=step
for (i in 1:nrow(step)){
  if(is.na(step$steps[i])){
    new$steps[i]=step_avg$x[match(step$interval,step_avg$Group.1)[i]]
  }
}
new_day_na<-aggregate(new$steps,by=list(new$date), FUN=sum, na.rm=TRUE)
hist(new_day_na$x, col='red', main='Total number of steps per day', xlab='Number of steps')
mean(new_day_na$x)
median(new_day_na$x)

STEP 5
week<-weekdays(as.Date(new$date), TRUE)
new['daytype']<-week
for (i in 1:nrow(step)){
  if(new$daytype[i]=='Mon' | new$daytype[i]=='Tue' | new$daytype[i]=='Wed' | new$daytype[i]=='Thu' | new$daytype[i]=='Fri'){
    new$daytype[i]="weekday"
  }
  else{
    new$daytype[i]="weekend"
  }
}

library(ggplot2)
new_wday<-aggregate(new[new$daytype=='weekday',]$steps,by=list(new[new$daytype=='weekday',]$interval), FUN=mean, na.rm=TRUE)
new_wday['type']<-'weekday'
new_wend<-aggregate(new[new$daytype=='weekend',]$steps,by=list(new[new$daytype=='weekend',]$interval), FUN=mean, na.rm=TRUE)
new_wend['type']<-'weekend'
final<-rbind(new_wday,new_wend)
final<-transform(final,type=factor(type))
ggplot(data = final, mapping = aes(x = Group.1, y = x)) + geom_line() +
  facet_grid(rows = vars(type))+labs(x='Interval',y='Average number of steps', title = 'Average steps - Weekday/Weekend')

