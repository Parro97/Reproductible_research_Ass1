library(knitr)
library(dplyr)
library(ggplot2)
setwd("C:\\Users\\crist\\OneDrive\\Escritorio\\Curso R\\Coursera\\Reproductible research\\Ass 1")
#Preprocessing
data_row <- read.csv('activity.csv')
data <- data_row[ with (data_row, { !(is.na(steps)) } ), ]
#Mean total number of steps taken per day
by_day <- group_by(data, date)
steps_by_day <- summarise(by_day, total = sum(steps))
hist(steps_by_day$total, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
summary(steps_by_day)

#Average daily activity pattern
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")

max_steps_row <- which.max(steps_by_interval$steps)
steps_by_interval[max_steps_row, ]

#Imputing missing values

sum(is.na(data_row))
data_imputed <- data_row
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- steps_by_interval[
      steps_by_interval$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}
df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)", 
     xlab="Total number of steps in a day")

mean(df_imputed_steps_by_day$steps)
median(df_imputed_steps_by_day$steps)
mean(steps_by_day$total)
median(steps_by_day$total)


#Are there differences in activity patterns between weekdays and weekends?

data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"

data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

df_imputed_steps_by_interval <- aggregate(steps ~ interval + type_of_day, data_imputed, mean)

qplot(interval, 
      steps, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)

