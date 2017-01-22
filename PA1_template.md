Untitled
================

Peer-graded Assignment: Course Project 1
========================================

Loading and preprocessing the data
----------------------------------

Show any code that is needed to

1.  The data (i.e. read.csv())
2.  Process/transform the data (if necessary) into a format suitable for your analysis

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
setwd("C://Users//user//Dropbox//Coursera//RepData_PeerAssessment1//data")
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in the dataset.
1. Calculate the total number of steps taken per day

``` r
totalsteps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
head(totalsteps)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

1.  If you do not understand the difference between a histogram and a barplot, research the difference between them.
    Make a histogram of the total number of steps taken each day

``` r
hist(totalsteps$steps, xlab = "steps", breaks = 16)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day

``` r
mean(totalsteps$steps)
```

    ## [1] 10766.19

``` r
median(totalsteps$steps)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = "1") of the 5 minute interval (x-axis) and and the average number of steps taken, averaged across all days (y-axis)

``` r
average <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
plot(average$interval, average$steps, type = "l", lwd = 2,
     xlab = "5min interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
average$interval[which.max(average$steps)]
```

    ## [1] 835

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

``` r
sum(is.na(data))
```

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
activity <- data
for (i in average$interval) {
    activity[activity$interval == i & is.na(activity$steps),]$steps <- average$steps[average$interval == i]
}
any(is.na(activity$steps))
```

    ## [1] FALSE

1.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
totalactivity <- aggregate(steps~date, data = activity, sum, na.rm = TRUE)
```

1.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
hist(totalactivity$steps, breaks = 16, xlab = "steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
mean(totalactivity$steps)
```

    ## [1] 10766.19

``` r
median(totalactivity$steps)
```

    ## [1] 10766.19

The mean does not deviate, but the median does, now being the same as the mean. Because the NA's are now based on means, it makes sense for the values to be closer to the mean and deviating less than before. The frequency of the values is now also higher, as a result from adding more values rather than removing then like before.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1. Create a new factor variable in the dataset with two levels: 'weekday' and 'weekend' indicating whether a given date is a weekday or weekend day.

``` r
activity$day <- weekdays(activity$date)
activity$week <- ''
activity[activity$day == "Saturday" | activity$day == "Sunday", ]$week <- "weekend"
activity[activity$day == "Monday" | activity$day == "Tuesday" | activity$day == "Wednesday" | activity$day == "Thursday" | activity$day == "Friday", ]$week <- "weekday"
activity$week <- factor(activity$week)
```

1.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
library(lattice)
average_activity <- aggregate(steps ~interval + week, data = activity, mean)
xyplot(steps ~interval | week, data = average_activity, type = "l", layout = c(2,1))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-12-1.png)
