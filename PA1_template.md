---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data




```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv', header = TRUE)
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
total.steps <- aggregate(steps ~ date, data = activityData, FUN = sum)
barplot(total.steps$steps, xlab="Steps by day", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Calculate and report the ***mean*** and ***median*** total number of steps taken per day

```r
mean(total.steps$steps)
```

```
## [1] 10766.19
```

```r
median(total.steps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps.interval <- aggregate(steps ~ interval, data=activityData, FUN=mean)
plot(steps.interval, type="l", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interval[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e.  the total number of rows with NAs)

```r
sum(is.na(activityData[,]))
```

```
## [1] 2304
```

2. We will use the mean for each day to fill the missing values in original dataset.

3. Create a new dataset that is equal to the original dataset but with the  missing data filled in.

```r
activityData <- merge(activityData, total.steps, by="date", suffixes=c("",".y"))
empty <- is.na(activityData$steps)
activityData$steps[empty] <- activityData$steps.y[empty]
activityData <- activityData[ ,1:3]
```

4. Check result

```r
sum(is.na(activityData[,]))
```

```
## [1] 0
```

5. Make a histogram of the total number of steps taken each day and Calculate 
and report the mean and median total number of steps taken per day. 

```r
total.steps <- aggregate(steps ~ date, data=activityData, FUN=sum)
barplot(total.steps$steps, xlab="Steps by day", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(total.steps$steps)
```

```
## [1] 10766.19
```

```r
median(total.steps$steps)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a working or weekend day.

```r
GetDay <- function(date) {
    # Or "суббота", "воскресенье" - for russian locale
    day <- weekdays(as.Date(date)) %in% c("Saturday", "Sunday") 
}
activityData$IsWeekend <- as.factor(sapply(activityData$date, GetDay))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged  across all weekday days or weekend days (y-axis). 

```r
IsWeekend <- function(boolVal){
    if(boolVal){"Weekend days"}
    else {"Working days"}
}
par(mfrow=c(2,1))
for (bool in c(TRUE, FALSE)) {
    steps.bool <- aggregate(steps ~ interval, data = activityData, subset = activityData$IsWeekend == bool, FUN = mean)
    plot(steps.bool, type="l", main = IsWeekend(bool))
}
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
