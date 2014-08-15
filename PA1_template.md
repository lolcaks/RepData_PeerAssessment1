---
title: 'Reproducible Research: Peer Assessment 1'
author: "Chris Ramsey"
date: "Friday, August 15, 2014"
output: html_document
---

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
```

### Loading and processing the data

```r
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"));
activity$month <- as.numeric(format(data$date,"%m"));
```

```
## Error: object of type 'closure' is not subsettable
```

```r
activityComplete <-na.omit(activity);
library(ggplot2)
```

```
## Error: there is no package called 'ggplot2'
```

### What is mean total number of steps taken per day?
* Histogram of the total number of steps/day

```r
#create histogram
ggplot(activityComplete, aes(date, steps)) + geom_bar(stat = "identity", colour = "#663399", fill = "#663399", width = .8) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps");
```

```
## Error: could not find function "ggplot"
```

* Calculate/report mean and median of steps/day
Mean total number of steps taken per day:

```r
activityTotalSteps <- aggregate(activityComplete$steps, list(Date = activityComplete$date), FUN = "sum")$x;
mean(activityTotalSteps);
```

```
## [1] 10766
```

Median total number of steps taken per day:

```r
median(activityTotalSteps);
```

```
## [1] 10765
```

### Average daily activity pattern
* Create time series plot of 5 min interval - avg # of steps taken averaged across all days

```r
activityAvgSteps <- aggregate(activityComplete$steps, list(interval = as.numeric(as.character(activityComplete$interval))), FUN = "mean");
names(activityAvgSteps)[2] <- "stepmean";
ggplot(activityAvgSteps, aes(interval, stepmean)) + geom_line(color = "#663399", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken");
```

```
## Error: could not find function "ggplot"
```
* Which 5 min interval contains the maximum number of steps?

```r
activityAvgSteps[activityAvgSteps$stepmean == max(activityAvgSteps$stepmean), ];
```

```
##     interval stepmean
## 104      835    206.2
```
### Imputing missing values
* Total number of rows with NA

```r
sum(is.na(activity))
```

```
## [1] 2304
```

* Fill missing datas in dataset.
I am going to use values from 5 min interval above.

```r
filledActData <- activity 
for (i in 1:nrow(filledActData)) {
  if (is.na(filledActData$steps[i])) {
    filledActData$steps[i] <- activityAvgSteps[which(filledActData$interval[i] == activityAvgSteps$interval), ]$stepmean
  }
}
head(filledActData)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

```r
sum(is.na(filledActData))
```

```
## [1] 0
```

*Histogram of total number of steps taken with new data

```r
ggplot(filledActData, aes(date, steps)) + geom_bar(stat = "identity", colour = "#663399", fill = "#663399", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps");
```

```
## Error: could not find function "ggplot"
```

Mean total steps/day:

```r
newTotSteps <- aggregate(filledActData$steps, list(Date = filledActData$date), FUN = "sum")$x;
newMean <- mean(newTotSteps);
newMean
```

```
## [1] 10766
```

Median total steps/day:

```r
newMedian <- median(newTotSteps);
newMedian
```

```
## [1] 10766
```

Compare the new/old versians of mean and medians prior to imputting missing data:

```r
oldMean <- mean(activityTotalSteps);
oldMedian <- median(activityTotalSteps);
newMean-oldMean;
```

```
## [1] 0
```

```r
newMedian-oldMedian;
```

```
## [1] 1.189
```

New factors weekend and weekday:
### Differences in activity patterns between weekdays and weekends
* Create new variables ("weekday" and "weekend").

```r
head(filledActData)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

```r
filledActData$weekdays <- factor(format(filledActData$date, "%A"))
levels(filledActData$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(filledActData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(filledActData$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(filledActData$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```
* Panel Plot containing time series plot & avg # steps averaged across all weekdays or weekend days

```r
averageSteps <- aggregate(filledActData$steps, 
                      list(interval = as.numeric(as.character(filledActData$interval)), 
                           weekdays = filledActData$weekdays),
                      FUN = "mean")
names(averageSteps)[3] <- "meanOfSteps"
library(lattice)
xyplot(averageSteps$meanOfSteps ~ averageSteps$interval | averageSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 
