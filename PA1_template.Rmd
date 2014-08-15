---
title: "Reproducible Research: Peer Assessment 1"
author: "Chris Ramsey"
date: "Friday, August 15, 2014"
output: html_document
---
### Loading and processing the data
```{r}
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"));
activity$month <- as.numeric(format(data$date,"%m"));
activityComplete <-na.omit(activity);
library(ggplot2)
```

### What is mean total number of steps taken per day?
* Histogram of the total number of steps/day
```{r}
#create histogram
ggplot(activityComplete, aes(date, steps)) + geom_bar(stat = "identity", colour = "#663399", fill = "#663399", width = .8) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps");
```

* Calculate/report mean and median of steps/day
Mean total number of steps taken per day:
```{r}
activityTotalSteps <- aggregate(activityComplete$steps, list(Date = activityComplete$date), FUN = "sum")$x;
mean(activityTotalSteps);
```

Median total number of steps taken per day:
```{r}
median(activityTotalSteps);
```

### Average daily activity pattern
* Create time series plot of 5 min interval - avg # of steps taken averaged across all days
```{r}
activityAvgSteps <- aggregate(activityComplete$steps, list(interval = as.numeric(as.character(activityComplete$interval))), FUN = "mean");
names(activityAvgSteps)[2] <- "stepmean";
ggplot(activityAvgSteps, aes(interval, stepmean)) + geom_line(color = "#663399", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken");
```
* Which 5 min interval contains the maximum number of steps?
```{r}
activityAvgSteps[activityAvgSteps$stepmean == max(activityAvgSteps$stepmean), ];
```
### Imputing missing values
* Total number of rows with NA
```{r}
sum(is.na(activity))
```

* Fill missing datas in dataset.
I am going to use values from 5 min interval above.
```{r}
filledActData <- activity 
for (i in 1:nrow(filledActData)) {
  if (is.na(filledActData$steps[i])) {
    filledActData$steps[i] <- activityAvgSteps[which(filledActData$interval[i] == activityAvgSteps$interval), ]$stepmean
  }
}
head(filledActData)
sum(is.na(filledActData))
```

*Histogram of total number of steps taken with new data
```{r}
ggplot(filledActData, aes(date, steps)) + geom_bar(stat = "identity", colour = "#663399", fill = "#663399", width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps");
```

Mean total steps/day:
```{r}
newTotSteps <- aggregate(filledActData$steps, list(Date = filledActData$date), FUN = "sum")$x;
newMean <- mean(newTotSteps);
newMean
```

Median total steps/day:
```{r}
newMedian <- median(newTotSteps);
newMedian
```

Compare the new/old versians of mean and medians prior to imputting missing data:
```{r}
oldMean <- mean(activityTotalSteps);
oldMedian <- median(activityTotalSteps);
newMean-oldMean;
newMedian-oldMedian;
```

New factors weekend and weekday:
### Differences in activity patterns between weekdays and weekends
* Create new variables ("weekday" and "weekend").
```{r}
head(filledActData)
filledActData$weekdays <- factor(format(filledActData$date, "%A"))
levels(filledActData$weekdays)
levels(filledActData$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(filledActData$weekdays)
table(filledActData$weekdays)
```
* Panel Plot containing time series plot & avg # steps averaged across all weekdays or weekend days
```{r}
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
