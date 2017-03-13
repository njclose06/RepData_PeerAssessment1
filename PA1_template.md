---
title: "Reproducible Data"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Assignment with description and code in one document using the provided Activity monitoring data.



## Loading and preprocessing the data

Unzip the Activity file

```{r}
library(dplyr)
unzip(zipfile = "activity.zip")

```

Load the data

```{r}
ActivityData <- read.csv(file="activity.csv", header=TRUE)
```




## What is mean total number of steps taken per day?

Create data set which aggregates the total number of steps for each day.

```{r}
subDat3 <- aggregate(steps ~ date, data = ActivityData, sum)
```

Make a histogram of the total number of steps taken each day

```{r echo=FALSE}
hist(subDat3$steps, xlab="Number of steps", main = "Total number of steps taken each day")
```

Calculate the mean and median total number of steps taken per day

```{r}
meanst <- summarize(subDat3, mean(steps))
medianst <- summarize(subDat3,median(steps))

```
The mean is `r meanst` and the median is `r medianst`.



## What is the average daily activity pattern?

Create data set that finds the average number of steps per dAY.

```{r}
subDat4 <- aggregate(steps ~ interval, data = ActivityData, mean)
```

Create a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.

```{r echo=FALSE}
plot(subDat4$interval,subDat4$steps,type="l",xlab="Interval", ylab="Average Steps", main = "Average Steps per Interval")
```

Identify 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

```{r}
subDat4[which(subDat4$steps == max(subDat4$steps, na.rm = TRUE)), ]
maxrw <- subDat4$interval[104]

```
The `r maxrw` interval contains the maximum number of steps.




## Imputing missing values

1. Report the total number of missing values in the dataset

```{r}
miss <- sum(is.na(ActivityData))
```

The total number of missing values is `r miss`.

2. Replace missing values with average steps per interval and create a new dataset with the missing data filled in.

```{r}
NewDataSet <- ActivityData

for (i in 1:nrow(NewDataSet)) {
  if (is.na(NewDataSet$steps[i])) {
    # Find the index value for when the interval matches the average
    itver <- which(NewDataSet$interval[i] == subDat4$interval)
    # Assign the value to replace the NA
    NewDataSet$steps[i] <- subDat4[itver,]$steps
  }
}
```

Create data set that sums the number of steps taken per day in data set with filled in values. 

```{r}
subfillDat3 <- aggregate(steps ~ date, data = NewDataSet, sum)
```

Histogram of the total number of steps taken each day using new data set.
```{r echo=FALSE}
hist(subfillDat3$steps, xlab="Number of steps", main = "Total number of steps taken each day")
```

The mean and median total number of steps taken per day
```{r}
meanfillst <- summarize(subfillDat3, mean(steps))
medianfillst <- summarize(subfillDat3,median(steps))
```

The mean is `r meanfillst` and the median is `r medianfillst`.

The impact of replacing the missing values with the mean is negligible, there is a plus 1 increase. 




## Are there differences in activity patterns between weekdays and weekends?

1. New factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
NewDataSet$date <- as.Date(NewDataSet$date)
#create a vector of weekdays
wkdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
NewDataSet$wDay <- factor((weekdays(NewDataSet$date) %in% wkdays), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

subNewDat4 <- subset(NewDataSet,wDay=="weekday")
subNewDat4wken <- subset(NewDataSet,wDay=="weekend")

AvgsubNewDat4 <-aggregate(steps ~ interval, data = subNewDat4, mean)
AvgsubNewDat4wken <-aggregate(steps ~ interval, data = subNewDat4wken, mean)
```


Plots containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days. 


```{r echo=FALSE}
par(mfrow=c(2,1), mar=c(2,2,2,1)) 

plot(AvgsubNewDat4$interval,AvgsubNewDat4$steps,type="l",ylim = c(0,250), xlab="Interval", ylab="Average Steps", main = "Weekday")
plot(AvgsubNewDat4wken$interval,AvgsubNewDat4wken$steps,type="l",ylim = c(0,250), xlab="Interval", ylab="Average Steps", main = "Weekend")
```

Weekend activity differs from weekday in that the step activity starts in later intervals.
