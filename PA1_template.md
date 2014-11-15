---
title: "Reproducable Research"
author: "Bluebonobo"
date: "Saturday, November 08, 2014"
output: html_document
---
 
 
####Loading and preprocessing the data
>1. Load the data (i.e. read.csv())
>2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
setwd(".")
activitytable <- read.csv(file="activity.csv",head=TRUE,sep=",")
```

####What is mean total number of steps taken per day?
>1. For this part of the assignment, you can ignore the missing values in the dataset.
>Make a histogram of the total number of steps taken each day

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

>2. Calculate and report the mean and median total number of steps taken per day

```r
agg_date_mean <- mean(agg_date$x)
print(agg_date_mean)
```

```
## [1] 9354.23
```

```r
agg_date_median <- as.numeric(median(agg_date$x))
print(agg_date_median)
```

```
## [1] 10395
```

####What is the average daily activity pattern?
>1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
agg_interval <- aggregate(activitytable$steps, by=list(interval=activitytable$interval), FUN=mean, na.rm=TRUE)
colnames(agg_interval)[match("x",colnames(agg_interval))] <- "steps"
plot(agg_interval$interval, agg_interval$steps, type="l", col="blue", xlab="Interval", ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

>2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval<-agg_interval[which.max(agg_interval[,2]),1]
print(max_interval)
```

```
## [1] 835
```



####Imputing missing values
>1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
rm_activitytable <- activitytable[complete.cases(activitytable),]
dim(activitytable)[1] - dim(rm_activitytable)[1]
```

```
## [1] 2304
```

This is in line with the number of NAs reported when running the summary function

```r
summary(activitytable)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


>2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The summary function call indicates that NAs are only found in the Steps column (i.e. 2304 NAs steps)
We chose to replace the NAs with the mean of the number of steps per interval across the entire dataset.

>3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
>4. Make a histogram of the total number of steps taken each day 


```r
noNA_activitytable<- activitytable
noNA_activitytable$steps[is.na(noNA_activitytable$steps)] <- mean(noNA_activitytable$steps, na.rm=TRUE)
noNA_agg_date <- aggregate(noNA_activitytable$steps, by=list(noNA_activitytable$date), FUN=sum, na.rm=TRUE)
hist(noNA_agg_date$x, main="Frequency Steps/Day with missing data filled in",  xlab="", ylab="", col="grey")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

> 4. and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
noNA_agg_date_mean <- mean(noNA_agg_date$x)
print(noNA_agg_date_mean)
```

```
## [1] 10766.19
```

```r
noNA_agg_date_median <- as.numeric(median(noNA_agg_date$x))
print(noNA_agg_date_median)
```

```
## [1] 10766.19
```

#### Are there differences in activity patterns between weekdays and weekends?

> 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
> 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
noNA_activitytable$day <- weekdays(as.POSIXlt(noNA_activitytable$date))
noNA_activitytable$day <- ifelse(noNA_activitytable$day %in% c("Saturday", "Sunday"), "weekend", "weekday")
noNA_activitytable$day <- as.factor(noNA_activitytable$day)
summary(noNA_activitytable)
```

```
##      steps                date          interval           day       
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0   weekday:12960  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8   weekend: 4608  
##  Median :  0.00   2012-10-03:  288   Median :1177.5                  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5                  
##  3rd Qu.: 37.38   2012-10-05:  288   3rd Qu.:1766.2                  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0                  
##                   (Other)   :15840
```

```r
noNA_agg_interval <- aggregate(noNA_activitytable$steps, by=list(interval=noNA_activitytable$interval, day=noNA_activitytable$day), FUN=mean, na.rm=TRUE)
colnames(noNA_agg_interval)[match("x",colnames(noNA_agg_interval))] <- "steps"

library(lattice)
xyplot(noNA_agg_interval$steps ~ noNA_agg_interval$interval | noNA_agg_interval$day, layout = c(1, 2), type = "l", xlab="interval", ylab="Number of steps") 
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
