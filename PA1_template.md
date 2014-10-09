# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv('activity.csv')
```



## What is mean total number of steps taken per day?

```r
totaldailysteps<-tapply(activity$steps,activity$date,sum)
hist(totaldailysteps)
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 


```r
mean(totaldailysteps,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(totaldailysteps,na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
activityNoNa <- subset(activity, !is.na(steps))
intervals <- unique(activityNoNa$interval)
meanstepsperinterval<-tapply(activityNoNa$steps,activityNoNa$interval,mean)
plot(intervals,meanstepsperinterval,type="l")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 



```r
indexmaximum <- which(meanstepsperinterval==max(meanstepsperinterval),arr.ind = TRUE)[1]
intervalmax <- intervals[indexmaximum]
intervalmax
```

```
## [1] 835
```

## Imputing missing values

```r
TotalNas <- (dim(activity)[1]-dim(activityNoNa)[1])
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
#impute missing values with average of interval
#create new data frame with imputed missing values
```


```r
#change to imputed values
totaldailysteps<-tapply(activity$steps,activity$date,sum)
hist(totaldailysteps)
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 


```r
mean(totaldailysteps,na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(totaldailysteps,na.rm = TRUE)
```

```
## [1] 10765
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#create categorical variable
activity$wday<-weekdays(as.Date(activity$date))
activity$wday[activity$wday=="Sunday" | activity$wday=="Saturday"]<-"weekend"
activity$wday[activity$wday!="weekend"]<-"weekday"
```


```r
#create plot
```
