# Reproducible Research: Peer Assessment 1

library(ggplot2)

## Loading and preprocessing the data


```r
library(ggplot2)

dat<-read.csv("activity.csv")

dattime<-strptime(dat$date,"%Y-%m-%d")
dat<-cbind(dat,dattime)
```

## What is mean total number of steps taken per day?


```r
dat_agg<-aggregate(dat$steps,list(date=dat$dattime),sum)

names(dat_agg)[2]<-"Number_steps"
p<-ggplot(dat_agg,aes(x=date,y=Number_steps))+geom_histogram(stat="identity")
p+ggtitle("Plot of Number of steps per day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
steps_mean<-mean(dat_agg$Number_steps,na.rm=TRUE)
steps_median<-median(dat_agg$Number_steps,na.rm=TRUE)
```

The mean number of steps per day is 1.0766 &times; 10<sup>4</sup>. The median number of step per day is 10765.


## What is the average daily activity pattern?


```r
dat_agg_5min<-aggregate(dat$steps,list(interval=dat$interval),mean,na.rm=TRUE)
names(dat_agg_5min)[2]<-"Avg"
p1<-ggplot(dat_agg_5min,aes(x=interval,y=Avg))+geom_line()
p1<-p1+ggtitle("Mean number of steps per 5 min interval accross all days")
p1+labs(x="Five min Interval",y="Average number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
p1_max<-dat_agg_5min[dat_agg_5min$Avg==max(dat_agg_5min$Avg),1]
```

The 5 Min interval on average across all the days in the dataset, that contains the maximum number of steps is 835

## Imputing missing values

```r
miss_row<-sum(ifelse(is.na(dat$steps),1,0))

dat1<-dat

dat1$steps[is.na(dat1$steps)]<-0

dat_agg<-aggregate(dat1$steps,list(date=dat$dattime),sum)
names(dat_agg)[2]<-"Number_steps"
p<-ggplot(dat_agg,aes(x=date,y=Number_steps))+geom_histogram(stat="identity")
p+ggtitle("Plot of Number of steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
steps_mean1<-mean(dat_agg$Number_steps,na.rm=TRUE)
steps_median1<-median(dat_agg$Number_steps,na.rm=TRUE)
```
 
The number of rows with missing values (NAs) is 2304

The mean number of steps per day is 9354.2295. The median number of step per day is 1.0395 &times; 10<sup>4</sup>.


## Are there differences in activity patterns between weekdays and weekends?


```r
dat1$week<-ifelse(weekdays(dat1$dattime) %in% c('Saturday','Sunday'), 'weekend','weekday')

dat_agg_week<-aggregate(dat1$steps,list(interval=dat1$interval,week=dat1$week),mean,na.rm=TRUE)
names(dat_agg_week)[3]<-"Avg"
p3<-ggplot(dat_agg_week,aes(x=interval,y=Avg))+geom_line()
p3<-p3+facet_wrap(~week,nrow=2)+ggtitle("Mean number of steps per 5 min interval accross Weekdays and Weekends")
p3+labs(x="Five min Interval",y="Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



