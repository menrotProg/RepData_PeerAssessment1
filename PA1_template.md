# Reproducible Research: Peer Assessment 1


```r
rm(list=ls())
options(stringsAsFactors = FALSE)

if (!is.element("ggplot2",installed.packages()) ) {install.packages("ggplot2")}
library(ggplot2)

if (!is.element("sqldf",installed.packages()) ) {install.packages("sqldf")}
library(sqldf)
```

```
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
```

```r
if (!is.element("tcltk",installed.packages()) ) {install.packages("tcltk")}
library(tcltk)
```


## Loading and preprocessing the data


```r
if (!file.exists("activity.csv")) {unzip("activity.zip")}

Activity<-read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
totDaily<-tapply(Activity$steps,Activity$date,sum,na.rm=TRUE)
q <- qplot(totDaily, geom="histogram", binwidth = 2200, fill=I("blue"))

print(q)
```

![](PA1_template_files/figure-html/Mean per Day-1.png) 

```r
# Calculate the mean and Median

MeanDailySteps<-mean(totDaily)
MedianDailySteps<-median(totDaily)
```
The mean of the number of steps taken per day (ignoring NA) is 9354.2295082  
The median of the number of steps taken per day (ignoring NA) is 10395  

## What is the average daily activity pattern?

```r
##    plot (i.e. type = "l") of the 5-minute interval (x-axis) 
##    and the average number of steps taken, averaged across all days (y-axis)
##    compute the plot
##

AvgByInterval<-tapply(Activity$steps,as.factor(Activity$interval),mean, na.rm=TRUE)

forPlot <- data.frame(Time=levels(factor(Activity$interval)),AvgSteps=as.numeric(AvgByInterval))

plot (forPlot$Time,forPlot$AvgSteps,type="l",main="Daily Average no.of Steps per Time Interval",
      ylab="Avergae number of Steps",xlab="Time of  Day of the Interval")
```

![](PA1_template_files/figure-html/Daily activity pattern-1.png) 

```r
TimeOfMaxAverage <- forPlot[which.max( forPlot[,2]),1]
```

The time of day of the interval with maximum avergae steps is 835  

## Imputing missing values


```r
NumofNA <- sum(!complete.cases(Activity))
```
total number of missing cases is 2304  


```r
Clean <- Activity

for (i in 1:length(Activity[,1])) {
  if (is.na(Clean[i, "steps"])) {
    Clean[i, "steps"] <- forPlot[match(as.character(Clean[i,"interval"]), forPlot[,"Time"]), "AvgSteps"]
  }
}
  
totDailyClean<-tapply(Clean$steps,Clean$date,sum,na.rm=TRUE)

# Plot a histogram
q <- qplot(totDailyClean, geom="histogram", binwidth = 2200, fill=I("blue"))

print(q)
```

![](PA1_template_files/figure-html/Imputing missing values 2-1.png) 

```r
# Calculate the mean and Median

MeanDailyStepsClean<-mean(totDailyClean)
MedianDailyStepsClean<-median(totDailyClean)
```

The mean of the clean data is 1.0766189\times 10^{4}  
The median of the clean data is 1.0766189\times 10^{4}  





## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Clean$day <- "weekday"
for (i in 1:length(Clean[,1])) {
  if (as.POSIXlt(Clean[i,"date"])$wday==6 | as.POSIXlt(Clean[i,"date"])$wday==0) {
    Clean[i, "day"] <- "weekend"
  }
} 
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
Clean$day <- as.factor(Clean$day)

# Create data frames for each type of day

CleanWday<-sqldf("SELECT * FROM Clean WHERE day = 'weekday'")
CleanWend<-sqldf("SELECT * FROM Clean WHERE day = 'weekend'")


# Calculate the weekday averages
AvgByInterval<-tapply(CleanWday$steps,as.factor(CleanWday$interval),mean, na.rm=TRUE)
forPlotD <- data.frame(Time=levels(factor(CleanWday$interval)),AvgSteps=as.numeric(AvgByInterval), TypeOfDay="weekday")

# Calculate the weekend averages
AvgByInterval<-tapply(CleanWend$steps,as.factor(CleanWend$interval),mean, na.rm=TRUE)
forPlotE <- data.frame(Time=levels(factor(CleanWend$interval)),AvgSteps=as.numeric(AvgByInterval), TypeOfDay="weekend")

# Merge dataframes
forplot <- rbind(forPlotD, forPlotE)

# Covert the times from char to numeric value
forplot$Time <- as.numeric(forplot$Time)


## Plot multifaceted plot
 
facets <- ggplot(forplot,aes(Time, AvgSteps ))
facets <- facets + geom_line(aes(group=TypeOfDay, color = TypeOfDay))
facets <- facets + facet_grid(TypeOfDay~.)
facets <- facets + labs(y="Average Number of Steps", x="Time of Day (in 5 min intervals)", 
                       title=("activity patterns between weekdays and weekends") )

print (facets)
```

![](PA1_template_files/figure-html/plot averaged across all weekday days or weekend days-1.png) 

