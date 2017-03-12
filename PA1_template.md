First we load R packages to be used
-----------------------------------

    library(scales)
    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

    library(ggplot2)

Read the csv file "activity"
----------------------------

    data_act <- read.csv('activity.csv')

What is mean total number of steps taken per day?
-------------------------------------------------

    totaldailysteps <-  tapply(data_act$steps, data_act$date, sum, na.rm=TRUE)

### Make a histogram of the total number of steps taken each day

    qplot(totaldailysteps, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

     meantotaldailysteps <- mean(totaldailysteps, na.rm = TRUE)
    meantotaldailysteps

    ## [1] 9354.23

    mediantotaldailysteps <- median(totaldailysteps, na.rm = TRUE)
    mediantotaldailysteps

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    averagenumsteps <- aggregate(x=list(ave_steps=data_act$steps), by=list(interval=data_act$interval), FUN=mean, na.rm=TRUE)
    ggplot(data=averagenumsteps, aes(x=interval, y=ave_steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken") 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    maxsteps <- which.max(averagenumsteps$ave_steps)
    maxsteps

    ## [1] 104

    maxstepstime <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averagenumsteps[maxsteps,'interval'])
    maxstepstime

    ## [1] "8:35"

Imputing missing values
-----------------------

### 1.Number of missing values

    numofnas <- length(which(is.na(data_act$steps)))
    numofnas

    ## [1] 2304

### 2. Devise a strategy for filling in all of the missing values in the dataset.

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

    imputedactdata <- data_act
    imputedactdata$steps <- impute(data_act$steps, fun=mean)

Make a histogram of the total number of steps taken each day
------------------------------------------------------------

    imputedtotaldailysteps <- tapply(imputedactdata$steps, imputedactdata$date, sum)
    qplot(imputedtotaldailysteps, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

#### Calculate and report the mean and median total number of steps taken per day.

    meanimputedtotaldailysteps <- mean(imputedtotaldailysteps)
    meanimputedtotaldailysteps

    ## [1] 10766.19

    medianimputedtotaldailysteps <- median(imputedtotaldailysteps)
    medianimputedtotaldailysteps

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    imputedactdata$dateType <-  ifelse(as.POSIXlt(imputedactdata$date)$wday %in% c(0,6), 'weekend', 'weekday')

### Make a panel plot containing a time series plot

    meanimputedact_data <- aggregate(steps ~ interval + dateType, data=imputedactdata, mean)
    ggplot(meanimputedact_data, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(dateType ~ .) +
        xlab("5-minute interval") + 
        ylab("avarage number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
