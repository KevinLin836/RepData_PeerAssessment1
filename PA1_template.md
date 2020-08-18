---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
To load the data in activity.csv file, we check if the file exists in the directory. If not, we unzip the activity.zip folder and data from the csv file into the global environment. 


```r
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
total_steps <- data %>% 
    group_by(date) %>%
    summarize(steps = sum(steps, na.rm = T))    
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(total_steps$steps, xlab = "Total number of steps", main = "Histogram of total number of steps taken per day")  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

To calculate the mean and median total number of steps taken per day:  

```r
mean_steps <- mean(total_steps$steps, na.rm = T)
median_steps <- median(total_steps$steps, na.rm = T)
```
The mean total number of steps is **9354.2295082**. The median total number of steps is **10395**.  

## What is the average daily activity pattern?  

Looking at the step interval vs. the average number of steps taken:  

```r
library(ggplot2)
mean_steps <- data %>%
    group_by(interval) %>%
    summarize(steps = mean(steps, na.rm = T))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(mean_steps, aes(interval, steps)) + geom_line() + ggtitle("Average Daily Activity Pattern") + xlab("5 Min Interval") + ylab("Average Steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

To determine the 5-minute interval that contains the maximum number of steps:  

```r
max_int <- mean_steps$interval[mean_steps$steps == max(mean_steps$steps)]
```
The 5-minute interval that contains the maximum number of steps is **835**.  
## Imputing missing values

```r
num_NA <- table(is.na(data$steps))
```

There are **2304** missing values. To fill all of the missing values in the dataset, the mean value of that 5 min interval will be used as a replacement.

```r
transformed_data <- data
for (i in seq_along(data$steps)){
    if (is.na(data$steps[i])){
        transformed_data$steps[i] <- mean_steps$steps[mean_steps$interval == data$interval[i]]
    }
}
```

A new dataset, *transformed_data*, with all of the missing values filled in is created. We display the first few rows of the new dataset.  

```r
head(transformed_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Comparing the new dataset to the old:  

```r
new_total_steps <- transformed_data %>% 
    group_by(date) %>%
    summarize(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(new_total_steps$steps, xlab = "Total number of steps", main = "Histogram of total number of steps taken per day")  
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

With the *NA* values filled in, the histogram plot changed. To calculate the mean and median total number of steps taken per day:  

```r
new_mean_steps <- mean(new_total_steps$steps)
new_median_steps <- median(new_total_steps$steps)
```

The mean total number of steps of the new datatset is **10766.19**. The median total number of steps is **10766.19**. Imputing missing data increased the mean and median total number of steps. 

## Are there differences in activity patterns between weekdays and weekends?

To compare the activity patterns between weekdays and weekends:  

```r
transformed_data$date <- as.POSIXct(transformed_data$date, format="%Y-%m-%d")
new_data <- transformed_data %>%
    mutate(day = weekdays(date), daytype = day) 
for (i in seq_along(new_data$day)){
    if (new_data$day[i] %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')){
        new_data$daytype[i] <- 'Weekday'
    }
    else if (new_data$day[i] %in% c('Saturday', 'Sunday')){
        new_data$daytype[i] <- "Weekend"
    }
}
head(new_data)
```

```
##       steps       date interval    day daytype
## 1 1.7169811 2012-10-01        0 Monday Weekday
## 2 0.3396226 2012-10-01        5 Monday Weekday
## 3 0.1320755 2012-10-01       10 Monday Weekday
## 4 0.1509434 2012-10-01       15 Monday Weekday
## 5 0.0754717 2012-10-01       20 Monday Weekday
## 6 2.0943396 2012-10-01       25 Monday Weekday
```

```r
new_plot_data <- new_data %>% 
    group_by(interval, daytype) %>%
    summarize(steps = mean(steps, na.rm = T))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
head(new_plot_data)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [3]
##   interval daytype  steps
##      <int> <chr>    <dbl>
## 1        0 Weekday 2.25  
## 2        0 Weekend 0.215 
## 3        5 Weekday 0.445 
## 4        5 Weekend 0.0425
## 5       10 Weekday 0.173 
## 6       10 Weekend 0.0165
```

```r
ggplot(new_plot_data, aes(interval, steps)) + facet_wrap(. ~ new_plot_data$daytype, nrow = 2, ncol = 1) + geom_line() + ggtitle("Average Daily Activity Pattern (Weekday vs Weekend)") + xlab("5 Min Interval") + ylab("Average Steps Taken")  
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
