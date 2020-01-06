#
# Copyright 2017 Dave Langer
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 


#
# This R source code file corresponds to video 2 of the YouTube series
# "Introduction to Time Series Forecasting with R" located at the 
# following URL:
#     https://youtu.be/7aMJL6odSkQ     
#


# install.packages(c("xts", "lubridate", "ggplot2", "forecast"))
# Load packages
library(xts)
library(lubridate)
library(ggplot2)
library(forecast)
library(data.table)
library(tidyverse)

rm(list=ls())
#
# Video 1
#
# Load in sea ice data.
# Path: http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv
# And add Column names
#
isn<-data.table::fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep=";")
colnames(isn) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin" )
# Add an explicit date feature to the data frame.
isn$Ymd <- as.Date(paste(isn$Year, isn$Month, 
                         isn$Day, sep = "-"))
isn_a <- isn[Year>=2009,.(Ymd,Year,Month,Spots,Sd)]

View(isn_a)
ggplot(data=isn_a,aes(x=Sd,y=Spots)) +geom_point() + geom_smooth(method="glm") +
  geom_smooth(method="loess")

# Filter data to only the northern hemisphere.
#isn <- isn[isn$hemisphere == "north",]


#
# Video 2
#


# Leverage the xts package to create the initial time series object
# as the observations in the original dataset are measured every 
# two days.
# help(package = "xts")

isn.xts <- xts(x = isn$Spots, order.by = isn$Ymd)
str(isn.xts)




# Use the xts package and aggreage the data to the month level,
# averaging the Extent values for each month.
isn.monthly <- apply.monthly(isn.xts, mean)
str(isn.monthly)
View(isn.monthly)
isn.weekly <- apply.weekly(isn.xts, sum)
str(isn.weekly)
View(isn.weekly)
## Calc weekly Std Dev.
isn.sd <- apply.weekly(isn.xts, sd)
str(isn.sd)
View(isn.sd)
isn_wek <- as.data.table(isn.weekly)
isn_bb <- as.data.table(isn.sd)

isn_b <- merge(isn.weekly,isn.sd,by=index)
# In time series analysis it is a common practice to split data 
# using 80/20 for train/test.
isn.end <- floor(0.8 * length(isn.monthly))
isn.train <- isn.monthly[1:isn.end]
isn.test <- isn.monthly[(isn.end + 1):length(isn.monthly)]




# Many of the visualizations/functions work best/only with R 
# ts objects, convert xts train/test data to ts objects.
isn.start <- c(year(start(isn.train)), month(start(isn.train)))
isn.end <- c(year(end(isn.train)), month(end(isn.train)))
isn.train <- ts(as.numeric(isn.train), start = isn.start,
                   end = isn.end, frequency = 12)
isn.train

isn.start <- c(year(start(isn.test)), month(start(isn.test)))
isn.end <- c(year(end(isn.test)), month(end(isn.test)))
isn.test <- ts(as.numeric(isn.test), start = isn.start,
                   end = isn.end, frequency = 12)
isn.test

# Use variable to track forecast horizon
forecast.horizon <- length(isn.test)
