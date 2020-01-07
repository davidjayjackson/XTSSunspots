library(tidyverse)
library(xts)
library(prophet)
library(data.table)
rm(list=ls())
## Read Data: 
isn<-fread("http://sidc.be/silso/DATA/SN_d_tot_V2.0.csv",sep=";")
colnames(isn) <- c("Year","Month","Day", "Fdate","Spots", "Sd","Obs" ,"Defin" )
# Add an explicit date feature to the data frame.
isn$Ymd <- as.Date(paste(isn$Year, isn$Month, 
                         isn$Day, sep = "-"))
## Add Yes and No counts.
isn$Cts <- ifelse(isn$Spots <=0,"No","Yes")
## Create Yes/No Chart by Year: Falim Ahmad
data.frame(table(isn$Cts)) %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var1)) +
  geom_col() + labs(title="Ratio of Days w/wo Spots: 1850-2019")

barplot(table(isn$Cts),main="Total Days with/out Spots")
## Begin Decade calc: 2009 - 2019
isn_d <- isn %>% filter(Year >=2009 &  Year <=2019) %>% select(Year,Month,Spots,Sd,Cts,Obs)
data.frame(table(isn_d$Cts)) %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var1)) +
  geom_col() + labs(title="Ratio of Days w/wo Spots: 2009-2019") + facet_wrap(~Year)

ggplot(isn_d,aes(x=Month,y=Spots)) +geom_col() + facet_wrap(~Year) +
  ggtitle("Summary of ISN by Year and Month:2009 - 2019") +ylab("Internation Sunbspot Number")
## Spots vs Sd
ggplot(isn_d,aes(x=Obs,y=Sd)) +geom_col() + facet_wrap(~Year) +
  ggtitle("Summary of SD vs Obs by Year and Month:2009 - 2019") +
   ylab("# of Observations") + xlab("Standard Deviation") +
  coord_flip()
##
# ggplot(isn_d,aes(x=Month,y=Spots)) +geom_col() + facet_wrap(~Cts) +
#   ggtitle("Summary of SD vs ISN by Year and Month:2009 - 2019") +
#   ylab("Internation Sunbspot Number") + xlab("Standard Deviation") +
#   geom_smooth(method="glm")
## Begin Prophet Prediction
df<-isn[Ymd >="1850-01-01" ,.(Ymd,Spots)]
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=365.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=4000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("SIDC Sunspot Prediction: Jan. 1810 - May. 2022")
## Cycle 25 and Beyond
forecast <- as.data.table(forecast)
forecast1 <- forecast[ds >="2019-01-01"  &  ds <="2022-12-31",]
ggplot(data=forecast1,aes(x=ds,y=yhat)) +geom_line() + 
  geom_smooth() +   ggtitle("SIDC: Cycle 25 and  Beyond: 2019 - 2022")
## Create Yes/No Chart by Year: Falim Ahmad
data.frame(table(isn$Cts)) %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var1)) +
  geom_col() + labs(title="Decade of the 60's")

isn_d <-isn %>% filter(Year >=2009 & Year <=2019) %>% select(Year,Month,Spots,Sd,Cts,Obs)
  ggplot(data=isn_d,aes(x=Cts , fill=Cts ))+
  geom_histogram(stat="count") + labs(title="Ratio of Days w/wo Spots: 2009-2019") + facet_wrap(~Year)
