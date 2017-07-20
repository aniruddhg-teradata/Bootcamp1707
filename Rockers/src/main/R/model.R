

#get necessary libraries

library(readr) #reading external files
library(dplyr) #data wrangling
library(lubridate) #date manipulation
library(bnlearn) #bayesian network modeling

#read in data

la_sheriff_crime <- read_csv("~/Downloads/LA_Sheriff_Crimes_from_2004_to_2016__Ordered_by_Zip_Code.csv")

#convert timestamp to POSIX and create date column without time (to merge with foreclosure dataset)

la_sheriff_crime$CRIME_DATE<-mdy_hms(la_sheriff_crime$CRIME_DATE)
la_sheriff_crime<-la_sheriff_crime %>% mutate(date_notime=as.Date(date))

#get the weekdays

la_sheriff_crime<-la_sheriff_crime %>% mutate(weekday=weekdays(CRIME_DATE))

#6-12 morning,12-18 noon,18-22 evening,22-6 night

hours<-as.numeric(format(la_sheriff_crime$CRIME_DATE,"%H"))
la_sheriff_crime<-la_sheriff_crime %>% mutate(timeofday=as.character(cut(hours,c(6,12,18,22),c("morning","noon","evening"))))
la_sheriff_crime$timeofday<-ifelse(is.na(la_sheriff_crime$timeofday),"night",la_sheriff_crime$timeofday)
rm(hours)





#filter for 2016, get rid of NAs, only selected columns

la_sheriff_crime<-la_sheriff_crime %>% mutate(year=year(CRIME_DATE)) %>% filter(year==2016) %>% select(-year)
la_sheriff_crime<-la_sheriff_crime %>% select(ZIP,CRIME_DATE,CRIME_CATEGORY_DESCRIPTION,weekday,timeofday)
la_sheriff_crime<-na.omit(la_sheriff_crime)




#rename columns for better readability

names(la_sheriff_crime)<-c("zip","date","crime","weekday","timeofday","date_notime")




#get unique zip codes

zip_unq<-unique(la_sheriff_crime$zip)





#fit the model

la_sheriff_crime<-as.data.frame(lapply(la_sheriff_crime,as.factor))
res<-hc(la_sheriff_crime) #hill climbing
crime_fit<-bn.fit(res,la_sheriff_crime)

plot(res) #plot the graph






#adjust network structure (in case algorithm result is not sufficient)

crime_links<-as.data.frame(expand.grid(names(la_sheriff_crime),names(la_sheriff_crime))) #link everything

arcs(res)<-matrix(c("weather","crime",
                    "zip","crime","foreclosures","crime",
                    "timeofday","crime","weekday","crime"),ncol = 2,byrow = T,dimnames = list(NULL,c("from","to")))

res$arcs<-res$arcs[-which((res$arcs[,'from']=="crime" & res$arcs[,'to']=="zip")),] #remove links





#cpquery based on monte carlo particle filters(?!?) and yiels different probabilities,
#increase number of draws (n) to reduce variation, find trade-off between speed and accuracy

cpquery(crime_fit, event = (crime=="ARSON"),
        evidence = (weekday=="Monday"),n=100000)





#foreclosure join

foreclosures2016 <- read_csv("~/Documents/foreclosures2016.csv")

foreclosure_count<-foreclosures2016 %>% count(RegisteredDate)

names(foreclosure_count)<-c("date","foreclosures")

attributes(foreclosure_count)$spec<-NULL

foreclosure_count$date<-mdy(foreclosure_count$date)

la_sheriff_crime<-left_join(la_sheriff_crime,foreclosure_count,by=c("date_notime"="date"))

rm(foreclosure_count,foreclosures2016)

la_sheriff_crime[is.na(la_sheriff_crime$foreclosures),"foreclosures"]<-0





#weather join

weather <- read_csv("~/weather.csv")

weather<-weather %>% mutate(date=ymd(paste(weather$Year,weather$Month,weather$Day))) %>% select(date,Events)

names(weather)<-c("date","weather")

weather[which(weather$weather=="Fog , Rain"),"weather"]<-"Fog"

weather[is.na(weather$weather),"weather"]<-"Sunny"

la_sheriff_crime<-left_join(la_sheriff_crime,weather,by=c("date_notime"="date"))

rm(weather)





#get rid of timestamps and finalize dataset

la_sheriff_crime<-la_sheriff_crime %>% select(-c(date,date_notime))
la_sheriff_crime<-as.data.frame(lapply(la_sheriff_crime,as.factor))



#reduced crime dataset

#reduce zip, crime type and group foreclosures by quartiles
la_sheriff_crime %>% count(zip) %>% arrange(desc(n)) %>% mutate(frq=n/sum(n),cumsum=cumsum(frq))

la_sheriff_crime %>% count(crime) %>% arrange(desc(n)) %>% mutate(frq=n/sum(n),cumsum=cumsum(frq))



crime_reduced<-sample_n(la_sheriff_crime,500)

crime_reduced<-as.data.frame(lapply(crime_reduced,as.character))

crime_reduced<-as.data.frame(lapply(crime_reduced,as.factor))

res<-hc(crime_reduced)

arcs(res)<-matrix(c("weather","crime",
                    "zip","crime","foreclosures","crime",
                    "timeofday","crime","weekday","crime"),ncol = 2,byrow = T,dimnames = list(NULL,c("from","to")))

crime_fit<-bn.fit(res,crime_reduced)




#visualization and maps

library(shiny)
library(ggmap)
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)

ggmap(get_map(location="Los Angeles", zoom=9))

la_poly<-readOGR(file.choose()) %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

ggplot(la_poly, aes(x = long, y = lat, group = group)) + geom_path()

