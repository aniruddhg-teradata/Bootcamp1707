

#get necessary libraries

library(dplyr) #data wrangling
library(lubridate) #date manipulation
library(bnlearn) #bayesian network modeling

#read in data

la_sheriff_crime <- read_csv("~/Downloads/LA_Sheriff_Crimes_from_2004_to_2016__Ordered_by_Zip_Code.csv")

#convert timestamp to POSIX

la_sheriff_crime$CRIME_DATE<-mdy_hms(la_sheriff_crime$CRIME_DATE)

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

#get unique zip codes

zip_unq<-unique(la_sheriff_crime$zip)

#rename columns for better readability

names(la_sheriff_crime)<-c("zip","date","crime","weekday","timeofday")

#fit the model

la_sheriff_crime<-as.data.frame(lapply(la_sheriff_crime,as.factor))
res<-hc(la_sheriff_crime)
crime_fit<-bn.fit(res,la_sheriff_crime)

plot(res) #plot the graph

#adjust network structure (in case algorithm result is not sufficient)

crime_links<-as.data.frame(expand.grid(names(la_sheriff_crime),names(la_sheriff_crime)))

names(crime_links)<-c("from","to")

res$arcs<-res$arcs[-which((res$arcs[,'from']=="crime" & res$arcs[,'to']=="zip")),] #remove links

#cpquery based on monte carlo particle filters(?!?) and yiels different probabilities,
#increase number of draws (n) to reduce variation, find trade-off between speed and accuracy

cpquery(crime_fit, event = (crime=="BURGLARY"),
        evidence = (weekday=="Monday" & timeofday=="noon" & zip==zip),n=100000)

#foreclosure join

foreclosure_count<-foreclosures2016 %>% count(RegisteredDate)

names(foreclosure_count)[1]<-"date"

left_join(la_sheriff_crime,foreclosure_count,by="date")


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

