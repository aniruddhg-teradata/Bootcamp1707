#v3

#get necessary libraries

library(readr) #reading external files
library(dplyr) #data wrangling
library(lubridate) #date manipulation
library(bnlearn) #bayesian network modeling


#:):):):):):):):):):):):):):):):):):):):):):):):):)#


#DATA WRANGLING

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


#:):):):):):):):):):):):):):):):):):):):):):):):):)#


#BAYESIAN NETWORK

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




#:):):):):):):):):):):):):):):):):):):):):):):):):)#




#REDUCED CRIME DATASET

#alternatively draw random sample of required size

#reduce zip, crime type and group foreclosures by quartiles

#these zips making up 35% of total crimes
zip_reduced<-la_sheriff_crime %>% count(zip) %>% arrange(desc(n)) %>% select(zip) %>% top_n(10)
zip_reduced<-c(90012,90706,90650,90262,90220,90723,90022,90703,90660,91770)
la_sheriff_crime_reduced<-la_sheriff_crime %>% filter(zip %in% zip_reduced)
rm(zip_reduced)


#these crime types making up 85% of total crimes
crimetype_reduced<-la_sheriff_crime %>% count(crime) %>% arrange(desc(n)) %>% select(crime) %>% top_n(10)
crimetype_reduced<-c(as.character(crimetype_reduced$crime))
la_sheriff_crime_reduced<-la_sheriff_crime_reduced %>% filter(crime %in% crimetype_reduced)
rm(crimetype_reduced)


#group forclosures in bin by quartiles

quantile(as.integer(la_sheriff_crime_reduced$foreclosures))


la_sheriff_crime_reduced<-la_sheriff_crime_reduced %>%
  mutate(foreclosures=cut(as.integer(foreclosures),
                 c(-Inf,quantile(as.integer(la_sheriff_crime_reduced$foreclosures))),
                 c("0","1-3","4-18","19-44","45-65")))


timeofday_reduced<-c("morning","noon","evening","night")
weekday_reduced<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")



#:):):):):):):):):):):):):):):):):):):):):):):):):)#



#REDUCED SET BAYESIAN MODEL (runs fast)


#fit the model

la_sheriff_crime_reduced<-as.data.frame(lapply(la_sheriff_crime_reduced,as.character))
la_sheriff_crime_reduced<-as.data.frame(lapply(la_sheriff_crime_reduced,as.factor))
res<-hc(la_sheriff_crime_reduced) #hill climbing
crime_fit<-bn.fit(res,la_sheriff_crime_reduced)

plot(res) #plot the graph

#adjust network structure (in case algorithm result is not sufficient)

crime_links<-as.data.frame(expand.grid(names(la_sheriff_crime),names(la_sheriff_crime))) #link everything

arcs(res)<-matrix(c("weather","crime",
                    "zip","crime","foreclosures","crime",
                    "timeofday","crime","weekday","crime"),ncol = 2,byrow = T,dimnames = list(NULL,c("from","to")))

res$arcs<-res$arcs[-which((res$arcs[,'from']=="crime" & res$arcs[,'to']=="zip")),] #remove links

#cpquery based on monte carlo particle filters(?!?) and yiels different probabilities,
#increase number of draws (n) to reduce variation, find trade-off between speed and accuracy


#ZIP loop
zip_probs<-data.frame(zip=zip_reduced,prob=NA)
row=1

for(x in zip_reduced)
{


  zip_probs[row,2]<-cpquery(crime_fit, event = (crime=="VANDALISM"),
        evidence = (weekday=="Saturday" & timeofday=="night" &
                      zip==x),n=1000000)
  row=row+1
  
}
  
rm(x,row)

#weekday loop
weekday_probs<-data.frame(weekday=weekday_reduced,prob=NA)
row=1

for(x in weekday_reduced)
{
  
  
  weekday_probs[row,2]<-cpquery(crime_fit, event = (crime=="VANDALISM"),
                            evidence = (weekday==x & timeofday=="night"),n=1000000)
  row=row+1
  
}

rm(x,row)


#timeofday loop
timeofday_probs<-data.frame(timeofday=timeofday_reduced,prob=NA)
row=1

for(x in timeofday_reduced)
{
  
  
  timeofday_probs[row,2]<-cpquery(crime_fit, event = (crime=="VANDALISM"),
                                evidence = (weekday=="Monday" & timeofday==x),n=1000000)
  row=row+1
  
}

rm(x,row)
  
#keep weather "Sunny" as weather states are not evenly distributed and will lead to very low probability
#foreclosure data way too sparse and not evenly distributed among zip code areas



#:):):):):):):):):):):):):):):):):):):):):):):):):)#



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

