require(rjson) 
require(RCurl) 
library(jsonlite)
library(data.table)
library(dplyr)
library(plyr)
library(leaflet)
library(ggplot2)
library(forecast)
library(animation)

install.packages("~/Documents/Bootcamp/data/rHadoopClient_0.2.tar", repos=NULL, type="source")
library(rHadoopClient)


read_Json_had <-  function(pathte)
{
  varJS <- rHadoopClient::read.hdfs(paste0("hdfs://ec2-54-208-153-19.compute-1.amazonaws.com:8020/landingzone/",pathte))
  varJS[,1] <- lapply(seq(1:dim(varJS)[1]),function(x) gsub("\\[\\[","",varJS[,1]))
  varJS[,1] <- lapply(seq(1:dim(varJS)[1]),function(x) gsub("\\[","",varJS[,1]))
  varJS[,5] <- lapply(seq(1:dim(varJS)[1]),function(x) gsub("\\]\\]","",varJS[,5]))
  varJS[,5] <- lapply(seq(1:dim(varJS)[1]),function(x) gsub("\\]","",varJS[,5]))
  varJS[,6] <- NULL
  varJS
}  


assign_loc <- function(drugs2011){
  newloc <- matrix(nrow=dim(drugs2011)[1],ncol=4)
  ind <- which(drugs2011$METRO == 1)
  newloc[ind,1] <- "BOSTON"
  newloc[ind,2] <- 42.352881
  newloc[ind,3] <- -71.059911
  newloc[ind,4] <- 4552402
  ind <- which(drugs2011$METRO == 2)
  newloc[ind,1] <- "NEWYORK"
  newloc[ind,2] <- 40.698647 
  newloc[ind,3] <- -74.057162
  newloc[ind,4] <- 19567410
  ind <- which(drugs2011$METRO == 3)
  newloc[ind,1] <- "CHICAGO"
  newloc[ind,2] <- 41.875712 
  newloc[ind,3] <- -87.671082
  newloc[ind,4] <- 9461105
  ind <- which(drugs2011$METRO == 4)
  newloc[ind,1] <- "DETROIT"
  newloc[ind,2] <- 42.332267 
  newloc[ind,3] <- -83.069369
  newloc[ind,4] <- 4296250	
  ind <- which(drugs2011$METRO == 5)
  newloc[ind,1] <- "MINNEAPOLIS"
  newloc[ind,2] <- 42.332267 
  newloc[ind,3] <- -93.274090
  newloc[ind,4] <- 3348859
  ind <- which(drugs2011$METRO == 7 | drugs2011$METRO == 6)
  newloc[ind,1] <- "DADECOUNTY"
  newloc[ind,2] <- 25.488141 
  newloc[ind,3] <- -80.383191
  newloc[ind,4] <- 2712945
  ind <- which(drugs2011$METRO == 8)
  newloc[ind,1] <- "HOUSTON"
  newloc[ind,2] <- 29.745367 
  newloc[ind,3] <- -95.368367
  newloc[ind,4] <- 5920416
  ind <- which(drugs2011$METRO == 9)
  newloc[ind,1] <- "DENVER"
  newloc[ind,2] <- 39.723877
  newloc[ind,3] <- -104.985605 
  newloc[ind,4] <- 2543482
  ind <- which(drugs2011$METRO == 10)
  newloc[ind,1] <- "PHOENIX"
  newloc[ind,2] <- 33.501525
  newloc[ind,3] <- -112.089378 
  newloc[ind,4] <- 4192887
  ind <- which(drugs2011$METRO == 12 | drugs2011$METRO == 11)
  newloc[ind,1] <- "SANFRANCISCO"
  newloc[ind,2] <- 37.762300
  newloc[ind,3] <- -122.436169
  newloc[ind,4] <- 805235
  ind <- which(drugs2011$METRO == 13)
  newloc[ind,1] <- "SEATTLE"
  newloc[ind,2] <- 47.592308
  newloc[ind,3] <- -122.327673
  newloc[ind,4] <- 3439809
  ind <- which(drugs2011$METRO == 14)
  newloc[ind,1] <- "OTHER"
  newloc[ind,2] <- 38.555592
  newloc[ind,3] <- -101.427396
  newloc[ind,4] <- NA
  newloc
}

give_me_drugs <- function(drugs2011){
  drugs2011 <- drugs2011[,seq(1:30)]
  drugs2011$LOC <- assign_loc(drugs2011)[,1]
  drugs2011$LAT <- assign_loc(drugs2011)[,2]
  drugs2011$LONG <- assign_loc(drugs2011)[,3]
  drugs2011$POPULATION <- assign_loc(drugs2011)[,4]
  drugs2011_c <- ddply(drugs2011,.(LAT,LONG,POPULATION,LOC),nrow)
  drugs2011_c[['RATE']] <-  (drugs2011_c$V1 %>% as.numeric)/(drugs2011_c$POPULATION %>% as.numeric)
  colnames(drugs2011_c) <- c("LAT","LONG","POPULATION","LOCATION","AMOUNT","RATE")
  drugs2011_c
}


geocode_apply<-function(x){
  x<- gsub(",\\s","+",x)
  x<- gsub("-","+",x)
  x<- gsub("/","+",x)
  x<- gsub("\\s","+",x)
  #url   <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",x,"&key=AIzaSyAR-HW-CSeXIH3e_GZkpfRFKOqIy0juKM0")
  #print(url)
  #print(count)
  #result <- getURL(url)
  result <- fromJSON(paste0("https://maps.googleapis.com/maps/api/geocode/json?address=",x,"&key=AIzaSyAR-HW-CSeXIH3e_GZkpfRFKOqIy0juKM0"))
  #print(result)
  result
}


clean_pov <- function(pov,codes)
{
  pov <- as.data.frame(pov)
  #pov[['Area']] <- "X"
  #pov[['Lat']] <- 0
  #pov[['Long']] <- 0
  if(length(which(as.numeric(paste(pov$state)) > 56)) > 0)  pov <- pov[-c(which(as.numeric(paste(pov$state)) > 56)),]
  pov$state <- as.numeric(paste(pov$state))
  pov <- merge(x=pov,y=codes,by.x = "state" ,by.y = "Code") # <- merge(pov,pov2014, by.x = "SD_NAME", by.y = "SD_NAME")
  pov
}

usCodes <- read.csv("~/Documents/Bootcamp/data/us_state_codes.csv", sep =";")
usCodes$State <- colnames(murder)[2:52]

pov2015 <- read_Json_had("pov2015") 
# fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2015&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2015) <- pov2015[1,]
pov2015 <- pov2015[-1,]

pov2014 <- read_Json_had("pov2014")  #fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2014&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2014) <- pov2014[1,]
pov2014 <- pov2014[-1,]

pov2013 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2013&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2013) <- pov2013[1,]
pov2013 <- pov2013[-1,]

pov2012 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2012&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2012) <- pov2012[1,]
pov2012 <- pov2012[-1,]

pov2011 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2011&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2011) <- pov2011[1,]
pov2011 <- pov2011[-1,]

pov2010 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2010&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2010) <- pov2010[1,]
pov2010 <- pov2010[-1,]

pov2009 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2009&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2009) <- pov2009[1,]
pov2009 <- pov2009[-1,]

pov2008 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2008&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2008) <- pov2008[1,]
pov2008 <- pov2008[-1,]

pov2007 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2007&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2007) <- pov2007[1,]
pov2007 <- pov2007[-1,]

pov2006 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2006&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2006) <- pov2006[1,]
  pov2006 <- pov2006[-1,]

pov2005 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2005&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2005) <- pov2005[1,]
pov2005 <- pov2005[-1,]

pov2004 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2004&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2004) <- pov2004[1,]
pov2004 <- pov2004[-1,]

pov2003 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2003&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2003) <- pov2003[1,]
pov2003 <- pov2003[-1,]

pov2002 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2002&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2002) <- pov2002[1,]
pov2002 <- pov2002[-1,]

pov2001 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2001&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2001) <- pov2001[1,]
pov2001 <- pov2001[-1,]

pov2000 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=2000&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov2000) <- pov2000[1,]
pov2000 <- pov2000[-1,]

pov1999 <- fromJSON('http://api.census.gov/data/timeseries/poverty/saipe/schdist?get=SD_NAME,SAEPOVRAT5_17RV_PT&for=school+district+(unified):*&in=state:*&time=1999&key=90b5a6faf3e3aa4d50571d9a6447eb00ab47237c')
colnames(pov1999) <- pov1999[1,]
pov1999 <- pov1999[-1,]

murder <- read.csv("/Users/sk186171/Documents/Bootcamp/data/CLEAN_estimated_murder.csv")
rownames(murder) <- murder[,1]
murder_t <- transpose(murder)
colnames(murder_t) <- rownames(murder)
rownames(murder_t) <- colnames(murder)
murder_t <- murder_t[-1,]



drugs2011 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2011.tsv",sep = "\t")
drugs2011_c <- give_me_drugs(drugs2011)

drugs2010 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2010.tsv",sep = "\t")
drugs2010_c <- give_me_drugs(drugs2010)

drugs2009 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2009.tsv",sep = "\t")
drugs2009_c <- give_me_drugs(drugs2009)

drugs2008 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2008.tsv",sep = "\t")
drugs2008_c <- give_me_drugs(drugs2008)

drugs2007 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2007.tsv",sep = "\t")
drugs2007_c <- give_me_drugs(drugs2007)

drugs2006 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2006.tsv",sep = "\t")
drugs2006_c <- give_me_drugs(drugs2006)

drugs2005 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2005.tsv",sep = "\t")
drugs2005_c <- give_me_drugs(drugs2005)

drugs2004 <- read.csv("/Users/sk186171/Documents/Bootcamp/data/drugs2004.tsv",sep = "\t")
drugs2004_c <- give_me_drugs(drugs2004)

drugs_times <- cbind(drugs2004_c,drugs2005_c[,c(5,6)],drugs2006_c[,c(5,6)],
                     drugs2007_c[,c(5,6)],drugs2008_c[,c(5,6)],drugs2009_c[,c(5,6)],
                     drugs2010_c[,c(5,6)],drugs2011_c[,c(5,6)])
colnames(drugs_times) <-    c("LAT","LONG","POPULATION","LOCATION","AMOUNT2004","RATE2004","AMOUNT2005","RATE2005","AMOUNT2006","RATE2006","AMOUNT2007","RATE2007"
                              ,"AMOUNT2008","RATE2008","AMOUNT2009","RATE2009","AMOUNT2010","RATE2010","AMOUNT2011","RATE2011")

longlat <- NULL
longlat$LAT <- drugs_times$LAT
longlat$LONG <- drugs_times$LONG
longlat$AREA <- drugs_times$LOCATION

drugsAV <- NULL
years <- c(2004,2005,2006,2007,2008,2009,2010,2011)
drugsAV <- rbind(drugs2004_c[,c(5)],drugs2005_c[,c(5)],drugs2006_c[,c(5)],drugs2007_c[,c(5)],drugs2008_c[,c(5)],drugs2009_c[,c(5)],
                 drugs2010_c[,c(5)],drugs2011_c[,c(5)])
drugsAV <- cbind(years,drugsAV) %>% as.data.frame
colnames(drugsAV) <- c("YEAR",longlat$AREA)

drugsNORM <- NULL
years <- c(2004,2005,2006,2007,2008,2009,2010,2011)
drugsNORM <- rbind(drugs2004_c[,c(6)],drugs2005_c[,c(6)],drugs2006_c[,c(6)],drugs2007_c[,c(6)],drugs2008_c[,c(6)],drugs2009_c[,c(6)],
                 drugs2010_c[,c(6)],drugs2011_c[,c(6)])
drugsNORM <- cbind(years,drugsNORM) %>% as.data.frame
colnames(drugsNORM) <- gsub("OTHER","OTHERS",colnames(drugsNORM))
colnames(drugsNORM) <- gsub("PHOENIX","PHOENI",colnames(drugsNORM))
drugsSORT <- NULL
drugsSORT$Year <- drugsNORM$YEAR
for(i in 1:length(names(regions))){
  drugsSORT[[paste0(names(regions)[i])]] <- drugsNORM[[paste0(names(regions)[i])]]
}
drugsSORT <- as.data.frame(drugsSORT)
colnames(drugsNORM) <- c("YEAR",longlat$AREA)

drugsNORM_TOT <- NULL
for(i in 2:dim(drugsNORM)[2]) drugsNORM_TOT <- rbind(drugsNORM_TOT,cbind(years,c(drugsNORM[[i]])))
drugsNORM_TOT  <- as.data.frame(drugsNORM_TOT)
drugsNORM_TOT$AREA <- "X"
for(j in 2:dim(drugsNORM)[2]) {for(i in 1:length(years)) drugsNORM_TOT$AREA[(j-2)*length(years)+i] <- colnames(drugsNORM)[j] }

murder_TOT <- NULL
years <- murder$Year
for(i in 2:dim(murder)[2]) murder_TOT <- rbind(murder_TOT,cbind(years,c(murder[[i]])))
murder_TOT  <- as.data.frame(murder_TOT)
murder_TOT$AREA <- "X"
for(j in 2:dim(murder)[2]) {for(i in 1:length(years)) murder_TOT$AREA[(j-2)*length(years)+i] <- colnames(murder)[j] }

pov2014 <- as.data.frame(pov2014)
pov2014[['Area']] <- "X"
pov2014[['Lat']] <- 0
pov2014[['Long']] <- 0
for(i in 10642:dim(pov2014)[1]){
  res <-  geocode_apply(pov2014[i,1])
  if(res$status == "OK"){
    pov2014$Area[i] <- res$results$address_components[[1]]$long_name[2]
    pov2014$Lat[i] <- res$results$geometry$location[1] 
    pov2014$Long[i] <- res$results$geometry$location[2] 
  }
  if(res$status == "REQUEST_DENIED"){
    print("STOP!")
  }
}


pov1999 <- clean_pov(pov1999,usCodes)
pov2000 <- clean_pov(pov2000,usCodes)
pov2001 <- clean_pov(pov2001,usCodes)
pov2002 <- clean_pov(pov2002,usCodes)
pov2003 <- clean_pov(pov2003,usCodes)
pov2004 <- clean_pov(pov2004,usCodes)
pov2005 <- clean_pov(pov2005,usCodes)
pov2006 <- clean_pov(pov2006,usCodes)
pov2007 <- clean_pov(pov2007,usCodes)
pov2008 <- clean_pov(pov2008,usCodes)
pov2009 <- clean_pov(pov2009,usCodes)
pov2010 <- clean_pov(pov2010,usCodes)
pov2011 <- clean_pov(pov2011,usCodes)
pov2012 <- clean_pov(pov2012,usCodes)
pov2013 <- clean_pov(pov2013,usCodes)
pov2014 <- clean_pov(pov2014,usCodes)
pov2015 <- clean_pov(pov2015,usCodes)


regions <- as.data.frame(drugs_times$LOCATION)


regions <- (list(c("Rhode.Island", "Connecticut", "Massachusetts", "New.Hampshire", "Maine", "Vermont"),
c("New.York", "New.Jersey", "Pennsylvania", "Delaware"),
c("Illinois", "Indiana", "Wisconsin"),
c("Michigan", "Ohio"),
c("Minnesota", "North.Dakota", "South.Dakota"),
c("Florida", "Georgia"),
c("Texas", "Louisiana"),
c("Utah", "Colorado", "Kansas"),
c("Arizona", "New.Mexico"),
c("California"),
c("Washington", "Oregon", "Idaho"),
c("Nevada", "Wyoming", "Montana", "Oklahoma", "Arkansas", "Mississippi", "Alabama", "Missouri", "West.Virginia", "Kentucky", "North.Carolina", "South.Carolina", "Alaska")))
names(regions) <- c("BOSTON","NEWYORK","CHICAGO","DETROIT","MINNEAPOLIS","DADECOUNTY","HOUSTON","DENVER","PHOENI","SANFRANCISCO","SEATTLE","OTHERS")


names(regions)[grep("Montana",regions )]

murder$Year <-  seq(from = 1960, to = 2014)
murder_regions <- NULL
murder_regions$Year <-murder$Year
for(i in 1:length(names(regions))){
  for(j in 1:length(murder$Year)){
    zw <- 0
    for(k in 1:length(regions[i][[1]])){
      zw <- zw + murder[[paste0(regions[i][[1]][k] )]][j] 
      murder_regions[[paste0(names(regions)[i])]][j] <- zw/length(regions[i][[1]])
    }   
  }
}

pov1999State <- ddply(pov1999,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2000State <- ddply(pov2000,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2001State <- ddply(pov2001,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2002State <- ddply(pov2002,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2003State <- ddply(pov2003,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2004State <- ddply(pov2004,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2005State <- ddply(pov2005,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2006State <- ddply(pov2006,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2007State <- ddply(pov2007,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2008State <- ddply(pov2008,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2009State <- ddply(pov2009,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2010State <- ddply(pov2010,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2011State <- ddply(pov2011,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2012State <- ddply(pov2012,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2013State <- ddply(pov2013,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2014State <- ddply(pov2014,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))
pov2015State <- ddply(pov2015,.(State),summarize,mean = mean(as.numeric(paste(SAEPOVRAT5_17RV_PT))))

pov_states <- NULL
pov_states$Year <- c(seq(from = 1999, to = 2015))

for(i in 1:length(names(murder))){
  pov_states[[names(murder)[i]]] <- c(pov1999State$mean[pov1999State$State == names(murder)[i]],
                                          pov2000State$mean[pov2000State$State == names(murder)[i]],
                                          pov2001State$mean[pov2001State$State == names(murder)[i]],
                                          pov2002State$mean[pov2002State$State == names(murder)[i]],
                                          pov2003State$mean[pov2003State$State == names(murder)[i]], 
                                          pov2004State$mean[pov2004State$State == names(murder)[i]],
                                          pov2005State$mean[pov2005State$State == names(murder)[i]],
                                          pov2006State$mean[pov2006State$State == names(murder)[i]],
                                          pov2007State$mean[pov2007State$State == names(murder)[i]],
                                          pov2008State$mean[pov2008State$State == names(murder)[i]],
                                          pov2009State$mean[pov2009State$State == names(murder)[i]],
                                          pov2010State$mean[pov2010State$State == names(murder)[i]],
                                          pov2011State$mean[pov2011State$State == names(murder)[i]],
                                          pov2012State$mean[pov2012State$State == names(murder)[i]],
                                          pov2013State$mean[pov2013State$State == names(murder)[i]],
                                          pov2014State$mean[pov2014State$State == names(murder)[i]],
                                          pov2015State$mean[pov2015State$State == names(murder)[i]])
} 
  
  
pov_regions <- NULL
pov_regions$Year <- pov_states$Year
for(i in 1:length(names(regions))){
  for(j in 1:length(pov_states$Year)){
    zw <- 0
    for(k in 1:length(regions[i][[1]])){
      zw <- zw + pov_states[[paste0(regions[i][[1]][k] )]][j] 
      pov_regions[[paste0(names(regions)[i])]][j] <- zw/length(regions[i][[1]])
    }   
  }
}
  
  
pov_regions <- as.data.frame(pov_regions)
murder_regions <- as.data.frame(murder_regions)  

murderReg_TOT <- NULL
years <- murder_regions$Year
for(i in 2:dim(murder_regions)[2]) murderReg_TOT <- rbind(murderReg_TOT,cbind(years,c(murder_regions[[i]])))
murderReg_TOT  <- as.data.frame(murderReg_TOT)
murderReg_TOT$Region <- "X"
for(j in 2:dim(murder_regions)[2]) {for(i in 1:length(years)) murderReg_TOT$Region[(j-2)*length(years)+i] <- colnames(murder_regions)[j] }
murderReg_TOT

povReg_TOT <- NULL
years <- pov_regions$Year
for(i in 2:dim(pov_regions)[2]) povReg_TOT <- rbind(povReg_TOT,cbind(years,c(pov_regions[[i]])))
povReg_TOT  <- as.data.frame(povReg_TOT)
povReg_TOT$Region <- "X"
for(j in 2:dim(pov_regions)[2]) {for(i in 1:length(years)) povReg_TOT$Region[(j-2)*length(years)+i] <- colnames(pov_regions)[j] }
povReg_TOT$Region <- gsub('X.','',povReg_TOT$Region)
povReg_TOT$Region <- gsub('\\.','',povReg_TOT$Region)
colnames(povReg_TOT) <- c("YEAR","RATE","REGION")
colnames(murderReg_TOT) <- c("YEAR","RATE","REGION")
colnames(drugsNORM_TOT) <- c("YEAR","RATE","REGION")

povReg_TOT$TYPE <- "POVERTY"
murderReg_TOT$TYPE <- "MURDER"
drugsNORM_TOT$TYPE <- "DRUGS"

alltog <- rbind(povReg_TOT,murderReg_TOT,drugsNORM_TOT)

save(povReg_TOT, file = "./poverty.RData")
save(murderReg_TOT, file = "./murder.RData")
save(drugsNORM_TOT, file = "./drugs.RData")

colnames(pov_regions) <- gsub('X.','',colnames(pov_regions))
colnames(pov_regions) <- gsub('\\.','',colnames(pov_regions))

colnames(murder_regions) <- gsub('X.','',colnames(murder_regions))
colnames(murder_regions) <- gsub('\\.','',colnames(murder_regions))

cors <- NULL
year <- c(seq(from = 1999, to = 2014, by = 1))

for(i in 2:length(names(regions))){
  cutm <- NULL
  print(colnames(pov_regions)[i])
  cutm <- cbind(murder_regions[[colnames(pov_regions)[i+1]]][murder_regions$Year %in% year ],pov_regions[[colnames(pov_regions)[i+1]]][pov_regions$Year %in% year ]) %>% as.matrix
  cors <- c(cors,cor(x= murder_regions[[colnames(pov_regions)[i+1]]][murder_regions$Year %in% year ],y =pov_regions[[colnames(pov_regions)[i+1]]][pov_regions$Year %in% year ]))
}
cors_murder <- cors 


cors <- NULL
year <- c(seq(from = 2004, to = 2011, by = 1))

cutm <- cbind(drugsNORM[[colnames(pov_regions)[2]]][drugsNORM$YEAR %in% year ],pov_regions[[colnames(pov_regions)[2]]][pov_regions$Year %in% year ]) 
for(i in 2:length(names(regions))){
  print(colnames(pov_regions)[i])
  cutm <- rbind(drugsNORM[[colnames(pov_regions)[i+1]]][drugsNORM$YEAR %in% year ],pov_regions[[colnames(pov_regions)[i+1]]][pov_regions$Year %in% year ]) 
}
cors_drugs <- cors


loc <- NULL
loc$region <- names(regions)
loc$lat <- unlist(lapply(seq(1:length(loc$region)[1]),function(x) drugs_times$LAT[drugs_times$LOCATION ==loc$region[x]]))
loc$long <- unlist(lapply(seq(1:length(loc$region)[1]),function(x) drugs_times$LONG[drugs_times$LOCATION ==loc$region[x]]))
loc$population <- unlist(lapply(seq(1:length(loc$region)[1]),function(x) drugs_times$POPULATION[drugs_times$LOCATION ==loc$region[x]]))

loc$drugs <- unlist(lapply(seq(from = 2,to=dim(drugsSORT)[2]),function(x) mean(drugsSORT[[x]])))
loc$murder <- unlist(lapply(seq(from = 2, to = dim(murder_regions)[2]),function(x) mean(murder_regions[[x]],na.rm = T)))
loc$poverty <- unlist(lapply(seq(from = 2, to = dim(pov_regions)[2]),function(x) mean(pov_regions[[x]],na.rm = T)))
save(loc,file = "./locations.RData")

povMurd <- NULL
year <- c(seq(from = 1999, to = 2014, by = 1))
for(i in 2:length(names(regions))){
  povMurd$pov <- c(povMurd$pov,pov_regions[[colnames(pov_regions)[i+1]]][pov_regions$Year %in% year ])
  povMurd$murder <- c(povMurd$murder,murder_regions[[colnames(pov_regions)[i+1]]][murder_regions$Year %in% year ])
}

povMurd <- as.data.frame(povMurd)


year <- c(seq(from = 2004, to = 2011, by = 1))
povdrugs <- NULL
for(i in 2:length(names(regions))){
  povdrugs$pov <- c(povdrugs$pov,pov_regions[[colnames(pov_regions)[i+1]]][pov_regions$Year %in% year ])
  povdrugs$drugs <- c(povdrugs$drugs,drugsNORM[[colnames(pov_regions)[i+1]]][drugsNORM$YEAR %in% year ])
}
povdrugs <- as.data.frame(povdrugs)


year <- c(seq(from = 2004, to = 2011, by = 1))
murder_drugs <- NULL
for(i in 2:length(names(regions))){
  murder_drugs$murder <- c(murder_drugs$murder,murder_regions[[colnames(murder_regions)[i+1]]][murder_regions$Year %in% year ])
  murder_drugs$drugs <- c(murder_drugs$drugs,drugsNORM[[colnames(pov_regions)[i+1]]][drugsNORM$YEAR %in% year ])
}
murder_drugs <- as.data.frame(murder_drugs)

dif_murd <- NULL
for(i in seq(from = 2,to=dim(murder_regions)[2]))
{
  add_up <- NULL
  for(j in 1:10){
    add_up <- c(add_up,(murder_regions[[i]][dim(murder_regions)[1]-j]-murder_regions[[i]][dim(murder_regions)[1]-j-1] ) )
  }
  dif_murd[[paste(colnames(murder_regions)[i])]] <- add_up
}
dif_murd <- dif_murd %>% as.data.frame

diff_murd_mean <- sapply(seq(1:dim(dif_murd)[2]),function(x) mean(dif_murd[[x]]) ) 

var_murd <- NULL
for(i in seq(from = 2,to=dim(murder_regions)[2]))
{
  add_up <- NULL
  for(j in 1:10){
    add_up <- c(add_up,(murder_regions[[i]][dim(murder_regions)[1]-j]-murder_regions[[i]][dim(murder_regions)[1]-j-1] - diff_murd_mean[i-1] )^2 )
  }
  var_murd[[paste(colnames(murder_regions)[i])]] <- add_up
}
var_murd <- var_murd %>% as.data.frame

var_murd_mean <- sapply(seq(1:dim(var_murd)[2]),function(x) mean(var_murd[[x]]) ) 

set.seed(1)
pred_murder <- NULL
pred_murder$year <- c(2014,2015,2016,2017,2018,2019,2020)
for(i in seq(from = 2,to=dim(murder_regions)[2])){
  zw <- tail(murder_regions[[i]],1) 
  ran_zw <- rnorm(6,mean = diff_murd_mean[i-1],sd = sqrt(var_murd_mean[i-1]))
  for(j in 1:6) zw <- c(zw,tail(zw,1) + ran_zw[j])
  pred_murder[[paste(colnames(murder_regions)[i])]] <- zw
}
pred_murder <- as.data.frame(pred_murder)



dif_drug <- NULL
for(i in seq(from = 2,to=dim(drugsNORM)[2]))
{
  add_up <- NULL
  for(j in 1:dim(drugsNORM)[1]){
    add_up <- c(add_up,(drugsNORM[[i]][dim(drugsNORM)[1]-j]-drugsNORM[[i]][dim(drugsNORM)[1]-j-1] ) )
  }
  dif_drug[[paste(colnames(drugsNORM)[i])]] <- add_up
}
dif_drug <- dif_drug %>% as.data.frame

diff_drug_mean <- sapply(seq(1:dim(dif_drug)[2]),function(x) mean(dif_drug[[x]]) ) 

var_drug <- NULL
for(i in seq(from = 2,to=dim(drugsNORM)[2]))
{
  add_up <- NULL
  for(j in 1:dim(drugsNORM)[1]){
    add_up <- c(add_up,(drugsNORM[[i]][dim(drugsNORM)[1]-j]-drugsNORM[[i]][dim(drugsNORM)[1]-j-1] - diff_drug_mean[i-1] )^2 )
  }
  var_drug[[paste(colnames(drugsNORM)[i])]] <- add_up
}
var_drug <- var_drug %>% as.data.frame

var_drug_mean <- sapply(seq(1:dim(var_drug)[2]),function(x) mean(var_drug[[x]]) ) 

pred_drug <- NULL
pred_drug$year <- c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
for(i in seq(from = 2,to=dim(drugsNORM)[2])){
  zw <- tail(drugsNORM[[i]],1) 
  ran_zw <- rnorm(9,mean = diff_drug_mean[i-1],sd = sqrt(var_drug_mean[i-1]))
  for(j in 1:9) zw <- c(zw,tail(zw,1) + ran_zw[j])
  pred_drug[[paste(colnames(drugsNORM)[i])]] <- zw
}
pred_drug <- as.data.frame(pred_drug)





dif_pov <- NULL
for(i in seq(from = 2,to=dim(pov_regions)[2]))
{
  add_up <- NULL
  for(j in 1:dim(pov_regions)[1]){
    add_up <- c(add_up,(pov_regions[[i]][dim(pov_regions)[1]-j]-pov_regions[[i]][dim(pov_regions)[1]-j-1] ) )
  }
  dif_pov[[paste(colnames(pov_regions)[i])]] <- add_up
}
dif_pov <- dif_pov %>% as.data.frame

diff_pov_mean <- sapply(seq(1:dim(dif_pov)[2]),function(x) mean(dif_pov[[x]]) ) 

var_pov <- NULL
for(i in seq(from = 2,to=dim(pov_regions)[2]))
{
  add_up <- NULL
  for(j in 1:dim(pov_regions)[1]){
    add_up <- c(add_up,(pov_regions[[i]][dim(pov_regions)[1]-j]-pov_regions[[i]][dim(pov_regions)[1]-j-1] - diff_pov_mean[i-1] )^2 )
  }
  var_pov[[paste(colnames(pov_regions)[i])]] <- add_up
}
var_pov <- var_pov %>% as.data.frame

var_pov_mean <- sapply(seq(1:dim(var_pov)[2]),function(x) mean(var_pov[[x]]) ) 

pred_pov <- NULL
pred_pov$year <- c(seq(from = 2015, to =2020))
for(i in seq(from = 2,to=dim(pov_regions)[2])){
  zw <- tail(pov_regions[[i]],1) 
  ran_zw <- rnorm(5,mean = diff_pov_mean[i-1],sd = sqrt(var_pov_mean[i-1]))
  for(j in 1:5) zw <- c(zw,tail(zw,1) + ran_zw[j])
  pred_pov[[paste(colnames(pov_regions)[i])]] <- zw
}
pred_pov <- as.data.frame(pred_pov)


save(pred_drug,file="./pred_drug.RData")
save(pred_murder,file="./pred_murder.RData")

ggplot(data = povMurd,aes(x=pov,y=murder)) + geom_point() + stat_smooth(method = "lm", col = "red") + theme_bw()
ggplot(data = povdrugs,aes(x=pov,y=drugs)) + geom_point() + stat_smooth(method = "lm", col = "red")
ggplot(data = povdrugs,aes(x=pov,y=drugs)) + geom_point() + geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE) + theme_bw()

save(povMurd,file="./povMurder.RData")
save(povdrugs,file="./povDrugs.RData")


colnames(pred_drug) <- colnames(drugsNORM)
colnames(pred_murder) <- colnames(murder_regions)
colnames(pred_pov) <- colnames(pov_regions)
get_tog_murder <- rbind(murder_regions,pred_murder)
get_tog_drugs <- rbind(drugsNORM,pred_drug)
get_tog_pov <- rbind(pov_regions,pred_pov)
get_tog_murder <- subset(get_tog_murder, get_tog_murder$Year > 2003)
get_tog_pov <- subset(get_tog_pov, get_tog_pov$Year > 2003)
get_tog_pov <- get_tog_pov[,-c(1)]
get_tog_murder <- get_tog_murder[,-c(1)]
get_tog_drugs <- get_tog_drugs[,-c(1)]
get_tog_drugs$OTHERS <- NULL
get_tog_murder$OTHERS <- NULL
get_tog_pov$OTHERS <- NULL
boston<- cbind(get_tog_murder$BOSTON %>% as.matrix,get_tog_drugs$BOSTON %>% as.matrix,get_tog_pov$BOSTON)%>% as.data.frame

ind <- NULL
for(i in 1:dim(get_tog_murder)[2]){
  ind <- cbind(ind, get_tog_drugs[[paste(colnames(get_tog_murder)[i])]])
} 
get_tog_drugs <- ind %>% as.data.frame
colnames(get_tog_drugs) <- colnames(get_tog_murder)

save(get_tog_drugs,file = "./drugs_ani.RData")
save(get_tog_murder,file = "./murder_ani.RData")
save(get_tog_pov,file = "./pov_ani.RData")

Rosling.bubbles(x = get_tog_murder %>% as.matrix, y =get_tog_pov %>% as.matrix ,type = "circles", circles=  get_tog_drugs, text = 2004:2020 )
saveHTML({
  ani.options(interval = 0.2, nmax = 18,loop = T)
  Rosling.bubbles(x = get_tog_murder %>% as.matrix, y = get_tog_drugs %>% as.matrix ,type = "circles", circles= get_tog_pov, text = 2004:2020 ,
                  xlab = "Murder rate", ylab = " Drug abuse")})
