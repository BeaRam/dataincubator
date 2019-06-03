#Script for Citibike Data Analysis

setwd("/Volumes/Terra/CitiBike")

BikeJan <- read.csv("201501-citibike-tripdata.csv")
BikeFeb <- read.csv("201502-citibike-tripdata.csv")
BikeMar <- read.csv("201503-citibike-tripdata.csv")
BikeApr <- read.csv("201504-citibike-tripdata.csv")
BikeMay <- read.csv("201505-citibike-tripdata.csv")
BikeJun <- read.csv("201506-citibike-tripdata.csv")
BikeJul <- read.csv("201507-citibike-tripdata.csv")
BikeAug <- read.csv("201508-citibike-tripdata.csv")
BikeSep <- read.csv("201509-citibike-tripdata.csv")
BikeOct <- read.csv("201510-citibike-tripdata.csv")
BikeNov <- read.csv("201511-citibike-tripdata.csv")
BikeDec <- read.csv("201512-citibike-tripdata.csv")

JanTD <- BikeJan$tripduration
FebTD <- BikeFeb$tripduration
MarTD <- BikeMar$tripduration
AprTD <- BikeApr$tripduration
MayTD <- BikeMay$tripduration
JunTD <- BikeJun$tripduration
JulTD <- BikeJul$tripduration
AugTD <- BikeAug$tripduration
SepTD <- BikeSep$tripduration
OctTD <- BikeOct$tripduration
NovTD <- BikeNov$tripduration
DecTD <- BikeDec$tripduration

YearTD <- list( vec1 = c(JanTD, FebTD, MarTD, AprTD, MayTD, JunTD, JulTD, AugTD, SepTD, OctTD, NovTD, DecTD))

YearTDMed <- lapply(YearTD, median)
YearTDMed

##Median Trip Duration: 629

YearTD2 <- list(JanTD, FebTD, MarTD, AprTD, MayTD, JunTD, JulTD, AugTD, SepTD, OctTD, NovTD, DecTD)

EachMonthTDAvg <- lapply(YearTD2, mean)
DiffAvg <- max(unlist(EachMonthTDAvg)) - min(unlist(EachMonthTDAvg))
DiffAvg

##Trip duration average difference: 430.5703

JanBID <- BikeJan$bikeid
FebBID <- BikeFeb$bikeid
MarBID <- BikeMar$bikeid
AprBID <- BikeApr$bikeid
MayBID <- BikeMay$bikeid
JunBID <- BikeJun$bikeid
JulBID <- BikeJul$bikeid
AugBID <- BikeAug$bikeid
SepBID <- BikeSep$bikeid
OctBID <- BikeOct$bikeid
NovBID <- BikeNov$bikeid
DecBID <- BikeDec$bikeid
YearBikeID <- list(vec1 = c(JanBID, FebBID, MarBID, AprBID, MayBID, JunBID, JulBID, AugBID, SepBID, OctBID, NovBID, DecBID))
sd(table(YearBikeID))

##Standard Deviation of Bikes used in 2015: 328.9118

JanUT <- BikeJan$usertype 
FebUT <- BikeFeb$usertype 
MarUT <- BikeMar$usertype 
AprUT <- BikeApr$usertype 
MayUT <- BikeMay$usertype 
JunUT <- BikeJun$usertype 
JulUT <- BikeJul$usertype 
AugUT <- BikeAug$usertype 
SepUT <- BikeSep$usertype 
OctUT <- BikeOct$usertype 
NovUT <- BikeNov$usertype 
DecUT <- BikeDec$usertype 
install.packages("plyr")
library(plyr)

BikeJanSub <- data.frame(table(BikeJan$tripduration > 2700, BikeJan$usertype == "Subscriber"))
BikeFebSub <-data.frame(table(BikeFeb$tripduration > 2700, BikeFeb$usertype == "Subscriber"))
BikeMarSub <-data.frame(table(BikeMar$tripduration > 2700, BikeMar$usertype == "Subscriber"))
BikeAprSub <-data.frame(table(BikeApr$tripduration > 2700, BikeApr$usertype == "Subscriber"))
BikeMaySub <-data.frame(table(BikeMay$tripduration > 2700, BikeMay$usertype == "Subscriber"))
BikeJunSub <-data.frame(table(BikeJun$tripduration > 2700, BikeJun$usertype == "Subscriber"))
BikeJulSub <-data.frame(table(BikeJul$tripduration > 2700, BikeJul$usertype == "Subscriber"))
BikeAugSub <-data.frame(table(BikeAug$tripduration > 2700, BikeAug$usertype == "Subscriber"))
BikeSepSub <-data.frame(table(BikeSep$tripduration > 2700, BikeSep$usertype == "Subscriber"))
BikeOctSub <-data.frame(table(BikeOct$tripduration > 2700, BikeOct$usertype == "Subscriber"))
BikeNovSub <-data.frame(table(BikeNov$tripduration > 2700, BikeNov$usertype == "Subscriber"))
BikeDecSub <-data.frame(table(BikeDec$tripduration > 2700, BikeDec$usertype == "Subscriber"))

TotalLengthYear <- length(BikeJan) + length (BikeFeb) + length(BikeMar) + length(BikeApr) + length(BikeMay) + length(BikeJun) + length(BikeJul) + length(BikeAug) + length(BikeSep) + length(BikeOct) + length(BikeNov) + length(BikeDec)
TotalLengthYear

SubFreq <- (BikeJanSub[[4, 3]] + BikeFebSub[[4, 3]] + BikeMarSub[[4, 3]] + BikeAprSub[[4, 3]] + BikeMaySub[[4, 3]] + BikeJunSub[[4, 3]] + BikeJulSub[[4, 3]] + BikeAugSub[[4, 3]] + BikeSepSub[[4, 3]] + BikeOctSub[[4, 3]] + BikeNovSub[[4, 3]] + BikeDecSub[[4, 3]]) 
SubFreq
BikeJanCust <- data.frame(table(BikeJan$tripduration > 1800, BikeJan$usertype == "Customer"))
BikeFebCust <- data.frame(table(BikeFeb$tripduration > 1800, BikeFeb$usertype == "Customer"))
BikeMarCust <- data.frame(table(BikeMar$tripduration > 1800, BikeMar$usertype == "Customer"))
BikeAprCust <- data.frame(table(BikeApr$tripduration > 1800, BikeApr$usertype == "Customer"))
BikeMayCust <- data.frame(table(BikeMay$tripduration > 1800, BikeMay$usertype == "Customer"))
BikeJunCust <- data.frame(table(BikeJun$tripduration > 1800, BikeJun$usertype == "Customer"))
BikeJulCust <- data.frame(table(BikeJul$tripduration > 1800, BikeJul$usertype == "Customer"))
BikeAugCust <- data.frame(table(BikeAug$tripduration > 1800, BikeAug$usertype == "Customer"))
BikeSepCust <- data.frame(table(BikeSep$tripduration > 1800, BikeSep$usertype == "Customer"))
BikeOctCust <- data.frame(table(BikeOct$tripduration > 1800, BikeOct$usertype == "Customer"))
BikeNovCust <- data.frame(table(BikeNov$tripduration > 1800, BikeNov$usertype == "Customer"))
BikeDecCust <- data.frame(table(BikeDec$tripduration > 1800, BikeDec$usertype == "Customer"))
CustFreq <- (BikeJanCust[[4, 3]] + BikeFebCust[[4, 3]] + BikeMarCust[[4, 3]] + BikeAprCust[[4, 3]] + BikeMayCust[[4, 3]] + BikeJunCust[[4, 3]] + BikeJulCust[[4, 3]] + BikeAugCust[[4, 3]] + BikeSepCust[[4, 3]] + BikeOctCust[[4, 3]] + BikeNovCust[[4, 3]] + BikeDecCust[[4, 3]])
ExceedTD <- (SubFreq + CustFreq) / TotalLengthYear
ExceedTD

##Percentage of Trips that exceed the allotted 30 mins: 0.03810678

StationDups <- function(x) {sum(x$start.station.id == x$end.station.id)}
JanDups <- StationDups(BikeJan)
FebDups <- StationDups(BikeFeb)
MarDups <- StationDups(BikeMar)
AprDups <- StationDups(BikeApr)
MayDups <- StationDups(BikeMay)
JunDups <- StationDups(BikeJun)
JulDups <- StationDups(BikeJul)
AugDups <- StationDups(BikeAug)
SepDups <- StationDups(BikeSep)
OctDups <- StationDups(BikeOct)
NovDups <- StationDups(BikeNov)
DecDups <- StationDups(BikeDec)
TotalStationDups <- (JanDups + FebDups + MarDups + AprDups + MayDups + JunDups + JulDups + AugDups + SepDups + OctDups + NovDups + DecDups) / TotalLengthYear
TotalStationDups

##Percentage of Trips that begin & end at same station: 0.02235839

install.packages("geosphere")
library(geosphere)
gcd.slc <- function(long1, lat1, long2, lat2) {R <- 6371; d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R; return(d) }
YearTotalDistance <- gcd.slc(BikeJan$start.station.longitude, BikeJan$start.station.latitude, BikeJan$end.station.longitude, BikeJan$end.station.latitude) + gcd.slc(BikeFeb$start.station.longitude, BikeFeb$start.station.latitude, BikeFeb$end.station.longitude, BikeFeb$end.station.latitude) + gcd.slc(BikeMar$start.station.longitude, BikeMar$start.station.latitude, BikeMar$end.station.longitude, BikeMar$end.station.latitude) + gcd.slc(BikeApr$start.station.longitude, BikeApr$start.station.latitude, BikeApr$end.station.longitude, BikeApr$end.station.latitude) + gcd.slc(BikeMay$start.station.longitude, BikeMay$start.station.latitude, BikeMay$end.station.longitude, BikeMay$end.station.latitude) + gcd.slc(BikeJun$start.station.longitude, BikeJun$start.station.latitude, BikeJun$end.station.longitude, BikeJun$end.station.latitude) + gcd.slc(BikeJul$start.station.longitude, BikeJul$start.station.latitude, BikeJul$end.station.longitude, BikeJul$end.station.latitude) + gcd.slc(BikeAug$start.station.longitude, BikeAug$start.station.latitude, BikeAug$end.station.longitude, BikeAug$end.station.latitude) + gcd.slc(BikeSep$start.station.longitude, BikeSep$start.station.latitude, BikeSep$end.station.longitude, BikeSep$end.station.latitude) + gcd.slc(BikeOct$start.station.longitude, BikeOct$start.station.latitude, BikeOct$end.station.longitude, BikeOct$end.station.latitude) + gcd.slc(BikeNov$start.station.longitude, BikeNov$start.station.latitude, BikeNov$end.station.longitude, BikeNov$end.station.latitude) gcd.slc(BikeDec$start.station.longitude, BikeDec$start.station.latitude, BikeDec$end.station.longitude, BikeDec$end.station.latitude)
TotalAvgDist <- mean(YearTotalDist, na.rm = TRUE)
TotalAvgDist

##Total Average Distance: 1282.534 km

NotDups <- function(x) { sum(x$start.station.id != x$end.station.id, na.rm = TRUE)}
TotalNotDups <- NotDups(BikeJan) + NotDups(BikeFeb) + NotDups(BikeMar) + NotDups(BikeApr) + NotDups(BikeMay) + NotDups(BikeJun) + NotDups(BikeJul) + NotDups(BikeAug) + NotDups(BikeSep) + NotDups(BikeOct) + NotDups(BikeNov) + NotDups(BikeDec)
AvgTotalNotDups <- mean(TotalNotDups, na.rm = TRUE)
AvgTotalNotDups

##Average number of bike rides that begin and end at different stations: 9715772


TotalDuration <- sum(as.numeric(BikeJan$tripduration, BikeFeb$tripduration, BikeMar$tripduration, BikeApr$tripduration, BikeMay$tripduration, BikeJun$tripduration, BikeJul$tripduration, BikeAug$tripduration, BikeSep$tripduration, BikeOct$tripduration, BikeNov$tripduration , BikeDec$tripduration))
TotalDuration
maxJanTD <- max(as.numeric(BikeJan$tripduration))
maxFebTD <- max(as.numeric(BikeFeb$tripduration))
maxMarTD <- max(as.numeric(BikeMar$tripduration))
maxAprTD <- max(as.numeric(BikeApr$tripduration))
maxMayTD <- max(as.numeric(BikeMay$tripduration))
maxJunTD <- max(as.numeric(BikeJun$tripduration))
maxJulTD <- max(as.numeric(BikeJul$tripduration))
maxAugTD <- max(as.numeric(BikeAug$tripduration))
maxSepTD <- max(as.numeric(BikeSep$tripduration))
maxOctTD <- max(as.numeric(BikeOct$tripduration))
maxNovTD <- max(as.numeric(BikeNov$tripduration))
maxDecTD <- max(as.numeric(BikeNov$tripduration))
maxofAll <- max(c(maxJanTD, maxFebTD, maxMarTD, maxAprTD, maxMayTD, maxJunTD, maxJulTD, maxAugTD, maxSepTD, maxOctTD, maxNovTD, maxDecTD), na.rm = TRUE )
maxofAll/TotalDuration

##Max Trip Duration/Total Bike Use: 0.03138801
