### Data structure: make all covariates data into one file #####
##############################
library(dplyr)
library(reshape)
library(lattice)
## read stuff ##
rm(list=ls(all=TRUE))
data<-read.csv("data/All_widedata/wildboar_datawide_n.csv",header=TRUE) # detection data of muntjac 
sampcovs<-read.csv("data/wildboar_datawide_camhour.csv",header=TRUE) # sampling occasion data for muntjac
datalong<-read.csv("data/All_longdata/wildboar_long.csv",header=TRUE) # long format detection data of muntjac 
qs <- read.csv("data/QS_102.csv", stringsAsFactors = FALSE, header=TRUE) # orignial questionnaire data
sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE) # orignial site covaite data ie. elevation, distance & population 
stationTemp <- datalong$station
station <- unique(stationTemp) #remove duplicates

###########################  park enforcement variables #################################################
## 

## making enforcement level variables ##
enforce <-data.frame(cbind(qs$NO, qs$PAs, qs$Village, qs$Time_of_outreach, qs$No_of_Punishment, qs$Relationship, qs$Score_of_Punishment_Of_P,
                           qs$Score_of_Punishment_Of_H))
names(enforce) <- c("NO", "PAs", "Village", "Time_of_outreach", "No_of_Punishment", "Relationship", "Score_of_Punishment_Of_P",
                    "Score_of_Punishment_Of_H")
pa <- tolower(c("BULANGSHAN", "MANGAO", "MENGLA", "MENGLUN", "MENGSONG", "NABANHE"))

## 
reach <- aggregate(qs$Time_of_outreach ~ qs$PAs, FUN = "mean")
punish <- aggregate(qs$No_of_Punishment ~ qs$PAs, FUN = "mean")
relationship <- aggregate(qs$Relationship ~ qs$PAs, FUN = "mean")
Score.P <-aggregate(qs$Score_of_Punishment_Of_P ~ qs$PAs, FUN = "mean")
Score.H <-aggregate(qs$Score_of_Punishment_Of_H ~ qs$PAs, FUN = "mean")

# demographic variable
# education 
edu <- table(qs$PAs, qs$Education)
edu <- as.data.frame.matrix(edu)
names(edu) <- c("Elementary", "High", "Middle", "None", "Secondary", "Temple")
head(edu)
edu <- edu[c(4,1,3,2,5,6)]
edu <- edu[,0:5]
wt <- c(0, 4, 7, 10, 13)
wtedu <- data.frame()
for(i in 1:6) {
  j <- as.data.frame(sum(edu[i,] * wt)/sum(edu[i,]))
  wtedu <- dplyr::bind_rows(wtedu, j)
}
wtedu <- cbind(pa, wtedu)
names(wtedu)<-c("pas", "wtedu")

# income 
income <- aggregate(qs$Income ~ qs$PAs, FUN = "mean")
# new data frame
anthrop <- data.frame(pa, reach[,2], punish[,2], relationship[,2], Score.P[,2], Score.H[,2], wtedu[,2],income[,2])
names(anthrop) <- c("pas","reach","punish","relationship", "score.P","score.H", "edu", "income")

## change lvshilin to menglun
lv <- which(sitecovs$PAS == "Lvshilin")
sitecovs$PAS[lv] <- "menglun"
sitecovs$PAS <- tolower(sitecovs$PAS)

# adding new variables
anthrop.mx <- matrix(999, ncol = 8, nrow = nrow(sitecovs))
for(i in 2:8) {
  for(j in 1:6){
    n <- which(sitecovs$PAS == anthrop$pas[j])
    anthrop.mx[n,i] <- anthrop[j,i]
  }
}

anthrop <- data.frame(anthrop.mx)
anthrop[,1] <- sitecovs$PAS
anthrop <- data.frame(tolower(sitecovs$NO), anthrop)
names(anthrop) <- c("no", "pas","reach","punish","relationship", "score.P","score.H", "edu", "income")

#############################################################################

stationNameTable<-matrix(999, length(station), 2, byrow=T)
for(i in 1:length(station)) {      
  stationNameTable[i,1]<-toString(station[i])
}
stationN <- station[1]
dayCount <- 0
j <- 1

for(i in 1:length(stationTemp)) {
  if (stationN == stationTemp[i]) {
    dayCount <- dayCount + 1
  } else {
    stationN <- stationTemp[i]
    stationNameTable[j,2] <- dayCount
    j <- j + 1
    dayCount <- 1
  }
  if (i == length(stationTemp)) {
    stationNameTable[j,2] <- dayCount
  }
}

####
station <- stationNameTable[,1]
camdays <- as.numeric(stationNameTable[,2])
## read stuff ##

#####################################
## edit site covariables ##
maxdays<-max(camdays)
numsites<-length(station)
depvar<-as.matrix(data[,2:(maxdays+1)])
q <- which(depvar > 1)
depvar[q] <- 1
camhours<-as.matrix(sampcovs[,2:(maxdays+1)])

#### 
#### standarize all variables for RN model #####

elevation <- scale(sitecovs$ele)
population <- scale(sitecovs$pop3000m)
distance <- scale(sitecovs$dis)
pasize <- scale(sitecovs$size.m)
outreach <- scale(anthrop$reach)
punishment <- scale(anthrop$punish)

cam_angle <- sitecovs$Cam_angle
pas <- sitecovs$PAS

anthrop$park.ind <- pas

dec.sitecov <- data.frame(elevation,population,distance,pasize,outreach,punishment,cam_angle,pas)
park <- anthrop$pas
park[which(park == "mengla")] <- 3
park[which(park == "mangao")] <- 2
park[which(park== "menglun")] <- 4
park[which(park == "bulangshan")] <- 1
park[which(park == "nabanhe")] <- 6
park[which(park == "mengsong")] <- 5
park <- as.numeric(park)
dec.sitecov$park.ind <- park
STDED.dec.sitecov <- dec.sitecov

write.csv(STDED.dec.sitecov,file="data/final/STDED.DEC.sitecov.csv")

J <- j # number of sites 
K <- length(data)-1

dec.data <- list(y=depvar, J=J, K=K, park.ind=park, ele=elevation,population=population, distance=distance, parksize=pasize, outreach=outreach, punishment=punishment,
                 cam_angle=cam_angle)

write.csv(dec.data, file = "data/final/dec.data.wildboar.csv")
