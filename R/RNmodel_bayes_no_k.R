# RN model with park random effects #
library(dplyr)
library(R2jags)

### 3-demensinal data
####
rm(list=ls(all=TRUE))
list.filenames <- list.files(path = "data/All_widedata", pattern="*.csv")
list.filenames <- paste(c("data/All_widedata/"), list.filenames, sep="")
# create an empty list that will serve as a container to receive the incoming files
spp.temp <- c("blackbear","brushtailedporcupine", "chineseferretbadger",  "commonmacaque", "commonpalmcivet","crabeatingmongoose", "camhour","dhole",               
              "gaur", "goral","hogbadger","leopardcat","maskedpalmcivet", "muntjac", "pigtailedmacaque", "porcupine", "sambar", "serow", "smallindiancivet", 
              "spotedlinsang", "weasel", "wildboar",   "yellowthroatedmarten")
spp <- spp.temp[-7]
list.data<-list()
# create a loop to read in your data
for (i in 1:length(list.filenames)){
  a <- read.csv(list.filenames[i])
  a <- a[-1]
  a <- as.matrix(a)
  colnames(a) <- NULL
  list.data[[i]]<-a
}

names(list.data)<-spp.temp
camhour <- as.data.frame(list.data[7]) # save camhour data
a <- read.csv(list.filenames[1]) 
station <- a[,1] # GET colnames of camhour 
a <- a[-1] # remove "station"
colnames(camhour) <- colnames(a)
list.data <- list.data[-7] # remove camhour from spp list

#1. "blackbear", 2. "brushtailedporcupine", 3. "chineseferretbadger",  4. "commonmacaque", 5. "commonpalmcivet", 6. "crabeatingmongoose", 7. "dhole",               
#8. "gaur", 9. "goral", 10. "hogbadger", 11. "leopardcat", 12. "maskedpalmcivet", 13. "muntjac", 14. "pigtailedmacaque", 15. "porcupine", 16."sambar",
#17. "serow", 18. "smallindiancivet", 19. "spotedlinsang", 20. "weasel", 21. "wildboar", 22. "yellowthroatedmarten"

##########################################################################
##########################################################################

data <- list.data[[21]] 
# choose species 
sampcovs<-read.csv("data/wildboar_datawide_camhour.csv",header=TRUE, strip.white=TRUE)
datalong<-read.csv("data/All_longdata/wildboar_long.csv",header=TRUE, strip.white=TRUE) 
qs <- read.csv("data/QS_102.csv", stringsAsFactors = FALSE, header=TRUE, strip.white=TRUE)
sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
stationTemp <- datalong$station
station <- unique(stationTemp) #remove duplicates

###########################  Anthropological variables  #################################################

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
depvar<-data
site.det <- rowSums(depvar, na.rm =T)
q <- which(site.det > 0)
site.det[q] <- 1 

camhours<-as.matrix(sampcovs[,2:(maxdays+1)])
camhours <- rowSums(camhour, na.rm =T)

##### make every factor multiply pop3000m then standardized ###
pop.reach <- sitecovs$pop3000m * anthrop$reach
popREACH.mean <- mean(pop.reach)
popREACH.sd <- sd(pop.reach)
popREACH <- (pop.reach-popREACH.mean)/popREACH.sd

pop.punish <- sitecovs$pop3000m * anthrop$punish
popPUN.mean <- mean(pop.punish)
popPUN.sd <- sd(pop.punish)
popPUN <- (pop.punish-popPUN.mean)/popPUN.sd

pop.r <- sitecovs$pop3000m * anthrop$relationship
popR.mean <- mean(pop.r)
popR.sd <- sd(pop.r)
popR <- (pop.r-popR.mean)/popR.sd

pop.sp <- sitecovs$pop3000m * anthrop$score.P
popSP.mean <- mean(pop.sp)
popSP.sd <- sd(pop.sp)
popSP <- (pop.sp-popSP.mean)/popSP.sd

pop.sh <- sitecovs$pop3000m * anthrop$score.H
popSH.mean <- mean(pop.sh)
popSH.sd <- sd(pop.sh)
popSH <- (pop.sh-popSH.mean)/popSH.sd

pop.edu <- sitecovs$pop3000m * anthrop$edu
popEDU.mean <- mean(pop.edu)
popEDU.sd <- sd(pop.edu)
popEDU <- (pop.edu-popEDU.mean)/popEDU.sd

pop.income <- sitecovs$pop3000m * anthrop$income
popIN.mean <- mean(pop.income)
popIN.sd <- sd(pop.income)
popIN <- (pop.income-popIN.mean)/popIN.sd

#### standarize anthrop #####
size.ave <- mean(sitecovs$size.m)
size.sd <- sd(sitecovs$size.m)
pasize <- (sitecovs$size.m-size.ave)/size.sd

punish.ave <- mean(anthrop$punish)
punish.sd <- mean(anthrop$punish)
punish <- (anthrop$punish - punish.ave)

reach.ave <- mean(anthrop$reach)
reach.sd <- sd(anthrop$reach)
reach<- (anthrop$reach - reach.ave)/reach.sd

relationship.ave <- mean(anthrop$relationship)
relationship.sd <- sd(anthrop$relationship)
relationship <- (anthrop$relationship - relationship.ave)/relationship.sd

score.P.ave <- mean(anthrop$score.P)
score.P.sd <- sd(anthrop$score.P)
score.P <- (anthrop$score.P - score.P.ave)/score.P.sd

score.H.ave <- mean(anthrop$score.H)
score.H.sd <- sd(anthrop$score.H)
score.H <- (anthrop$score.H - score.H.ave)/score.H.sd

edu.ave <- mean(anthrop$edu)
edu.sd <- sd(anthrop$edu)
edu <- (anthrop$edu - edu.ave)/edu.sd

income.ave <- mean(anthrop$income)
income.sd <- sd(anthrop$income)
income <- (anthrop$income - income.ave)/income.sd

pop.mean3000 <- mean(sitecovs$pop3000m)
pop.sd3000 <- sd(sitecovs$pop3000m)
pop3000.s <- (sitecovs$pop3000m-pop.mean3000)/pop.sd3000 

park <- anthrop$pas
park[which(park == "mengla")] <- 3
park[which(park == "mangao")] <- 2
park[which(park== "menglun")] <- 4
park[which(park == "bulangshan")] <- 1
park[which(park == "nabanhe")] <- 6
park[which(park == "mengsong")] <- 5
park <- as.numeric(park)


######## BAYESIAN MODELING ########

################# Observation cov ##########
#standardize camhour covs....
camhours.mean<-mean(na.omit(camhours)) #gets rid of NAs in data
camhours.sd<-sd(na.omit(camhours))
camhours.z<-(camhours-camhours.mean)/camhours.sd
#na <- which(is.na(camhours.z[,]) == TRUE)
#camhours.z[na] <- camhours.z[1000]

# make cam angle sheet 

cam_angle <- sampcovs
cam_angle$station <- as.character(cam_angle$station)
sitecovs.c <- sitecovs
sitecovs.c$NO <- tolower(as.character(sitecovs.c$NO))

for(i in 1:nrow(sitecovs)) {
  if(cam_angle[i,1] == sitecovs.c$NO[i]) {
    p <- which(is.na(cam_angle[i,]) == FALSE)
   # cam_angle[i,p] <- sitecovs$Cam_angle[i]
    cam_angle[i,] <- sitecovs$Cam_angle[i] # turn all NAs to 1
  } 
}
cam_angle.z <- as.matrix(cam_angle[,-1])
cam_angle.z <- cam_angle.z[,1]
#obsCovs <- list(camhours=camhours.z, cam_angle=cam_angle.z) ## obsercation cov

################# Observation cov ##########
### site cov ####

#sitecov.z <- data.frame(park, sitecovs$ele.s, sitecovs$dis.s, pasize, reach, punish, relationship, score.P, score.H,edu,income,
#                        popREACH, popPUN, popR, popSP, popSH, popEDU, popIN, pop3000.s) # sitecovs$ele.s, sitecovs$pop500.s, pop3000.s,
# sitecovs$size.m, pop.p, puns, sitecovs$wtedu, pop.ed) # popP = pop500 * (1-pun)  
#str(sitecov.z)
#names(sitecov.z) <- c("park.ind","ele.s","dis", "PAsize", "reach","punish","relationship", "score.P","score.H", "edu", 
#                     "income", "popREACH", "popPUN", "popR", "popSP", "popSH", "popEDU", "popIN", "pop3000.s")

#str(sitecov.z)
#head(sitecovs)

### site cov ####

J <- j # number of sites 
#K <- length(cam_angle)-1

dec.data <- list(w=site.det, J=J, # K=K, 
                 park.ind=park, 
                 ele=sitecovs$ele.s, dis=sitecovs$dis.s, parksize=pasize, outreach=reach, punishment=punish,
                 cam_angle=cam_angle.z) 
                #camhour=camhours.z)
#dec.data.qou <- list("w", "J", "K", "park.ind","ele", )

dec.params <- c("N","lambda","r", "p", #"w", "p","r","N","lambda",
                "mu.a1","mu.a2","mu.a3","mu.a4","mu.a5","mu.a6",
                "mu.b1","mu.b2","mu.b3", "park")
#                "sigma.a1","sigma.a2","sigma.a3","sigma.a4","sigma.a5","sigma.a6",
#               "sigma.b1","sigma.b2","sigma.b3",
#               "a1", "a2","a3","a4","a5","a6",
#                "b1","b2","b3",
#                "park","sd.p"


modelFile="R/RNmodel_bayes_no_k.txt"

# initial 
dec.inits = function(){ list (
  mu.a1=rnorm(1),mu.a2=rnorm(1),mu.a3=rnorm(1),mu.a4=rnorm(1),mu.a5=rnorm(1),mu.a6=rnorm(1),
  mu.b1=rnorm(1),mu.b2=rnorm(1),mu.b3=rnorm(1),
  w=site.det #,N=dpois(j,lambda = 0.25), ### ?????????
 # sigma.a1=dunif(1,min=0.5,max=1),sigma.a2=dunif(1,min=0.5,max=1),sigma.a3=dunif(1,min=0.5,max=1),
#  sigma.a4=dunif(1,min=0.5,max=1),sigma.a5=dunif(1,min=0.5,max=1),sigma.a6=dunif(1,min=0.5,max=1),
#  sigma.b1=dunif(1,min=0.5,max=1),sigma.b2=dunif(1,min=0.5,max=1),sigma.b3=dunif(1,min=0.5,max=1)
  )}
#  w
#}

fit <- jags(dec.data, dec.inits, dec.params, model.file=modelFile,     # test run
            n.chains=3, n.iter=50000, n.burnin=20000, n.thin=20)

## results ###
library("mcmcplots")
library("ggplot2")
par <- c("mu.a1","mu.a2","mu.a3","mu.a4","mu.a5","mu.a6",
        "mu.b1","mu.b2","mu.b3","park")
#par <- c("park")
#a1 <- fit$BUGSoutput$sims.list$mu.a1

mcmcplot(fit, parm=par)

caterplot(fit, c("mu.a1","mu.a2","mu.a3","mu.a4","mu.a5","mu.a6",
                "mu.b1","mu.b2","mu.b3","park"))
  
r <- fit$BUGSoutput$sims.list$r
N <- fit$BUGSoutput$sims.list$N
mean(N)
mean(r)
#mu.a1 <- fit$BUGSoutput$sims.list$mu.a1


