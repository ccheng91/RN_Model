library(reshape)
library(lattice)
library(Rcpp)
library(unmarked)
library(AICcmodavg)
library(dplyr)

## read stuff ##
rm(list=ls(all=TRUE))
data<-read.csv("data/All_widedata/muntjac_datawide_n.csv",header=TRUE)
sampcovs<-read.csv("data/muntjac_datawide_camhour.csv",header=TRUE)
datalong<-read.csv("data/All_longdata/muntjac_long.csv",header=TRUE) 
qs <- read.csv("data/QS_102.csv", stringsAsFactors = FALSE, header=TRUE)
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
depvar<-as.matrix(data[,2:(maxdays+1)])
q <- which(depvar > 1)
depvar[q] <- 1
camhours<-as.matrix(sampcovs[,2:(maxdays+1)])

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
punish.sd <- sd(anthrop$punish)
punish <- (anthrop$punish - punish.ave)/punish.sd

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

park <- anthrop$pas


#reach    punish relationship   score.P   score.H      edu   income


str(anthrop)
str(sitecovs)
#library(psych)
#pairs.panels(sitecovs[1:4])
#head(sitecovs)


#pop.mean <- mean(sitecovs$pop.a)
#pop.sd <- sd(as.vector(sitecovs$pop.a))
#pop.p <- (sitecovs$pop.a-pop.mean)/pop.sd ## pop ajusted by punishment score and standardized 
#forest<-sitecovs$forest_type
#puns <- 1-sitecovs$pun

##### 
## pop 3000 standized 
pop.mean3000 <- mean(sitecovs$pop3000m)
pop.sd3000 <- sd(sitecovs$pop3000m)
pop3000.s <- (sitecovs$pop3000m-pop.mean3000)/pop.sd3000 

# pop3000m * edu and standardize ## 

#forest<-sitecovs$forest_type
#dfg.mean<-mean(sitecovs$mass.dipt) #dipt fruits collected from ground
#dfg.sd<-sd(sitecovs$mass.dipt)
#dfg.z<-(sitecovs$mass.dipt-dfg.mean)/dfg.sd
#####


sitecov.z <- data.frame(sitecovs$ele.s, sitecovs$dis.s, pasize, reach, punish, relationship, score.P, score.H,edu,income,
                        popREACH, popPUN, popR, popSP, popSH, popEDU, popIN, pop3000.s, park) # sitecovs$ele.s, sitecovs$pop500.s, pop3000.s,
# sitecovs$size.m, pop.p, puns, sitecovs$wtedu, pop.ed) # popP = pop500 * (1-pun)  
str(sitecov.z)
names(sitecov.z) <- c("ele.s","dis", "PAsize", "reach","punish","relationship", "score.P","score.H", "edu", 
                      "income", "popREACH", "popPUN", "popR", "popSP", "popSH", "popEDU", "popIN", "pop3000.s","park")

str(sitecov.z)
head(sitecovs)


################# Observation cov ##########
#standardize camhour covs....
camhours.mean<-mean(na.omit(camhours)) #gets rid of NAs in data
camhours.sd<-sd(na.omit(as.vector(camhours)))
camhours.z<-(camhours-camhours.mean)/camhours.sd

# make cam angle sheet 

cam_angle <- sampcovs
cam_angle$station <- as.character(cam_angle$station)
sitecovs.c <- sitecovs
sitecovs.c$NO <- tolower(as.character(sitecovs.c$NO))
for(i in 1:nrow(sitecovs)) {
  if(cam_angle[i,1] == sitecovs.c$NO[i]) {
    p <- which(is.na(cam_angle[i,]) == FALSE)
    cam_angle[i,p] <- sitecovs$Cam_angle[i]
  } 
}
cam_angle.z <- as.matrix(cam_angle[,2:(maxdays+1)])

obsCovs <- list(camhours=camhours.z, cam_angle=cam_angle.z) ## obsercation cov

################# observation cov ##############
################ load umf #####################

data.umf<-unmarkedFrameOccu(y=depvar,siteCovs=sitecov.z, obsCovs=obsCovs)
###########################
m001<-occuRN(~1 ~1, data.umf)
m002<-occuRN(~camhours + cam_angle ~ele.s + park, data.umf)
m003<-occuRN(~camhours + cam_angle ~ele.s + dis+ park, data.umf)
m004<-occuRN(~camhours + cam_angle ~ele.s + dis + pop3000.s+ park, data.umf)
m005<-occuRN(~camhours + cam_angle ~ele.s + dis + PAsize+ park, data.umf)
m006<-occuRN(~camhours + cam_angle ~ele.s + dis + PAsize + pop3000.s+ park, data.umf)
m007<-occuRN(~camhours + cam_angle ~ele.s + dis + PAsize + reach + punish+ park, data.umf)

m008<-occuRN(~camhours + cam_angle ~ele.s + dis + reach + punish+ park, data.umf)
m009<-occuRN(~camhours + cam_angle ~ele.s + dis + pop3000.s + reach + punish+ park, data.umf)
m010<-occuRN(~camhours + cam_angle ~ele.s + dis + PAsize + pop3000.s + reach + punish+ park, data.umf)

#m031<-occuRN(~camhours + cam_angle ~ ele.s + PAsize + reach + punish + relationship + edu, data.umf)
#m032<-occuRN(~camhours + cam_angle ~ ele.s + PAsize + reach + punish + relationship + income, data.umf)
#m019<-occuRN(~camhours + cam_angle ~ ele.s + PAsize + dis + edu + income, data.umf)
#m020<-occuRN(~camhours + cam_angle ~ ele.s + PAsize + dis + pop3000.s +reach + punish , data.umf)
#m012<-occuRN(~camhours + cam_angle ~ele.s + distP.v + distSH.v, data.umf)
#m013<-occuRN(~camhours + cam_angle ~distP.v , data.umf)
#m014<-occuRN(~camhours + cam_angle ~distSH.v, data.umf)
#m015<-occuRN(~camhours + cam_angle ~distP.v + distSH.v, data.umf)

#.s <- as.numeric(c(coef(m004)[1:9]))
#s.s <- as.numeric(coef(m005)[1:10])
#sitecov.z$ele.s
#coef(m003)

#m006<-occu(~camhours ~1, data.umf,engine="C")
#m007<-occu(~camhours ~1 , data.umf,engine="C")
#m008<-occu(~camhours ~sitecovs.dis, data.umf,engine="C")
#m009<-occu(~camhours ~sitecovs.ele_stand +sitecovs.dis, data.umf,engine="C" )
#m010<-occu(~camhours ~sitecovs.ele_stand, data.umf,engine="C")
#m011<-occu(~camhours ~sitecovs.ele_stand +sitecovs.dis + sitecovs.cam_angle, data.umf,engine="C")
#m012<-occu(~camhours ~sitecovs.ele_stand +sitecovs.dis + sitecovs.cam_angle + sitecovs.ridge,data.umf,engine="C")
#m013<-occu(~camhours ~sitecovs.ele_stand *sitecovs.dis, data.umf,engine="C" )
#Cands<-list(m001,m002,m003,m004,m005, m006,m007, m008,m009,m010, m011, m012,m013,m014,m015,m016,m017, m018,m019,m020)#,
            #m021,m022)#,m023,m024,m025,m026,m027,m028,m029,m030,m031,m032,m033,m034,m035) #) # number of models
#mnames <- c("m001","m002","m003","m004","m005","m006","m007","m008","m009","m010", "m011", "m012","m013","m014", "m015","m016","m017", "m018","m019" ,"m020")#
#            ,"m021","m022")#,"m023","m024","m025","m026","m027","m028","m029","m030","m031","m032","m033","m034","m035")
#ff <- fitList("m001"= m001, "m002"=m002, "m003" =m003,"m004"=m004, "m005"=m005,"m006"=m006, "m007"=m007,"m008"=m008,"m009"=m009,"m010"=m010,"m011"=m011,
#              "m012"=m012,"m013"=m013,"m014"=m014, "m015"=m015, "m016" =m016,"m017"=m017,"m018"=m018,"m019"=m019, "m020"=m020)#,
             #"m021"=m021,"m022"=m022)#,"m023"=m023,"m024"=m024,"m025"=m025,"m026"=m026,"m027"=m027,"m028"=m028,"m029"=m029,
          #    "m030"=m030,"m031"=m031,"m032"=m032,"m033"=m033,"m034"=m034,"m035"=m035)
#ms <- modSel(ff)
###########################

cd <- list(m001,m002,m003,m005,m007, m008,m009,m010)
modename <- c("m001","m002","m003","m005","m007", "m008","m009","m010")
par <- c("ele.s","dis", "PAsize", "pop3000.s", "reach", "punish")
ff <- fitList(
  "m001~commonpalmcivet~~" =m001,
  "m002~commonpalmcivet~ele.s" = m002,
  "m003~commonpalmcivet~ele.s + dis" = m003,
  "m005~commonpalmcivet~ele.s + dis + PAsize " = m005,
  "m007~commonpalmcivet~ele.s + dis + PAsize + reach + punish" = m007,
  "m008~commonpalmcivet~ele.s + dis + reach + punish" =m008,
  "m009~commonpalmcivet~ele.s + dis + pop3000.s + reach + punish"=m009,
  "m010~commonpalmcivet~ele.s + dis + PAsize + pop3000.s + reach + punish" =m010
  )
ms <- modSel(ff)
toExport<-as(ms,"data.frame")
write.csv(toExport,file="result/wildboar_park_effe.csv")


#modavg(cand.set = cd, modnames = "", parm = "PAsize", parm.type = "lambda")
### model averaged prediction ####
## new data set ##
#head(sitecov.z)
#head(sitecovs$ele)
#ele.m<-mean(sitecovs$ele)
#ele.sd <- sd(sitecovs$ele)
#ele.seq <- seq(min(sitecovs$ele), max(sitecovs$ele), by = 5)
#ele.s <- (ele.seq-ele.m)/ele.sd
#nd1<- data.frame(ele.s=ele.s, dis=mean(sitecov.z$dis), pop3000.s=mean(pop3000.s),
#                 PAsize=mean(pasize), reach=mean(sitecov.z$reach), punish=mean(sitecov.z$punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  

#Modnames<-c("m001","m002","m003","m004","m005", "m006","m007", "m008","m009","m010")
#predmod <- modavgPred(cand.set = cd, modnames = Modnames, newdata = nd1, type = "response", parm.type= "lambda")

#qplot(trba, Predicted, data = preddata, geom = "line",
#      + xlab = "Scaled Basal Tree Area", ylab = "Estimated Abundance") +
#  + geom_ribbon(aes(x = trba, ymin = lower, ymax = upper), alpha = 0.1) +
#  + theme_bw()
#perdemod <- data.frame(predmod[7])
#names(perdemod) <- c("predicted", "se", "lowerCI", "upperCI")
#predmod$ele <- ele.seq
#plot(mod.avg.pred ~ ele.seq, data=predmod, type="l", ylab="Estimated abundance(wilboar)", col = "blue", lwd = 3, ylim=c(0,10))
#lines(lower.CL ~ ele.seq, predmod, type="l", col=gray(0.5))
#lines(upper.CL ~ ele.seq, predmod, type="l", col=gray(0.5))

#confint(m007, type="state")
#confint(m007, type="det")

#chisqRN=function(fm){
#  umf=getData(fm)
#  y=getY(umf)
#  y[y>1]=1
#  y=apply(y,1,sum,na.rm=T)
#  fv=fitted(fm,na.rm=T)
#  fv=apply(fv,1,sum,na.rm=T)
#  y[is.na(fv)]=NA
#  sum((y-fv)^2/(fv*(1-fv)),na.rm=T)
#}
#pb=parboot(object=m007, statistic=chisqRN, nsim=100)

#(pb <- parboot(m007, fitstats, nsim=100, report=1))


#preddata <- predict(m007, type = "response", newdata=nd4, appendData = TRUE)

#library("ggplot2")
#qplot(trba, Predicted, data = preddata, geom = "line",
#         + xlab = "Scaled Basal Tree Area", ylab = "Estimated Abundance") +
#  + geom_ribbon(aes(x = trba, ymin = lower, ymax = upper), alpha = 0.1) +
#  + theme_bw()
#summary(preddata)

#confint(m008, type="state")
#summary(ms)

#confint(m017, type="state") 

#Modnames<-c("m001","m002","m003","m004","m005","m006","m007","m008", "m009","m010", "m011","m012","m013")
#fms<-fitList("m001"=m001,"m002"=m002,"m003"=m003,"m004"=m004,"m005"=m005,"m006"=m006,"m007"=m007,
#             "m008"=m008, "m009"=m009,"m010"=m010,"m011"=m011,"m012"=m012,"m013"=m013)
#ms<-modSel(fms) #this is the model selection stuff about the avg covariate effects
#toExport<-as(ms,"data.frame")

#write.csv(toExport,file="result/commonpalmcivet_enforcement.csv")
                
#Modnames<-c("m001","m002","m003","m004","m005", "m006","m007", "m008","m009","m010")
#predmod <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata = nd1, type = "response", parm.type= "lambda"))
#colnames(predmod)

#e.dis <- predict(m017, type= "state", newdata=nd2, appendData=TRUE)
#head(e.dis )

#plot(Predicted ~ dis, data=e.dis, type="l", ylab="Estimated abundance(wilboar)", col = "blue", lwd = 3)
#lines(lower ~ dis, e.dis, type="l", col=gray(0.5))
#lines(upper ~ dis, e.dis, type="l", col=gray(0.5))
# 
#e.reach <- predict(m017, type= "state", newdata=nd4, appendData=TRUE)
#head(e.reach )

#plot(Predicted ~ reach, data=e.reach, type="l", ylab="Estimated abundance(wilboar)", col = "blue", lwd = 3)
#lines(lower ~ reach, e.reach, type="l", col=gray(0.5))
#lines(upper ~ reach, e.reach, type="l", col=gray(0.5))
#
#e.punish <- predict(m007, type= "state", newdata=nd4, appendData=TRUE)
#head(e.punish )

#plot(Predicted ~ punish, data=e.punish, type="l", ylab="Estimated abundance(wilboar)", col = "blue", lwd = 3)
#lines(lower ~ punish, e.punish, type="l", col=gray(0.5))
#lines(upper ~ punish, e.punish, type="l", col=gray(0.5))
#
# For unmarkedFitOccuRN objects, either lambda or detect can be entered for 
# abundance and detectability parameters, respectively.


#plot(e.psi$Predicted ~ e.psi$PAsize,  type="l")

#m004<-occuRN(~camhours + cam_angle ~ele.s + popP , data.umf)




# Model-averaged abundance estimates across the range of FRUIT, this part is making a new data set, 50 is the size of the new data set
ele.s <- sitecov.z$ele.s
dis.s <- sitecov.z$dis.s


newData1<-data.frame(ele=seq(min(ele.s),max(ele.s),,50),PAS= "Nabanhe", #doing for forest=1 here (PRIMARY) and then forest=1 (secondary) below
                     camhours=mean(na.omit(camhours.z)),dis=mean(na.omit(dis.s)))


#predsmod<-data.frame(modavgPred(cand.set=Cands,modnames=mnames,newdata=newData1,type="response",parm.type="lambda"))

#pred1<-data.frame(dfg.std=newData1[,1],fruit=((newData1[,1]*dfg.sd)+dfg.mean),N_forest1=predsmod$mod.avg.pred, #prediction 1
#                  SE=predsmod$uncond.se,lowCI=predsmod$mod.avg.pred-1.96*predsmod$uncond.se,hiCI=predsmod$mod.avg.pred+1.96*predsmod$uncond.se) #puts the standardized continuous vcariabels back into their actual #s
























