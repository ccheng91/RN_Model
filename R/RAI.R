#### RAI ### 
library(ggplot2)
library(tidyr)
library(dplyr)

rm(list=ls(all=TRUE))

##### RAI for each spp each camera###
spp1.RAI <- read.csv("data/All_widedata/wildboar_datawide_n.csv", header=TRUE)
spp2.RAI <- read.csv("data/All_widedata/muntjac_datawide_n.csv", header=TRUE)
spp3.RAI <- read.csv("data/All_widedata/maskedpalmcivet_datawide_n.csv", header=TRUE)
spp4.RAI <- read.csv("data/All_widedata/commonpalmcivet_datawide_n.csv", header=TRUE)
camhour <- read.csv("data/All_widedata/datawide_camhour.csv", header=TRUE)

####
#list.filenames <- list.files(path = "data/All_widedata", pattern="*.csv")
#list.filenames <- paste(c("data/All_widedata/"), list.filenames, sep="")
# create an empty list that will serve as a container to receive the incoming files
#spp.temp <- c("blackbear","brushtailedporcupine", "chineseferretbadger",  "commonmacaque", "commonpalmcivet","crabeatingmongoose", "camhour","dhole",               
#              "gaur", "goral","hogbadger","leopardcat","maskedpalmcivet", "muntjac", "pigtailedmacaque", "porcupine", "sambar", "serow", "smallindiancivet", 
#              "spotedlinsang", "weasel", "wildboar",   "yellowthroatedmarten")
#spp <- spp.temp[-7]
#list.data<-list()

# create a loop to read in your data
#RAI <- matrix(999, 115, 23)
#for (i in 1:length(list.filenames)){
#  a <- read.csv(list.filenames[i])
#  a <- a[-1]
#  RAI[,i] <- rowSums(a, na.rm = T)
#  colnames(a) <- NULL
#}

RAI.1 <- spp1.RAI[-1]
RAI.1 <- (rowSums(spp1.RAI[,-1], na.rm = T))
RAI.2 <- rowSums(spp2.RAI[,-1], na.rm = T)
RAI.3 <- rowSums(spp3.RAI[,-1], na.rm = T)
RAI.4 <- rowSums(spp4.RAI[,-1], na.rm = T)
trapnight <- rowSums(camhour[,-1], na.rm = T)/24
RAI <- cbind((RAI.1*100/trapnight), (RAI.2*100/trapnight), 
             (RAI.3*100/trapnight), (RAI.4*100/trapnight))
RAI <- as.data.frame(RAI)
RAI<- cbind(spp1.RAI[,1], RAI)

anthrop <- read.csv("data/anthrop.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
RAI <- cbind(anthrop$pas, RAI)
names(RAI) <- c("pas", "site", "wildboar", "muntjac","maskedpalmcivet","commonpalmcivet")
RAI$pas <- as.character(RAI$pas)
n <- as.matrix(table(RAI$pas))
n <- sqrt(n)

#sitecov <- read.csv("data/sitecov_temp.csv")
#RAI1111 <- RAI

#RAI1111$dis <- sitecov$dis
#plot(RAI1111$wildboar ~ RAI1111$dis)
#summary(lm(RAI1111$wildboar ~ RAI1111$dis))

wildboar.mean <- aggregate(wildboar ~ pas, data=RAI, FUN="mean", na.rm=T)
wildboar.sd <- aggregate(wildboar ~ pas, data=RAI, FUN="sd", na.rm=T)
wildboar.sd[,2] <- wildboar.sd[,2]/n

muntjac.mean <- aggregate(muntjac ~ pas, data=RAI, FUN="mean", na.rm=T)
muntjac.sd <- aggregate(muntjac ~ pas, data=RAI, FUN="sd", na.rm=T)
muntjac.sd[,2] <- muntjac.sd[,2]/n

maskedpalmcivet.mean <- aggregate(maskedpalmcivet ~ pas, data=RAI, FUN="mean", na.rm=T)
maskedpalmcivet.sd <- aggregate(maskedpalmcivet ~ pas, data=RAI, FUN="sd", na.rm=T)
maskedpalmcivet.sd[,2] <- maskedpalmcivet.sd[,2]/n

commonpalmcivet.mean <- aggregate(commonpalmcivet ~ pas, data=RAI, FUN="mean", na.rm=T)
commonpalmcivet.sd <- aggregate(commonpalmcivet ~ pas, data=RAI, FUN="sd", na.rm=T)
commonpalmcivet.sd[,2] <- commonpalmcivet.sd[,2]/n


#names(RAI) <- spp.temp
#station <- as.character(spp1.RAI[,1])
#rownames(RAI) <- station
#camhour <- RAI[,7]
#RAI <- RAI[,-7]
#RAI <- (RAI*100)/(camhour/24)
#RAI<-round(RAI,2)

#sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
#head(sitecovs)
#anthrop <- read.csv("data/anthrop.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
#RAI <- cbind(anthrop$pas, RAI)
#colnames(RAI)[1] <- "pas"
#RAI <- aggregate(RAI[,-1], list(RAI$pas), FUN = "mean")

qsdata <- read.csv("data/QS_102.csv")
head(qsdata)

qsn <- as.matrix(table(qsdata$PAs))
###  #####

pa.reach.mean <- aggregate(qsdata$Time_of_outreach ~ qsdata$PAs, data=qsdata, FUN="mean", na.rm=T)
pa.punish.mean <- aggregate(qsdata$No_of_Punishment ~ qsdata$PAs, data=qsdata, FUN="mean", na.rm=T)
pa.reach.sd <- aggregate(qsdata$Time_of_outreach ~ qsdata$PAs, data=qsdata, FUN="sd", na.rm=T)
pa.reach.sd[,2] <- pa.reach.sd[,2]/qsn
pa.punish.sd <- aggregate(qsdata$No_of_Punishment ~ qsdata$PAs, data=qsdata, FUN="sd", na.rm=T)
pa.punish.sd[,2] <- pa.punish.sd[,2]/qsn

sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
size <- unique(sitecovs$size.m)
pa <-  tolower(unique(sitecovs$PAS))
size <-data.frame(pa, size)
size <- size[c(4,2,1,3,5,6),]
size[,2] <- size[,2]*10000

#colnames(RAI)[1] <- "pas"
#colnames(RAI)[length(RAI)-1] <- "outreach"
#colnames(RAI)[length(RAI)] <- "punishment"

abundance.wildboar.pa <- read.csv("result/abundance.wildboar.pa.csv", header=TRUE)
abundance.muntjac.pa <- read.csv("result/abundance.muntjac.pa.csv", header=TRUE)
abundance.maskedpalmcivet.pa <- read.csv("result/abundance.maskedpalmcivet.pa.csv", header=TRUE)
abundance.commonpalmcivet.pa <- read.csv("result/abundance.commonpalmcivet.pa.csv", header=TRUE)

six.point <- data.frame(pas=as.character(abundance.wildboar.pa[,2]), 
                        RAI.wildboar=wildboar.mean[,2], RAI.SD.wildboar=wildboar.sd[,2],
                        RAI.muntjac=muntjac.mean[,2], RAI.SD.muntjac=muntjac.sd[,2],
                       
                        RAI.maskedpalmcivet=maskedpalmcivet.mean[,2], RAI.SD.maskedpalmcivet=maskedpalmcivet.sd[,2],
                        RAI.commonpalmcivet=commonpalmcivet.mean[,2], RAI.SD.commonpalmcivet=commonpalmcivet.sd[,2], 
                        
                        abundance.wildboar=abundance.wildboar.pa[,3], lowCI.W=(abundance.wildboar.pa[,3]-abundance.wildboar.pa[,4]), hiCI.W=(abundance.wildboar.pa[,3]+abundance.wildboar.pa[,4]),
                        
                        abundance.muntjac=abundance.muntjac.pa[,3], lowCI.M=(abundance.muntjac.pa[,3]-abundance.muntjac.pa[,4]), hiCI.M=(abundance.muntjac.pa[,3]+abundance.muntjac.pa[,4]),
                        
                        abundance.maskedpalmcivet=abundance.maskedpalmcivet.pa[,3], lowCI.Ma=(abundance.maskedpalmcivet.pa[,3]-abundance.maskedpalmcivet.pa[,4]), hiCI.Ma=(abundance.maskedpalmcivet.pa[,3]+abundance.maskedpalmcivet.pa[,4]),
                        
                        abundance.commonpalmcivet=abundance.commonpalmcivet.pa[,3], lowCI.C=(abundance.commonpalmcivet.pa[,3]-abundance.commonpalmcivet.pa[,4]), hiCI.C=(abundance.commonpalmcivet.pa[,3]+abundance.commonpalmcivet.pa[,4]),
                        
                        Reach=pa.reach.mean[,2], Reach.SD=pa.reach.sd[,2], Punishment=pa.punish.mean[,2], Punishment.SD=pa.punish.sd[,2], size=size[,2])

#### multiplot function ######## multiplot function ######## multiplot function ####
#### multiplot function ######## multiplot function ######## multiplot function ####
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#### multiplot function ######## multiplot function ######## multiplot function ####
#### multiplot function ######## multiplot function ######## multiplot function ####
six.point$pas <- c("Bulangshan", "Mango", "Mengla", "Menglun", "Mengsong", "Nabanhe")

summary(lm(six.point$RAI.wildboar ~ six.point$Reach))
summary(lm(six.point$RAI.muntjac ~ six.point$Reach))
summary(lm(six.point$RAI.wildboar ~ six.point$Punishment))
summary(lm(six.point$RAI.muntjac ~ six.point$Punishment))

summary(lm(six.point$RAI.wildboar ~ six.point$size))
summary(lm(six.point$RAI.muntjac ~ six.point$size))
summary(lm(six.point$RAI.maskedpalmcivet ~ six.point$size))
summary(lm(six.point$RAI.commonpalmcivet ~ six.point$size))

summary(lm( six.point$Punishment ~ six.point$Reach))


P1 <- ggplot(six.point, aes(y=RAI.wildboar, x=Reach)) + geom_point() +theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar,width = .05))+
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
      theme(axis.line.x = element_line(), axis.line.y = element_line()) + 
      geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue",size = 7,check_overlap = TRUE) +
      labs(x="Freq of villager-reported outreach (times/year)",y="RAI of Wildboar", size=5) + geom_smooth(method='lm',color="black", linetype=2, se=FALSE) 

      
P2 <- ggplot(six.point, aes(y=RAI.muntjac, x=Reach)) + geom_point() + theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.muntjac + RAI.SD.muntjac, ymin = RAI.muntjac - RAI.SD.muntjac,width = .05)) + 
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
      theme(axis.line.x = element_line(), axis.line.y = element_line()) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25, size = 7, color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported outreach (times/year)",y="RAI of Muntjac") + geom_smooth(method='lm',color="black", linetype=2, se=FALSE) 

P3 <- ggplot(six.point, aes(y=RAI.maskedpalmcivet, x=Reach)) + geom_point() + theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.maskedpalmcivet + RAI.SD.maskedpalmcivet, ymin = RAI.maskedpalmcivet - RAI.SD.maskedpalmcivet,width = .05)) + 
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
      theme(axis.line.x = element_line(), axis.line.y = element_line())  + ylim(0,5) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25, size = 7, color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported outreach (times/year)",y="RAI of Masked palm civet")+ geom_smooth(method='lm', color="black",linetype=2, se=FALSE) 
  
P4 <- ggplot(six.point, aes(y=RAI.commonpalmcivet, x=Reach)) + geom_point() + theme_classic() +theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.commonpalmcivet + RAI.SD.commonpalmcivet, ymin = RAI.commonpalmcivet - RAI.SD.commonpalmcivet,width = .05)) + 
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
      theme(axis.line.x = element_line(), axis.line.y = element_line()) + ylim(0,5) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25,size = 7, color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported outreach (times/year)",y="RAI of Common palm civet")+ geom_smooth(method='lm', linetype=2, se=FALSE, colour="black") 

P5 <- ggplot(six.point, aes(y=RAI.wildboar, x=Punishment)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar, width = .01)) +
      geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1)) + xlim(0.4,1.5) +
      theme(axis.line.x = element_line(), axis.line.y = element_line()) + 
      geom_text(aes(label=pas),hjust=0, vjust=-0.25,size = 7, color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported punishment (times/year)",y="RAI of Wildboar")+ geom_smooth(method='lm', linetype=2, se=FALSE, colour="black") 


P6 <- ggplot(six.point, aes(y=RAI.muntjac, x=Punishment)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.muntjac + RAI.SD.muntjac, ymin = RAI.muntjac - RAI.SD.muntjac , width = .01)) +
      geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1)) + xlim(0.4,1.5) +
      theme(axis.line.x = element_line(), axis.line.y = element_line()) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25,size = 7, color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported punishment (times/year)",y="RAI of Muntjac")+ geom_smooth(method='lm', linetype=2, se=FALSE, colour="black") 

P7 <- ggplot(six.point, aes(y=RAI.maskedpalmcivet, x=Punishment)) + geom_point() + theme_classic() +theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.maskedpalmcivet + RAI.SD.maskedpalmcivet, ymin = RAI.maskedpalmcivet - RAI.SD.maskedpalmcivet ,width = .01)) +
      geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1)) + xlim(0.4,1.5) +
      theme(axis.line.x = element_line(), axis.line.y = element_line())+ ylim(0,5) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25, size = 7,color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported punishment (times/year)",y="RAI of Masked palm civet")+ geom_smooth(method='lm', linetype=2, se=FALSE, colour="black") 

P8 <- ggplot(six.point, aes(y=RAI.commonpalmcivet, x=Punishment)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
      geom_errorbar(aes(ymax = RAI.commonpalmcivet + RAI.SD.commonpalmcivet, ymin = RAI.commonpalmcivet - RAI.SD.commonpalmcivet ,width = .01)) +
      geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1)) + xlim(0.4,1.5) +
      theme(axis.line.x = element_line(), axis.line.y = element_line())+ ylim(0,5) +
      geom_text(aes(label=pas),hjust=0, vjust=-0.25, size = 7,color="Blue",check_overlap = TRUE) +
      labs(x="Freq of villager-reported punishment (times/year)",y="RAI of Common palm civet")+ geom_smooth(method='lm', linetype=2, se=FALSE, colour="black") 
      
      
P9 <- ggplot(six.point, aes(y=abundance.wildboar, x=Reach)) + geom_point() + theme_classic()+
      geom_errorbar(aes(ymax = hiCI.W, ymin = lowCI.W, width = .05))+xlim(2,6) +
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+
      theme(axis.line.x = element_line(), axis.line.y = element_line())+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
      labs(x="Freq of outreach (times/year)",y="Estimated local abundance of Wildboar")


P10 <-ggplot(six.point, aes(y=abundance.muntjac, x=Reach)) + geom_point() + theme_classic()+
      geom_errorbar(aes(ymax = hiCI.M, ymin = lowCI.M, width = .05))+xlim(2,6) +
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .05))+ #geom_smooth(method = "lm", formula = y ~ x) +
      theme(axis.line.x = element_line(), axis.line.y = element_line())+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
      labs(x="Freq of outreach (times/year)",y="Estimated local abundance of Muntjac")
  
  
P11 <-ggplot(six.point, aes(y=abundance.maskedpalmcivet, x=Reach)) + geom_point() + theme_classic() +
      geom_errorbar(aes(ymax = hiCI.Ma, ymin = lowCI.Ma, width = .05))+xlim(2,6) +
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .05))+
      theme(axis.line.x = element_line(), axis.line.y = element_line())+ ylim(0,5)+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
      labs(x="Freq of outreach (times/year)",y="Estimated local abundance of Masked palm civet")
  
  
P12 <-ggplot(six.point, aes(y=abundance.commonpalmcivet, x=Reach)) + geom_point() + theme_classic()+
      geom_errorbar(aes(ymax = hiCI.C, ymin = lowCI.C, width = .02))+ xlim(2,6) +
      geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .05))+
      theme(axis.line.x = element_line(), axis.line.y = element_line())+ ylim(-0.2,5)+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
      labs(x="Freq of outreach (times/year)",y="Estimated local abundance of Common palm civet")

P13 <-ggplot(six.point, aes(y=abundance.wildboar, x=Punishment)) + geom_point() + theme_classic()+
      geom_errorbar(aes(ymax = hiCI.W, ymin = lowCI.W, width = .01))+xlim(0.4,1.3) +
      geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1))+
      theme(axis.line.x = element_line(), axis.line.y = element_line())+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
      labs(x="Freq of punishment (times/year)",y="Estimated local abundance of Wildboar")
  
  
P14 <-ggplot(six.point, aes(y=abundance.muntjac, x=Punishment)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax = hiCI.M, ymin = lowCI.M, width = .01))+xlim(0.4,1.3) +
  geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
  labs(x="Freq of punishment (times/year)",y="Estimated local abundance of Muntjac")
  
  
P15 <-ggplot(six.point, aes(y=abundance.maskedpalmcivet, x=Punishment)) + geom_point() + theme_classic()+ 
  geom_errorbar(aes(ymax = hiCI.Ma, ymin = lowCI.Ma, width = .01))+ xlim(0.4,1.3) +
  geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + ylim(0,5)+geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
  labs(x="Freq of punishment (times/year)",y="Estimated local abundance of Masked palm civet")

P16 <-ggplot(six.point, aes(y=abundance.commonpalmcivet, x=Punishment)) + geom_point() + theme_classic()+
  geom_errorbar(aes(ymax = hiCI.C, ymin = lowCI.C, width= 0.01))+
  geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height=0.1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + ylim(-0.5,5) + xlim(0.4,1.3) +geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue") +
  labs(x="Freq of punishment (times/year)",y="Estimated local abundance of Common palm civet") 

### size ###
P17 <-ggplot(six.point, aes(y=RAI.wildboar, x=size)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar))+
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height=0.1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + xlim(0,100000) + # ylim(-0.5,5) +
  geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue",size = 6,check_overlap = TRUE) + geom_smooth(method='lm',color="black", linetype=1, se=FALSE) +
  labs(x="Size of Protected area (Ha)",y="RAI of wildboar", size=5) 

P18<-ggplot(six.point, aes(y=RAI.muntjac, x=size)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  geom_errorbar(aes(ymax = RAI.muntjac + RAI.SD.muntjac, ymin = RAI.muntjac - RAI.SD.muntjac)) +
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height=0.1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + xlim(0,100000) +# ylim(-0.5,5) + xlim(0.4,1.3) +
  geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue",size = 6,check_overlap = TRUE) + geom_smooth(method='lm',color="black", linetype=2, se=FALSE) +
  labs(x="Size of Protected area (Ha)",y="RAI of muntjac", size=5) 


P19 <-ggplot(six.point, aes(y=RAI.maskedpalmcivet, x=size)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  geom_errorbar(aes(ymax = RAI.maskedpalmcivet + RAI.SD.maskedpalmcivet, ymin = RAI.maskedpalmcivet - RAI.SD.maskedpalmcivet)) +
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height=0.1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + xlim(0,100000) +  ylim(0,5) + # xlim(0.4,1.3) +
  geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue",size = 6,check_overlap = TRUE) + geom_smooth(method='lm',color="black", linetype=2, se=FALSE) +
  labs(x="Size of Protected area (Ha)",y="RAI of masked palm civet", size=5) 

P20<-ggplot(six.point, aes(y=RAI.commonpalmcivet, x=size)) + geom_point() + theme_classic()+theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  geom_errorbar(aes(ymax = RAI.commonpalmcivet + RAI.SD.commonpalmcivet, ymin = RAI.commonpalmcivet - RAI.SD.commonpalmcivet)) + 
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height=0.1))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + ylim(0,5) + xlim(0,100000) + 
  geom_text(aes(label=pas),hjust=0, vjust=-0.25, color="Blue",size = 6,check_overlap = TRUE) + geom_smooth(method='lm',color="black", linetype=1, se=FALSE) +
  labs(x="Size of Protected area (Ha)",y="RAI of common palm civet", size=5) 

multiplot(P1, P3,P2, P4, cols=2)
multiplot(P5, P7,P6, P8, cols=2)
multiplot(P9, P11,P10, P12, cols=2)
multiplot(P13, P16,P14, P16, cols=2)
multiplot(P17, P19,P18, P20, cols=2)

## plot without error bar ### 

P1 <- ggplot(six.point, aes(y=RAI.wildboar, x=Reach)) + geom_point() +theme_classic()+ 
 # geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar,height = .2))+
 # geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P2 <- ggplot(six.point, aes(y=RAI.muntjac, x=Reach)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = RAI.muntjac + RAI.SD.muntjac, ymin = RAI.muntjac - RAI.SD.muntjac,height = .2)) + 
 # geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P3 <- ggplot(six.point, aes(y=RAI.maskedpalmcivet, x=Reach)) + geom_point() + theme_classic()+ 
  #geom_errorbar(aes(ymax = RAI.maskedpalmcivet + RAI.SD.maskedpalmcivet, ymin = RAI.maskedpalmcivet - RAI.SD.maskedpalmcivet,height = .2)) + 
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P4 <- ggplot(six.point, aes(y=RAI.commonpalmcivet, x=Reach)) + geom_point() + theme_classic() +
  #geom_errorbar(aes(ymax = RAI.commonpalmcivet + RAI.SD.commonpalmcivet, ymin = RAI.commonpalmcivet - RAI.SD.commonpalmcivet,height = .2)) + 
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + ylim(0,5)

P5 <- ggplot(six.point, aes(y=RAI.wildboar, x=Punishment)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar ,height = .2)) +
 # geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2)) +
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P6 <- ggplot(six.point, aes(y=RAI.muntjac, x=Punishment)) + geom_point() + theme_classic()+
 # geom_errorbar(aes(ymax = RAI.muntjac + RAI.SD.muntjac, ymin = RAI.muntjac - RAI.SD.muntjac ,height = .2)) +
 # geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2)) +
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P7 <- ggplot(six.point, aes(y=RAI.maskedpalmcivet, x=Punishment)) + geom_point() + theme_classic() +
  #geom_errorbar(aes(ymax = RAI.maskedpalmcivet + RAI.SD.maskedpalmcivet, ymin = RAI.maskedpalmcivet - RAI.SD.maskedpalmcivet ,height = .2)) +
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2)) +
  theme(axis.line.x = element_line(), axis.line.y = element_line())


P8 <- ggplot(six.point, aes(y=RAI.commonpalmcivet, x=Punishment)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = RAI.commonpalmcivet + RAI.SD.commonpalmcivet, ymin = RAI.commonpalmcivet - RAI.SD.commonpalmcivet ,height = .2)) +
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2)) +
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P9 <- ggplot(six.point, aes(y=abundance.wildboar, x=Reach)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = hiCI.W, ymin = lowCI.W, height = .2))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())


P10 <-ggplot(six.point, aes(y=abundance.muntjac, x=Reach)) + geom_point() + theme_classic()+
 # geom_errorbar(aes(ymax = hiCI.M, ymin = lowCI.M, height = .2))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())



P11 <-ggplot(six.point, aes(y=abundance.maskedpalmcivet, x=Reach)) + geom_point() + theme_classic() +
  #geom_errorbar(aes(ymax = hiCI.Ma, ymin = lowCI.Ma, height = .2))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())



P12 <-ggplot(six.point, aes(y=abundance.commonpalmcivet, x=Reach)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = hiCI.C, ymin = lowCI.C, height = .2))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())

P12 <-ggplot(six.point, aes(y=abundance.wildboar, x=Punishment)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = hiCI.W, ymin = lowCI.W, height = .2))+
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())


P13 <-ggplot(six.point, aes(y=abundance.muntjac, x=Punishment)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = hiCI.M, ymin = lowCI.M, height = .2))+
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())


P14 <-ggplot(six.point, aes(y=abundance.maskedpalmcivet, x=Punishment)) + geom_point() + theme_classic()+ 
 # geom_errorbar(aes(ymax = hiCI.Ma, ymin = lowCI.Ma, height = .2))+
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())



P15 <-ggplot(six.point, aes(y=abundance.commonpalmcivet, x=Punishment)) + geom_point() + theme_classic()+
  #geom_errorbar(aes(ymax = hiCI.C, ymin = lowCI.C, height = .2))+
  #geom_errorbarh(aes(xmax = Punishment + Punishment.SD, xmin = Punishment - Punishment.SD, height = .2))+
  theme(axis.line.x = element_line(), axis.line.y = element_line())


multiplot(P1, P3,P2, P4, cols=2)
multiplot(P5, P7,P6, P8, cols=2)
multiplot(P9, P11,P10, P12, cols=2)
multiplot(P12, P14,P13, P15, cols=2)

#z <- lm( RAI.muntjac ~ Punishment, data=six.point)
#summary(z)
#visreg::visreg(z)


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

rowSums(list.data[[2]], na.rm = T)

RAI.ma<- matrix(999, 115, 23)

for (i in 1:length(list.filenames)) {
  RAI.ma[,i] <-  rowSums(list.data[[i]], na.rm = T)
}

trapnight <- RAI.ma[,7]/24
RAI.ma <- RAI.ma[,-7]
RAI.ma <- as.data.frame(RAI.ma)
RAI.ma <- (RAI.ma*100)/trapnight

anthrop <- read.csv("data/anthrop.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
RAI.ma <- cbind(anthrop$pas, RAI.ma)
names(RAI.ma) <- c("pas", "blackbear","brushtailedporcupine", "chineseferretbadger",  "commonmacaque", "commonpalmcivet","crabeatingmongoose","dhole",               
                   "gaur", "goral","hogbadger","leopardcat","maskedpalmcivet", "muntjac", "pigtailedmacaque", "porcupine", "sambar", "serow", "smallindiancivet", 
                   "spotedlinsang", "weasel", "wildboar",   "yellowthroatedmarten")

RAI.ma$pas <- as.character(RAI.ma$pas)
n <- as.matrix(table(RAI.ma$pas))
n <- sqrt(n)

#sitecov <- read.csv("data/sitecov_temp.csv")
#RAI1111 <- RAI

#RAI1111$dis <- sitecov$dis
#plot(RAI1111$wildboar ~ RAI1111$dis)
#summary(lm(RAI1111$wildboar ~ RAI1111$dis))

#wildboar.mean <- aggregate(wildboar ~ pas, data=RAI, FUN="mean", na.rm=T)
#wildboar.sd <- aggregate(wildboar ~ pas, data=RAI, FUN="sd", na.rm=T)
#wildboar.sd[,2] <- wildboar.sd[,2]/n


#tapply(RAI.ma$pas, RAI.ma[,-1], mean)

#require(stats)
##groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
#tapply(groups, groups, length) #- is almost the same as
#table(groups)

## contingency table from data.frame : array with named dimnames
#tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
#tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)

grp <- group_by(RAI.ma, pas) 
grp.RAI <- summarise_all(grp, funs(mean))
grp.RAI <-as.data.frame(grp.RAI)
pa.sd <- as.data.frame(summarise_all(grp.RAI[,-1] , funs(sd)))
#pa.mean <- as.data.frame(summarise_all(grp.RAI[,-1] , funs(mean)))
pa.sd[2:6,] <- pa.sd[1,] 
#pa.mean[2:6,] <- pa.mean[1,] 

grp.RAI[,-1] <- grp.RAI[,-1]/pa.sd
All.RAI.mean <- rowMeans(grp.RAI[,-1])
All.RAI.sum <- rowSums(grp.RAI[,-1]) 

qsdata <- read.csv("data/QS_102.csv")
head(qsdata)

qsn <- as.matrix(table(qsdata$PAs))
###  #####

pa.reach.mean <- aggregate(qsdata$Time_of_outreach ~ qsdata$PAs, data=qsdata, FUN="mean", na.rm=T)
pa.punish.mean <- aggregate(qsdata$No_of_Punishment ~ qsdata$PAs, data=qsdata, FUN="mean", na.rm=T)
#pa.reach.sd <- aggregate(qsdata$Time_of_outreach ~ qsdata$PAs, data=qsdata, FUN="sd", na.rm=T)
#pa.reach.sd[,2] <- pa.reach.sd[,2]/qsn
#pa.punish.sd <- aggregate(qsdata$No_of_Punishment ~ qsdata$PAs, data=qsdata, FUN="sd", na.rm=T)
#pa.punish.sd[,2] <- pa.punish.sd[,2]/qsn

#grp.RAI$pa.reach.mean <- pa.reach.mean[,2]
#grp.RAI$pa.punish.mean <- pa.punish.mean[,2]

sitecovs <- read.csv("data/sitecov_temp.csv", header = T)
sitecovs$PAS <- as.character(sitecovs$PAS)
lv <- which(sitecovs$PAS == "Lvshilin")
sitecovs$PAS[lv] <- "menglun"
sitecovs$PAS <- tolower(sitecovs$PAS)

size <- aggregate(size ~ PAS, data=sitecovs, FUN="mean", na.rm=T)


RAI.long <- data.frame(RAI.mean=All.RAI.mean, RAI.sum=All.RAI.sum, pa.reach.mean=pa.reach.mean[,2], pa.punish.mean=pa.punish.mean[,2], size=size)
 names(RAI.long)[5] <- "pas"
 names(RAI.long)[6] <- 'size'
#gather(grp.RAI, spp, RAI, blackbear:yellowthroatedmarten, factor_key = T)

#RAI.long$RAI <- round(RAI.long$RAI, 2)
#RAI.long$RAI <- as.numeric(RAI.long$RAI)

plot(RAI.long$RAI.sum ~ RAI.long$pa.reach.mean)
summary(lm(RAI.long$RAI.sum ~ RAI.long$pa.reach.mean))
summary(lm(RAI.long$RAI.sum ~ RAI.long$pa.punish.mean))
summary(lm(RAI.long$RAI.sum ~ RAI.long$size))

P1 <- ggplot(RAI.long, aes(y=RAI, x=pa.reach.mean,shape=21)) + geom_point() +theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  #geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar,width = .05))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
  theme(axis.line.x = element_line(), axis.line.y = element_line()) +   geom_jitter() +scale_shape_identity() +
  labs(x="Freq of villager-reported outreach (times/year)",y="RAI of all mammal", size=5) + geom_smooth(method='lm',color="black") 


P2 <- ggplot(RAI.long, aes(y=RAI, x=pa.punish.mean, shape=21)) + geom_point() +theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  #geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar,width = .05))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
  theme(axis.line.x = element_line(), axis.line.y = element_line()) +   geom_jitter() +scale_shape_identity() +
  labs(x="Freq of punishment (times/year)",y="RAI of all mammal", size=5) + geom_smooth(method='lm',color="black") 


P3 <- ggplot(RAI.long, aes(y=RAI, x=size, shape=21)) + geom_point() +theme_classic()+ theme(axis.title  = element_text(size = 20),axis.text = element_text(size = 15))+
  #geom_errorbar(aes(ymax = RAI.wildboar + RAI.SD.wildboar, ymin = RAI.wildboar - RAI.SD.wildboar,width = .05))+
  #geom_errorbarh(aes(xmax = Reach + Reach.SD, xmin = Reach - Reach.SD, height = .1))+xlim(2,6) +
  theme(axis.line.x = element_line(), axis.line.y = element_line()) + # geom_jitter() 
  scale_shape_identity() +
  labs(x="Size of Protected area (ha)",y="RAI of all mammal", size=5) + geom_smooth(method='lm',color="black") 


