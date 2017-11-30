#### after fitting ### 
library(ggplot2)
library(AICcmodavg)
Modnames<-c("m001","m002","m003","m004","m005","m006","m007","m008","m009", "m010")
cd

##plot prediction size -- nd1 ###
## new data ----- PA size ###
size.seq <- seq(min(sitecovs$size.m), max(sitecovs$size.m),,500)
PAsize <- (size.seq- mean(sitecovs$size.m))/sd(sitecovs$size.m)
nd1<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=PAsize, reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  

avgpredic.1 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames,newdata=nd1 ,type="response",parm.type="lambda" ))

pre.size <- data.frame(PAsize=size.seq*1000, abundance=avgpredic.1$mod.avg.pred, SE=avgpredic.1$uncond.se, lowCI=avgpredic.1$mod.avg.pred-1.96*avgpredic.1$uncond.se,
                       hiCI=avgpredic.1$mod.avg.pred+1.96*avgpredic.1$uncond.se)

P <- ggplot(pre.size, aes(y=abundance, x=PAsize))  
P + geom_line() + geom_ribbon(data=pre.size, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="Size of Protected area (Ha)",y="Estimated Abundance")  + theme_bw() +  labs(title = "") 


## new data ----- distance ###
dis.seq <- seq(min(sitecovs$dis), max(sitecovs$dis),,500)
distance <- (dis.seq- mean(sitecovs$dis))/sd(sitecovs$dis)
nd2<- data.frame(ele.s=mean(sitecovs$ele.s), dis=distance, pop3000.s =mean(pop3000.s),
                 PAsize=mean(pasize), reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  

avgpredic.2 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd2 ,type="response",parm.type="lambda" ))

pre.dis <- data.frame(distance=dis.seq, abundance=avgpredic.2$mod.avg.pred, SE=avgpredic.2$uncond.se, lowCI=avgpredic.2$mod.avg.pred-1.96*avgpredic.2$uncond.se,
                       hiCI=avgpredic.2$mod.avg.pred+1.96*avgpredic.2$uncond.se)

P <- ggplot(pre.dis, aes(y=abundance, x=distance))  
P + geom_line() + geom_ribbon(data=pre.dis, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="Distance to board (m)",y="Estimated Abundance")  + theme_bw() + labs(title = "") 


## new data ----- freq outreach ###
reach.seq <- seq(min(anthrop$reach), max(anthrop$reach),,500)
reach <- (reach.seq- mean(anthrop$reach))/sd(anthrop$reach)
nd3<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=mean(pasize), reach=reach, punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.3 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd3 ,type="response",parm.type="lambda" ))

pre.reach <- data.frame(reach=reach.seq, abundance=avgpredic.3$mod.avg.pred, SE=avgpredic.3$uncond.se, lowCI=avgpredic.3$mod.avg.pred-1.96*avgpredic.3$uncond.se,
                      hiCI=avgpredic.3$mod.avg.pred+1.96*avgpredic.3$uncond.se)

P <- ggplot(pre.reach, aes(y=abundance, x=reach.seq))  
P + geom_line() + geom_ribbon(data=pre.reach, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="Freq out reach",y="Estimated Abundance")  + theme_bw() + labs(title = "") 

## new data ----- punish ###
pun.seq <- seq(min(anthrop$punish), max(anthrop$punish),,500)
punish <- (pun.seq- mean(anthrop$punish))/sd(anthrop$punish)
nd4<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=mean(sitecov.z$PAsize), reach=mean(sitecov.z$reach), punish=punish, camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.4 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd4 ,type="response",parm.type="lambda" ))

pre.pun <- data.frame(punish=pun.seq, abundance=avgpredic.4$mod.avg.pred, SE=avgpredic.4$uncond.se, lowCI=avgpredic.4$mod.avg.pred-1.96*avgpredic.4$uncond.se,
                        hiCI=avgpredic.4$mod.avg.pred+1.96*avgpredic.4$uncond.se)
P <- ggplot(pre.pun, aes(y=abundance, x=punish))  

P + geom_line()  + geom_ribbon(data=pre.pun, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="Freq of punishment (times/year)",y="Estimated Abundance")  + theme_bw() + labs(title = "") 

## new data ----- elevation ###
elevation.seq <- seq(min(sitecovs$ele), max(sitecovs$ele),,500)
ELE <- (elevation.seq- mean(sitecovs$ele))/sd(sitecovs$ele)
nd5<- data.frame(ele.s=ELE , dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                  PAsize=mean(pasize), reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.5 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd5 ,type="response",parm.type="lambda" ))
pre.ele <- data.frame(elevation=elevation.seq, abundance=avgpredic.5$mod.avg.pred, SE=avgpredic.5$uncond.se, lowCI=avgpredic.5$mod.avg.pred-1.96*avgpredic.5$uncond.se,
                      hiCI=avgpredic.5$mod.avg.pred+1.96*avgpredic.5$uncond.se)
P <- ggplot(pre.ele, aes(y=abundance, x=elevation))  

P + geom_line()  + geom_ribbon(data=pre.ele, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="elevation(m)",y="Estimated Abundance")  + theme_bw() + labs(title = "")

## new data ----- population ###
pop.seq <- seq(min(sitecovs$pop3000m), max(sitecovs$pop3000m),,500)
POP <- (pop.seq- mean(sitecovs$pop3000m))/sd(sitecovs$pop3000m)
nd6<-data.frame(ele.s=mean(sitecovs$ele.s) , dis=mean(sitecovs$dis.s), pop3000.s =POP,
                PAsize=mean(pasize), reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.6 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd6 ,type="response",parm.type="lambda" ))
pre.pop <- data.frame(population=pop.seq, abundance=avgpredic.6$mod.avg.pred, SE=avgpredic.6$uncond.se, lowCI=avgpredic.6$mod.avg.pred-1.96*avgpredic.6$uncond.se,
                      hiCI=avgpredic.6$mod.avg.pred+1.96*avgpredic.6$uncond.se)
P <- ggplot(pre.pop, aes(y=abundance, x=population))  

P + geom_line()  + geom_ribbon(data=pre.pop, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
  labs(x="Population",y="Abundance")  + theme_bw() + labs(title = "") 




### prediction but not useing ggplot ###


Modnames<-c("m001","m002","m003","m004","m005","m006","m007","m008","m009", "m010")
cd
##plot prediction size -- nd1 ###
size.seq <- seq(min(sitecovs$size.m), max(sitecovs$size.m),,500)
PAsize <- (size.seq- mean(sitecovs$size.m))/sd(sitecovs$size.m)
nd1<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=PAsize, reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  

avgpredic.1 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames,newdata=nd1 ,type="response",parm.type="lambda" ))

pre.size <- data.frame(PAsize=size.seq*1000, abundance=avgpredic.1$mod.avg.pred, SE=avgpredic.1$uncond.se, lowCI=avgpredic.1$mod.avg.pred-1.96*avgpredic.1$uncond.se,
                       hiCI=avgpredic.1$mod.avg.pred+1.96*avgpredic.1$uncond.se)

# defult plot #
plot(abundance ~ PAsize, data=pre.size, type="l",bty="l", ylim=c(0,5), 
    ylab="", xlab= "Size of Protected area (Ha)", col = "black", lwd = 2,
    main = "",cex.lab = 1)
lines(lowCI ~ PAsize, pre.size, type="l", col=gray(0.5), lty=2,lwd = 2)
lines(hiCI ~ PAsize, pre.size,type="l", col=gray(0.5), lty=2,lwd = 2)

#P <- ggplot(pre.size, aes(y=abundance, x=PAsize))  
#P + geom_line() + geom_ribbon(data=pre.size, aes(ymin=lowCI,ymax=hiCI),alpha=0.3) +
#    labs(x="Size of Proteced area(Ha)",y="")  + theme_bw()

##### plot prediction distane -- nd2 ###
dis.seq <- seq(min(sitecovs$dis), max(sitecovs$dis),,500)
distance <- (dis.seq- mean(sitecovs$dis))/sd(sitecovs$dis)
nd2<- data.frame(ele.s=mean(sitecovs$ele.s), dis=distance, pop3000.s =mean(pop3000.s),
                 PAsize=mean(pasize), reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  


avgpredic.2 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames,newdata=nd2 ,type="response",parm.type="lambda" ))
distance <- nd2$dis * sd(sitecovs$dis) + mean(sitecovs$dis) ## backtransform standardized data
pre.dis <- data.frame(distance=distance, abundance=avgpredic.2$mod.avg.pred, SE=avgpredic.2$uncond.se, lowCI=avgpredic.2$mod.avg.pred-1.96*avgpredic.2$uncond.se,
                       hiCI=avgpredic.2$mod.avg.pred+1.96*avgpredic.2$uncond.se)

plot(abundance ~ distance, data=pre.dis, type="l", ylim=c(0,5), 
     ylab="",xlab="Distance to boundary (m)", col = "black", lwd = 2,
     main = "", bty = "l",cex.lab = 1)
lines(lowCI ~ distance, pre.dis, type="l", col=gray(0.5), lty=2,lwd = 2)
lines(hiCI ~ distance, pre.dis,type="l", col=gray(0.5), lty=2,lwd = 2)

##### plot prediction number of outreach -- nd3 ###
reach.seq <- seq(min(anthrop$reach), max(anthrop$reach),,500)
reach <- (reach.seq- mean(anthrop$reach))/sd(anthrop$reach)
nd3<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=mean(pasize), reach=reach, punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  

avgpredic.3 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd3, type="response",parm.type="lambda" ))

pre.reach <- data.frame(reach=reach.seq, abundance=avgpredic.3$mod.avg.pred, SE=avgpredic.3$uncond.se, lowCI=avgpredic.3$mod.avg.pred-1.96*avgpredic.3$uncond.se,
                      hiCI=avgpredic.3$mod.avg.pred+1.96*avgpredic.3$uncond.se)

plot(abundance ~ reach, data=pre.reach, type="l", bty="l", ylim=c(0,5),
     ylab="",xlab="Frequency of villager-reported outreach (time/year)", col = "black", lwd = 2,
     main = "",cex.lab = 1)
lines(lowCI ~ reach, pre.reach, type="l", col=gray(0.5), lty=2,lwd = 2)
lines(hiCI ~ reach, pre.reach,type="l", col=gray(0.5), lty=2,lwd = 2)

##### plot prediction number of punish -- nd4 ###
pun.seq <- seq(min(anthrop$punish), max(anthrop$punish),,500)
punish <- (pun.seq- mean(anthrop$punish))/sd(anthrop$punish)
nd4<- data.frame(ele.s=mean(sitecovs$ele.s), dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=mean(sitecov.z$PAsize), reach=mean(sitecov.z$reach), punish=punish, camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.4 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd4, type="response",parm.type="lambda" ))

pre.pun <- data.frame(punish=pun.seq, abundance=avgpredic.4$mod.avg.pred, SE=avgpredic.4$uncond.se, lowCI=avgpredic.4$mod.avg.pred-1.96*avgpredic.4$uncond.se,
                        hiCI=avgpredic.4$mod.avg.pred+1.96*avgpredic.4$uncond.se)

plot(abundance ~ punish, data=pre.pun, type="l", bty="l",
     ylab="",xlab="Nunmber of villager-reported punishment (time/year)", col = "black", lwd = 2, ylim=c(0,100), 
     main = "",cex.lab = 1)
lines(lowCI ~ punish, pre.pun, type="l", col=gray(0.5), lty=2, lwd = 2)
lines(hiCI ~ punish, pre.pun,type="l", col=gray(0.5), lty=2, lwd = 2)

##### plot prediction number of elevation -- nd5 ###
elevation.seq <- seq(min(sitecovs$ele), max(sitecovs$ele),,500)
ELE <- (elevation.seq- mean(sitecovs$ele))/sd(sitecovs$ele)
nd5<- data.frame(ele.s=ELE , dis=mean(sitecovs$dis.s), pop3000.s =mean(pop3000.s),
                 PAsize=mean(pasize), reach=mean(reach), punish=mean(punish), camhours=mean(na.omit(camhours.z)), cam_angle=1)  
avgpredic.5 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd5, type="response",parm.type="lambda" ))
elevation <- nd5$ele.s * sd(sitecovs$ele) + mean(sitecovs$ele)
pre.ele <- data.frame(elevation=elevation, abundance=avgpredic.5$mod.avg.pred, SE=avgpredic.5$uncond.se, lowCI=avgpredic.5$mod.avg.pred-1.96*avgpredic.5$uncond.se,
                         hiCI=avgpredic.5$mod.avg.pred+1.96*avgpredic.5$uncond.se)

plot(abundance ~ elevation, data=pre.ele, type="l",bty="l", ylim=c(-1,1),
     ylab="", xlab="Elevation (m)", col = "black", lwd = 2,  
     main = "",cex.lab = 1.5)
lines(lowCI ~ elevation, pre.ele, type="l", col=gray(0.5), lty=2)
lines(hiCI ~ elevation, pre.ele,type="l", col=gray(0.5), lty=2)

##### plot prediction number human population density-- nd6 ###
avgpredic.6 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames, newdata=nd6, type="response",parm.type="lambda" ))
Pop3000m <- (nd6$pop3000.s * sd(sitecovs$pop3000m) + mean(sitecovs$pop3000m))/706.5 ## 3000m diameter = 705.5 HA
pre.pop <- data.frame(Pop.index=Pop3000m, abundance=avgpredic.6$mod.avg.pred, SE=avgpredic.6$uncond.se, lowCI=avgpredic.6$mod.avg.pred-1.96*avgpredic.6$uncond.se,
                      hiCI=avgpredic.6$mod.avg.pred+1.96*avgpredic.6$uncond.se)

plot(abundance ~ Pop.index, data=pre.pop, type="l", ylim=c(0,3),
     ylab="Predicted abundance", col = "blue", lwd = 3,
     main = "Wildboar")
lines(lowCI ~ Pop.index, pre.pop, type="l", col=gray(0.5), lty=2)
lines(hiCI ~ Pop.index, pre.pop,type="l", col=gray(0.5), lty=2)

