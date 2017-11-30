
#### prediction of the park ave abundance ####
#### prediction of the park ave abundance ######## prediction of the park ave abundance ####

Modnames<-c("m001","m002","m003","m004","m005","m006","m007","m008","m009", "m010")

sitecov.z <- cbind(sitecovs$PAS, sitecov.z, rowMeans(camhours.z, na.rm = T))
colnames(sitecov.z)[length(sitecov.z)] <- "camhours.z"
ele.paave <- aggregate(ele.s ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
dis.paave <- aggregate(dis ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
pop.paave <- aggregate(pop3000.s ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
pasize.paave <- aggregate(PAsize ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
reach.paave <- aggregate(reach ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
punish.paave <- aggregate(punish ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)
camhours.paave <- aggregate(camhours.z ~ sitecovs$PAS, data=sitecov.z, FUN="mean", na.rm=T)

nd1<- data.frame(ele.s=ele.paave[,2], dis=dis.paave[,2], pop3000.s =pop.paave[,2],
                 PAsize=pasize.paave[,2], reach=reach.paave[,2], punish=punish.paave[,2], camhours=camhours.paave[,2], cam_angle=1)  

avgpredic.1 <- data.frame(modavgPred(cand.set = cd, modnames = Modnames,newdata=nd1 ,type="response",parm.type="lambda" ))

abundance.wildboar.pa <- data.frame(ele.paave[,1], abundance=avgpredic.1$mod.avg.pred, SE=avgpredic.1$uncond.se, lowCI=avgpredic.1$mod.avg.pred-1.96*avgpredic.1$uncond.se,
                                    hiCI=avgpredic.1$mod.avg.pred+1.96*avgpredic.1$uncond.se)
colnames(abundance.wildboar.pa)[1] <- "pas"
write.csv(abundance.wildboar.pa,file="result/abundance.commonpalmcivet.pa.csv")

#### prediction of the park ave abundance ######## prediction of the park ave abundance ####