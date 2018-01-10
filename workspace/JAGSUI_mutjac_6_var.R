# RN model with park random effects #
library("jagsUI")
library(wiqid) # for the MLE runs
library(coda)

## read stuff ##
rm(list=ls(all=TRUE))
# dec.data<-read.csv("dec.data.csv",header=TRUE, strip.white=TRUE)
dec.data<-read.csv("data/datasheet/maskedpalmcivet.dec.data.csv",header=TRUE, row.names=1)
head(dec.data)
# Separate out the detection history and investigate
DH <- as.matrix(dec.data[, 1:284])
colSums(DH, na.rm=TRUE)
rowSums(DH, na.rm=TRUE)  # no all-NA rows or columns
y <- rowSums(DH, na.rm=TRUE)
table(y)
mean(y > 0) # Naive occupancy
n <- rowSums(!is.na(DH))
plot(table(n))
occSS0(y, n)
occSSrn0(y, n)  # Much lower AIC

# Look at effect of covars
covars <- dec.data[287:294]
head(covars)
covars$park <- as.factor(covars$park.ind) # needs to be a factor for MLE
occSSrnSite(y, n, lambda~park, data=covars)
# The order of the lambda's is 3,2,4,1,5,6
# Very small lambda for parks #2 and #4
# Variances can't be estimated -- investigate further

# Check covariates
colMeans(covars)
apply(covars, 2, sd) # The main covars have already been standardised
table(covars$park.ind)
table(covars$outreach)
table(covars$punishment) # Looks like these are park-level covars
with(covars, tapply(outreach, park.ind, sd))
with(covars, tapply(punishment, park.ind, sd)) # yes!!
# Extract the 6 values for the parks
( punish <- with(covars, tapply(punishment, park.ind, mean)) )
( oreach <- with(covars, tapply(outreach, park.ind, mean)) )
( area <- with(covars, tapply(parksize, park.ind, mean)) )
head(covars)
pairs(covars)
pairs(covars[, c(5,6,7)]) # Just the park-level covars
cor(covars[, c(5,6,7)])

( naive <- tapply(y, covars$park.ind, function(x) mean(x > 0)) )# park-wise naive occupancy
# No detections at parks 2 and 4 !! Check survey effort (trap-days)
( effort <- tapply(n, covars$park.ind, sum) ) # park-wise effort
table(covars$park.ind) # number of sites in each park
tapply(n, covars$park.ind, mean) # park-wise effort

round(cbind(park=1:6, area, punish, oreach, effort, naive), 3) # Summary table
# See discussion in email


# Run a model with no random effects, except for the park-level effect
# I like uniform priors for initial runs, you can see clearly if the prior is
#   constraining the posterior, as happens here. If that's the case, priors are
#   informative and need to be chosen carefully.
modelText <- "
model{
for(j in 1:J){ # sites 
# Biological model
N[j] ~ dpois(lambda[j])
log(lambda[j]) <- a0 + a1*ele[j] + a2*population[j] + a3*dis[j] + a4*parksize[j] +
a5*outreach[j] + a6*punishment[j] + park[park.ind[j]] 
#Detection model
y[j] ~ dbin(p[j], n[j])  # y[j] is the number of 1's for site j, out of n[j] days.
p[j] <- 1-(1-r[j])^N[j]
logit(r[j]) <- b0 + b1*cam_angle[j]
}

# Priors:
sd.p ~ dunif(0,20) 
tau.p <- 1/(sd.p*sd.p)
for(i in 1:6) {  # loop over parks
park[i] ~ dnorm(0, tau.p)
}
a0 ~ dunif(-10,10)
a1 ~ dunif(-10,10)
a2 ~ dunif(-10,10)
a3 ~ dunif(-10,10)
a4 ~ dunif(-5,10)
a5 ~ dunif(-5,10)
a6 ~ dunif(-5,50)  # upper limit increased after seeing early results, still a constraint!
b0 ~ dunif(-10,10)
b1 ~ dunif(-5,5)
}"
writeLines(modelText, con="model_fixedEff.txt")

jagsData <- as.list(covars[1:8])
jagsData$y <- y
jagsData$n <- n
jagsData$J <- length(y)
str(jagsData)

wanted <- c( "a0","a1", "a2", "a3", "a4", "a5", "a6", "b0", "b1","sd.p", "park","N")

inits <- function() list(N = y+1)

library(R2jags)
fit <- jags(jagsData, inits, wanted, model.file="model_fixedEff.txt",   # test run
            n.chains=3, n.iter=1000000, n.burnin =500000, n.thin=100) 
par <- c( "a2", "a3", "a4", "a5", "a6", "b2")
mcmcplots::mcmcplot(fit,parms=par)
mcmcplots::caterplot(fit,parms=par)


#Takes 3 mins -- n.effs too small.
( fit.fixed <- jags(jagsData, inits, wanted, model.file="model_fixedEff.txt", DIC=FALSE,    # test run
                    n.chains=3, n.iter=100000, n.adapt=10000, n.thin=10, parallel=TRUE) )
# Takes 20+ mins:
( fit.fixed <- jags(jagsData, inits, wanted, model.file="model_fixedEff.txt", DIC=FALSE,    # test run
                    n.chains=3, n.iter=1000000, n.adapt=10000, n.thin=100, parallel=FALSE) )

par(las=1)
plot(fit.fixed) # constraints on a4 and a6 show up
par(las=2)
crosscorr.plot(fit.fixed$samples, las=1)
# strong negative correlation between park[1]/park[3] and a1; makes sense - #1 and #3
#   have most data, so pretty sure what lambda is for these, so any change in a1 needs
#   corresponding change in park[].
fit.fixed <- fit
### plot prediction
a1 <- fit.fixed$BUGSoutput$sims.list$a1
a2 <- fit.fixed$BUGSoutput$sims.list$a2
a3 <- fit.fixed$BUGSoutput$sims.list$a3
a4 <- fit.fixed$BUGSoutput$sims.list$a4
a5 <- fit.fixed$BUGSoutput$sims.list$a5
a6 <- fit.fixed$BUGSoutput$sims.list$a6
park <- fit.fixed$BUGSoutput$sims.list$park
b2 <- fit.fixed$BUGSoutput$sims.list$b2

sitecovs<-read.csv("data/sitecov_temp.csv",header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)


"N[j] ~ dpois(lambda[j])
log(lambda[j]) <- a1 + park[park.ind[j]] + a2*ele[j] + a3*dis[j] + a4*parksize[j] +
a5*outreach[j] + a6*punishment[j] "

dec.data$park.ind
park


dis.dummmy=seq(min(sitecovs$dis),max(sitecovs$dis),,50)
dis1=(dis.dummmy-mean(sitecovs$dis))/sd(sitecovs$dis)
mele <- mean(dec.data$ele) 
mparksize <- mean(dec.data$parksize)
mreach <- mean(dec.data$outreach)
mpunish <- mean(dec.data$punishment)

lambda <- as.vector(rep(999,length(dis.dummmy)))
abundance <- matrix(999, nrow=length(dis.dummmy), length(a1))
for (i in 1:length(dis.dummmy)){
  for (k in 1:length(a1)) {
    lambda[i] <- log(a1[k] + park + a2[k]*mele + a3[k]*dis1[i] + a4[k]*mparksize + a5[k]*mreach + a6[k]*mpunish)
  }
  N[j] ~ dpois(lambda[j])
}
N <- ppois(lambda=100)

psi=matrix(0,nrow=length(uspecies1),ncol=dim(u)[1]) 
richness = matrix(0, nrow=length(eelev1), ncol=dim(u)[1])
for (j in 1:length(eelev1)) {
  for(i in 1: length(uspecies1)) {
    for (k in 1:dim(u)[1]) {
      N[j]=ppois(a1 + 1 + a2*ele + a3*dis1[j] + a4*parksizemean +
                   a5*outreachmean  + a6*punishmentmean ) 
    } }
  richness[j,] = apply(psi,2,sum)  
}   
richness4<-cbind(apply(richness,1,mean),apply(richness,1,quantile,0.975),apply(richness,1,quantile,0.025))
erichness<-cbind(elev.dummy,richness4) 

##### beta conf plot ############ beta conf plot ############ beta conf plot #######

library(reshape)
library(denstrip)
library(lattice)

## fast way

preds<-as.data.frame(cbind(a2,a3,a4,a5,a6,b2))
# sort effects by median size
idx<-sort(abs(apply(preds,2,median)),index.return=T,decreasing=F)$ix
idx2<-c(1,2,3,4,5,6)
# apply and sort labels
labs=c("elevation","population","parksize","punishment","outreach","camangle")[idx2]

mp=melt(preds[,idx2])
rpp=bwplot(variable~value,data=mp,xlab=list(label="Standardized beta coefficients",cex=1),
           ,panel = function(x, y) { 
             #grid.segments(1,0,0,0)
             xlist <- split(x, factor(y))
             for (i in seq(along=xlist))
               panel.denstrip(x=xlist[[i]], at=i)
           },par.settings = list(axis.line = list(col=NA)),scales=list(col=1,cex=1,x=list(col=1),y=list(draw=T,labels=labs)))
print(rpp)

mean(preds[,1],na.rm = TRUE)
mean(preds[,2],na.rm = TRUE)
mean(preds[,3],na.rm = TRUE)
mean(preds[,4],na.rm = TRUE)
mean(preds[,5],na.rm = TRUE)
mean(preds[,6],na.rm = TRUE)
mean(preds[,7],na.rm = TRUE)


# draw line at 0 across
trellis.focus("panel", 1, 1)
panel.abline(v=0,col=1,lty=2)
trellis.unfocus()
