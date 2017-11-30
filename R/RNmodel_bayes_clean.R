# RN model with park random effects #
library("dplyr")
library("jagsUI")

## read stuff ##
rm(list=ls(all=TRUE))
dec.data<-read.csv("data/dec.data.csv",header=TRUE, strip.white=TRUE)
J <- 115 # number of sites 
K <- 284 # number of survey 
dec.data <- list(y=as.matrix(dec.data[,2:285]), J=J, K=K, park.ind=dec.data$park.ind, ele=dec.data$ele, dis=dec.data$dis, parksize=dec.data$parksize, outreach=dec.data$outreach, punishment=dec.data$punishment,
                  cam_angle=dec.data$cam_angle)
### site cov ####
dec.params <- c("N","lambda","r","y", "p",
                "mu.a1","mu.a2","mu.a3","mu.a4","mu.a5","mu.a6",
                "mu.b1","mu.b2","mu.b3",
#                "sigma.a1","sigma.a2","sigma.a3","sigma.a4","sigma.a5","sigma.a6",
#               "sigma.b1","sigma.b2","sigma.b3",
#               "a1", "a2","a3","a4","a5","a6",
#                "b1","b2","b3",
                "park","sd.p")
sink("RNmodel_bayes.txt")
cat("
    model {
    
    # prior
    mu.a1 ~ dnorm(0,0.01)	
    mu.a2 ~ dnorm(0,0.01)	
    mu.a3 ~ dnorm(0,0.01)
    mu.a4 ~ dnorm(0,0.01)
    mu.a5 ~ dnorm(0,0.01)
    mu.a6 ~ dnorm(0,0.01)
    
    mu.b1 ~ dnorm(0,0.01)	
    mu.b2 ~ dnorm(0,0.01)	
    mu.b3 ~ dnorm(0,0.01)	
    
    sigma.a1 ~ dunif(0,10)	
    sigma.a2 ~ dunif(0,10)	
    sigma.a3 ~ dunif(0,10)	
    sigma.a4 ~ dunif(0,10)	
    sigma.a5 ~ dunif(0,10)	
    sigma.a6 ~ dunif(0,10)	
    
    sigma.b1 ~ dunif(0,10)
    sigma.b2 ~ dunif(0,10)
    sigma.b3 ~ dunif(0,10)
    
    tau.a1 <- pow(sigma.a1,-2)
    tau.a2 <- pow(sigma.a2,-2)
    tau.a3 <- pow(sigma.a3,-2)  
    tau.a4 <- pow(sigma.a4,-2)
    tau.a5 <- pow(sigma.a5,-2)
    tau.a6 <- pow(sigma.a6,-2)
    tau.b1 <- pow(sigma.b1,-2)
    tau.b2 <- pow(sigma.b2,-2)
    tau.b3 <- pow(sigma.b3,-2)
    
    for(j in 1:J){
    a1[j] ~ dnorm(mu.a1,tau.a1)
    a2[j] ~ dnorm(mu.a2,tau.a2)
    a3[j] ~ dnorm(mu.a3,tau.a3)
    a4[j] ~ dnorm(mu.a4,tau.a4)
    a5[j] ~ dnorm(mu.a5,tau.a5)
    a6[j] ~ dnorm(mu.a6,tau.a6)
    
    b1[j] ~ dnorm(mu.b1,tau.b1)
    b2[j] ~ dnorm(mu.b2,tau.b2)
    b3[j] ~ dnorm(mu.b3,tau.b3)
    }
    
    ## prior for the park random effect 
    
    sd.p ~ dunif(0,10) 
    tau.p <- 1/(sd.p*sd.p)
    for(p in 1:6){ # number of parks 
    park[p] ~ dnorm(0, tau.p)
    }
    
    #Abundance model with park random effects
    for(j in 1:J){ # sites 
    N[j] ~ dpois(lambda[j])
    log(lambda[j]) <- a1[j] + park[park.ind[j]] + a2[j]*ele[j] + a3[j]*dis[j] + a4[j]*parksize[j] +
    a5[j]*outreach[j] + a6[j]*punishment[j] 
    
    for(k in 1:K) { # in k sampling   
    #Detection model
    y[j,k] ~ dbern(p[j,k])
    p[j,k] <- 1-(1-r[j,k])^N[j]
    logit(r[j,k]) <- b1[j] + b3[j]*cam_angle[j] 
    }
    }
    }",fill=TRUE)
sink()

# initial 
m0 <- matrix(0, 115, 284)
y.int <- apply(m0, c(1,2), function(x) sample(c(0,1),1)) 

dec.inits = function(){ list (
  mu.a1=rnorm(1),mu.a2=rnorm(1),mu.a3=rnorm(1),mu.a4=rnorm(1),mu.a5=rnorm(1),mu.a6=rnorm(1),
  mu.b1=rnorm(1),mu.b2=rnorm(1),mu.b3=rnorm(1),#,N=dpois(j,lambda = 0.25), ### ?????????
  sigma.a1=dunif(1,min=0.5,max=1),sigma.a2=dunif(1,min=0.5,max=1),sigma.a3=dunif(1,min=0.5,max=1),
  sigma.a4=dunif(1,min=0.5,max=1),sigma.a5=dunif(1,min=0.5,max=1),sigma.a6=dunif(1,min=0.5,max=1),
  sigma.b1=dunif(1,min=0.5,max=1),sigma.b2=dunif(1,min=0.5,max=1),sigma.b3=dunif(1,min=0.5,max=1),
  y=y.int)
  }

fit <- jags(dec.data, inits=dec.inits, dec.params, model.file="RNmodel_bayes.txt",     # test run
            n.chains=3, n.iter=50000, n.burnin=20000, n.thin=20)

## results ###
#library("mcmcplots")
#library("ggplot2")
#par <- c("mu.a1","mu.a2","mu.a3","mu.a4","mu.a5","mu.a6",
#        "mu.b1","mu.b2","mu.b3","park")

#mcmcplot(fit, parm=par)


