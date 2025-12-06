library(network)
load("NetworkData/LatamNetwork.RData")
library(ergm)
library(coda)
#SampledNetworks <- Networks(RegionalNetworks)
set.seed(2758)
model1 <- ergm(LatamNet ~ edges + b1cov("Brochure.Length"), 
               control = control.ergm(MCMC.samplesize = 100000, 
                                      MCMC.burnin = 10000, 
                                      MCMLE.maxit = 10))
summary(model1)

model2 <- ergm(LatamNet ~ edges +
                 b2factor("vertex.names", levels=c("science","speaking","active_listening","learning_strategy","active_learning")) +
                 b1cov("Brochure.Length"))
summary(model2)

model3 <- ergm(LatamNet ~ edges + 
                 b2factor("vertex.names", levels=c("science","speaking","active_listening","learning_strategy","active_learning")) +
                 b1cov("Brochure.Length") +
                 b1factor("Program", levels=c("Master", "PhD")))
summary(model3) # AIC: 36919

model4 <- ergm(LatamNet ~ edges + 
                 b2factor("vertex.names", levels=c("science","speaking","active_listening","learning_strategy","active_learning")) +
                 b1cov("Brochure.Length") +
                 b1factor("Program", levels=c("Master", "PhD"))+
                 b1cov("OECD"))
summary(model4) # AIC: 36919
