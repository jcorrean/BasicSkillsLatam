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
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking", 
                                                   "reading_comprehension", 
                                                   "active_listening",
                                                   "learning_strategy")) +
                 b1cov("Brochure.Length"),               
               control = control.ergm(MCMC.samplesize = 100000,
                                      MCMC.burnin = 10000, 
                                      MCMLE.maxit = 10))
summary(model2)

model3 <- ergm(LatamNet ~ edges + 
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking",
                                                   "reading_comprehension",
                                                   "active_listening",
                                                   "learning_strategy")) +
                 b1cov("Brochure.Length") +
                 b1factor("Program", levels=c("Master", "PhD")),
               control = control.ergm(MCMC.samplesize = 100000,
                                      MCMC.burnin = 10000, 
                                      MCMLE.maxit = 10))
summary(model3) # AIC: 36919

model4 <- ergm(LatamNet ~ edges + 
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking",
                                                   "reading_comprehension",
                                                   "active_listening",
                                                   "learning_strategy")) +
                 b1cov("Brochure.Length") +
                 b1factor("Program", levels=c("Master", "PhD"))+
                 b1cov("OECD"),
               control = control.ergm(MCMC.samplesize = 100000,
                                      MCMC.burnin = 10000, 
                                      MCMLE.maxit = 10))
summary(model4) # AIC: 36919

library(parallel)
n_cores <- detectCores() - 1 
backend_control <- control.ergm(
  MCMC.samplesize = 50000, # Reducido de 100k a 50k: óptimo para convergencia sin quemar RAM
  MCMC.burnin = 10000, 
  MCMLE.maxit = 15,        # Un poco más de margen para ayudar a la convergencia
  parallel = n_cores, 
  parallel.type = "PSOCK"
)

model5_fixed <- ergm(LatamNet ~ edges + 
                       b2factor("vertex.names", levels = c("science", "speaking", 
                                                           "reading_comprehension", 
                                                           "active_listening", 
                                                           "learning_strategy")) +
                       b1cov("Brochure.Length") +
                       b1factor("Program", levels = c("Master", "PhD")) +
                       b1cov("OECD") +
                       gwb1degree(decay = 0.5, fixed = TRUE), # Aplicado al MODO 1 (Programas)
                     control = backend_control)
summary(model5_fixed)
