library(network)
load("NetworkData/LatamNetwork.RData")
library(ergm)
library(coda)
library(parallel)
n_cores <- detectCores() - 1 
backend_control <- control.ergm(
  MCMC.samplesize = 50000, # Reducido de 100k a 50k: óptimo para convergencia sin quemar RAM
  MCMC.burnin = 10000, 
  MCMLE.maxit = 15,        # Un poco más de margen para ayudar a la convergencia
  parallel = n_cores, 
  parallel.type = "PSOCK"
)
set.seed(2758)
model1 <- ergm(LatamNet ~ edges,
               control = backend_control)
summary(model1)

model2 <- ergm(LatamNet ~ edges + 
                 b1cov("Brochure.Length"), 
               control = backend_control)
summary(model2)

model3 <- ergm(LatamNet ~ edges + 
                 b1cov("Brochure.Length") +
                 gwb1degree(decay = 0.5, fixed = TRUE),
               control = backend_control)
mcmc.diagnostics(model3)
summary(model3)


model4 <- ergm(LatamNet ~ edges + 
                 b1cov("Brochure.Length") +
                 gwb1degree(decay = 0.5, fixed = TRUE) +
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking", 
                                                   "reading_comprehension", 
                                                   "active_listening",
                                                   "learning_strategy")),
               control = backend_control)
summary(model4)

model5 <- ergm(LatamNet ~ edges + 
                 b1cov("Brochure.Length") +
                 gwb1degree(decay = 0.5, fixed = TRUE) +
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking", 
                                                   "reading_comprehension", 
                                                   "active_listening",
                                                   "learning_strategy")) +
                 b1factor("Program", levels=c("Master", "PhD")),
               control = backend_control)
summary(model5)

model6 <- ergm(LatamNet ~ edges + 
                 b1cov("Brochure.Length") +
                 gwb1degree(decay = 0.5, fixed = TRUE) +
                 b2factor("vertex.names", levels=c("science",
                                                   "speaking", 
                                                   "reading_comprehension", 
                                                   "active_listening",
                                                   "learning_strategy")) +
                 b1factor("Program", levels=c("Master", "PhD")) +
                 b1cov("OECD"),
               control = backend_control)
summary(model6)  


gof6 <- gof(
  model6,
  GOF = ~ degree +
    distance +
    espartners +
    dspartners
)

print(gof6)

plot(gof6)

  

save.image("~/Documents/GitHub/BasicSkillsLatam/Inferential_Results.RData")
