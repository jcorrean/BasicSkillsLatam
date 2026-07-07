library(network)
load("NetworkData/LatamNetwork.RData")
LatamNet
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
mcmc.diagnostics(model4)
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
mcmc.diagnostics(model5)
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
mcmc.diagnostics(model6)
summary(model6)  


pdf("Supplementary_MCMC_Diagnostics_Model6.pdf",
    width = 10,
    height = 8)

dev.off()


gof6 <- gof(
  model6,
  GOF = ~ b1degree + b2degree + twopath + model,
  control = control.gof.ergm(nsim = 100)
)

print(gof6)

plot(gof6)


# ==============================================================================
# MODELO 7: ERGM Mixto Bipartito (biMERGM) - Enfoque Kevork & Kauermann (2022)
# ==============================================================================

library(mgcv)
# library(bimergm) # Asegúrate de cargar el paquete o el script con la función

# Seteamos la semilla idéntica para asegurar reproducibilidad en la fase MCMC interna
set.seed(2758)

# Kevork y Kauermann sugieren que 10 iteraciones del bucle externo son suficientes
# para que los efectos aleatorios se estabilicen y actúen como un offset firme.
n_iteraciones <- 10

# Ejecución del Modelo 7 utilizando tu especificación estructural
# NOTA: La función bimergm() no acepta un argumento de control externo directo (control.ergm),
# ya que el autor programó de manera fija el método "Stepping" en el código fuente.
model7 <- bimergm(
  formula = LatamNet ~ edges + 
    b1cov("Brochure.Length") + 
    gwb1degree(decay = 0.5, fixed = TRUE) + 
    b2factor("vertex.names", levels = c("science",
                                        "speaking", 
                                        "reading_comprehension", 
                                        "active_listening",
                                        "learning_strategy")) + 
    b1factor("Program", levels = c("Master", "PhD")) + 
    b1cov("OECD"),
  iter = n_iteraciones
)

# ==============================================================================
# POST-PROCESAMIENTO Y EXTRACCIÓN DE RESULTADOS
# ==============================================================================

# 1. Resumen de los Coeficientes Estructurales Fijos (Purificados por el offset)
# Accedemos al sub-objeto ergm final alojado en la lista
summary(model7$model)

# 2. Diagnósticos MCMC del motor interno tras la última iteración
# Esto te generará el equivalente al PDF que me mostraste, pero bajo el Modelo 7
pdf("Supplementary_MCMC_Diagnostics_Model7.pdf", width = 10, height = 8)
mcmc.diagnostics(model7$model)
dev.off()

# 3. Extracción y Guardado de los Efectos Aleatorios de los 3,155 Programas (u_i)
# Extraemos la última columna (iteración 10) que representa el estado convergido
efectos_programas <- model7$b1random[, n_iteraciones]

# Guardamos los efectos aleatorios como atributo de nodo para análisis posteriores
# (por ejemplo, para ver qué universidades específicas tienen los programas con mayor propensión)
LatamNet %v% "b1_random_effects" <- efectos_programas

# 4. Visualización de la Heterogeneidad Latente del Modo 1
hist(efectos_programas, 
     breaks = 40, 
     col = "darkcyan", 
     border = "white",
     main = "Distribución de la Heterogeneidad Latente (Programas)",
     xlab = "Interceptos Aleatorios de Socialidad (u_i)")




save.image("~/Documents/GitHub/BasicSkillsLatam/Inferential_Results.RData")
