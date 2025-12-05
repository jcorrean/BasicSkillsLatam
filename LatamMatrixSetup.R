load("Matrices/MatrizARG.RData")
load("Matrices/MatrizBRA.RData")
load("Matrices/MatrizCHL.RData")
load("Matrices/MatrizCOL.RData")
load("Matrices/MatrizCORI.RData")
load("Matrices/MatrizECU.RData")
load("Matrices/MatrizMEX.RData")
load("Matrices/MatrizURU.RData")
load("Matrices/MatrizVEN.RData")


load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

rm(list=setdiff(ls(), c("MATRIX_AR", "ARGTexts",
                        "MATRIZ_BR", "BRATexts",
                        "MATRIX_CL", "CHLTexts",
                        "MATRIZ_CO", "COLTexts",
                        "MATRIZ_CR", "CORITexts",
                        "MATRIZ_EC", "ECUTexts",
                        "MATRIX_MX", "MEXTexts",
                        "MATRIZ_UY", "URUTexts",
                        "MATRIZ_VE", "VENTexts")))

LatamNetwork <- do.call(rbind, list(MATRIX_AR,
                                    MATRIZ_BR,
                                    MATRIX_CL,
                                    MATRIZ_CO,
                                    MATRIZ_CR,
                                    MATRIZ_EC,
                                    MATRIX_MX,
                                    MATRIZ_UY,
                                    MATRIZ_VE))

LatamNodeProperties <- do.call(rbind, list(ARGTexts,
                                           BRATexts,
                                           CHLTexts,
                                           COLTexts,
                                           CORITexts,
                                           ECUTexts,
                                           MEXTexts,
                                           URUTexts,
                                           VENTexts))

rm(list=setdiff(ls(), c("LatamNetwork", "LatamNodeProperties")))
LatamNodeProperties$Program[LatamNodeProperties$Program == "Doctorado"] <- "PhD"
LatamNodeProperties$Program[LatamNodeProperties$Program == "Especialização"] <- "Specialization"
LatamNodeProperties$Program[LatamNodeProperties$Program == "Especialización"] <- "Specialization"
LatamNodeProperties$Program[LatamNodeProperties$Program == "Maestría"] <- "Master"
table(LatamNodeProperties$Program)
table(LatamNodeProperties$Country)

rownames(LatamNetwork)[1:514] <-paste0("AR", 1:514)
rownames(LatamNetwork)[515:1436] <-paste0("BR", 1:922)
rownames(LatamNetwork)[1437:1644] <- paste0("CL", 1:208)
rownames(LatamNetwork)[1645:1874] <- paste0("CO", 1:230)
rownames(LatamNetwork)[1875:1994] <- paste0("CR", 1:120)
rownames(LatamNetwork)[1995:2725] <- paste0("EC", 1:731)
rownames(LatamNetwork)[2726:3277] <- paste0("MX", 1:552)
rownames(LatamNetwork)[3278:3424] <- paste0("UY", 1:147)
rownames(LatamNetwork)[3425:3634] <- paste0("VE", 1:210)

vertices <- c(rownames(LatamNetwork), colnames(LatamNetwork))

library(network)
LatamNet <- network(LatamNetwork,
                     loops = FALSE,
                     directed = FALSE,
                     bipartite = TRUE,
                     vertices = vertices)
summary(LatamNet)

library(igraph)
bnR <- graph_from_biadjacency_matrix(LatamNetwork, directed = F)
bnR
tail(bipartite_mapping(bnR), 10)
V(bnR)$type <- bipartite_mapping(bnR)$type
V(bnR)$shape <- ifelse(V(bnR)$type, "circle", "square")
V(bnR)$color <- ifelse(V(bnR)$type, "green4", "red3")
V(bnR)$label.cex <- ifelse(V(bnR)$type, 0.5, 1)
V(bnR)$size <- sqrt(igraph::degree(bnR))
E(bnR)$color <- "lightgrey"
png(filename = "FR.png", width = 40, height = 18, units = "in", res = 300)
set.seed(8970)
plot(bnR, vertex.label = NA, layout = layout.bipartite, arrow.width = 0.1, arrow.size = 0.1)
dev.off()

ProgramsRegion <- data.frame(Degree.centrality = igraph::degree(bnR),
                             Closeness.centrality = igraph::closeness(bnR),
                             Betweennes.centrality = igraph::betweenness(bnR),
                             Eigenvector.centrality = igraph::eigen_centrality(bnR)$vector)
rownames(ProgramsRegion)
