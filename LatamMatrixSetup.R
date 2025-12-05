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
