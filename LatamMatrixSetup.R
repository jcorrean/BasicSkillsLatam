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

rm(list=setdiff(ls(), c("MATRIX_AR", "ARGTexts")))

