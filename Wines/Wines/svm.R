library(gmodels)
library(e1071)
library(caret)

wine = read.csv("data.csv", header = FALSE)

names(wine) = c("Cultivar", "Alcohol", "Malic_acid", "Ash", "Alkalinity_ash", "Magnesium", "Phenols", "Flavanoids",
      "NF_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD", "Proline")

set.seed(9850)
randomValues <- runif(nrow(wine))
shuffledWine = wine[order(randomValues),]
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

# -------------------------------------------------------------------------------------
# Powiekszony zbior testujacy - nieznormalizowane, nieprzetasowane 
'wineTrainning <- wine[1:33, 2:ncol(wine)]
wineTesting <- wine[34:178, 2:ncol(wine)]
wineTrainningSource <- wine[1:33, 1]
wineTestingSource <- wine[34:178, 1]'

#Powiekszony zbior testujacy - nieznormalizowane, przetasowane 
'wineTrainning <- shuffledWine[1:33, 2:ncol(shuffledWine)]
wineTesting <- shuffledWine[34:178, 2:ncol(shuffledWine)]
wineTrainningSource <- shuffledWine[1:33, 1]
wineTestingSource <- shuffledWine[34:178, 1]'

#Powiekszony zbior testujacy - normalizowane, nieprzetasowane 
'wineNormalized <- as.data.frame(lapply(wine[, 2:ncol(wine)], normalize))
wineTrainning <- wineNormalized[1:33,]
wineTesting <- wineNormalized[34:178,]
wineTrainningSource <- wine[1:33, 1]
wineTestingSource <- wine[34:178, 1]'

#Powiekszony zbior testujacy - normalizowane, przetasowane 
'shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))
wineTrainning <- shuffledWineNormalized[1:33,]
wineTesting <- shuffledWineNormalized[34:178,]
wineTrainningSource <- shuffledWine[1:33, 1]
wineTestingSource <- shuffledWine[34:178, 1]'

#-------------------------------------------------------------------------------------------------
# Powiekszony zbior uczacy - nieznormalizowane, nieprzetasowane 
'wineTrainning <- wine[1:145, 2:ncol(wine)]
wineTesting <- wine[146:178, 2:ncol(wine)]
wineTrainningSource <- wine[1:145, 1]
wineTestingSource <- wine[146:178, 1]'

#Powiekszony zbior uczacy - nieznormalizowane, przetasowane 
'wineTrainning <- shuffledWine[1:145, 2:ncol(shuffledWine)]
wineTesting <- shuffledWine[146:178, 2:ncol(shuffledWine)]
wineTrainningSource <- shuffledWine[1:145, 1]
wineTestingSource <- shuffledWine[146:178, 1]'

#Powiekszony zbior uczacy - normalizowane, nieprzetasowane 
'wineNormalized <- as.data.frame(lapply(wine[, 2:ncol(wine)], normalize))
wineTrainning <- wineNormalized[1:145,]
wineTesting <- wineNormalized[146:178,]
wineTrainningSource <- wine[1:145, 1]
wineTestingSource <- wine[146:178, 1]'

#Powiekszony zbior uczacy - normalizowane, przetasowane 
shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))
wineTrainning <- shuffledWineNormalized[1:145,]
wineTesting <- shuffledWineNormalized[146:178,]
wineTrainningSource <- shuffledWine[1:145, 1]
wineTestingSource <- shuffledWine[146:178, 1]

#-------------------------------------------------------------------
# Rowne zbiory - nieznormalizowane, nieprzetasowane 
'wineTrainning <- wine[1:89, 2:ncol(wine)]
wineTesting <- wine[90:178, 2:ncol(wine)]
wineTrainningSource <- wine[1:89, 1]
wineTestingSource <- wine[90:178, 1]'

# Rowne zbiory - nieznormalizowane, przetasowane 
'wineTrainning <- shuffledWine[1:89, 2:ncol(shuffledWine)]
wineTesting <- shuffledWine[90:178, 2:ncol(shuffledWine)]
wineTrainningSource <- shuffledWine[1:89, 1]
wineTestingSource <- shuffledWine[90:178, 1]'

# Rowne zbiory - normalizowane, nieprzetasowane 
'wineNormalized <- as.data.frame(lapply(wine[, 2:ncol(wine)], normalize))
wineTrainning <- wineNormalized[1:89,]
wineTesting <- wineNormalized[90:178,]
wineTrainningSource <- wine[1:89, 1]
wineTestingSource <- wine[90:178, 1]'

# Rowne zbiory - normalizowane, przetasowane 
'shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))
wineTrainning <- shuffledWineNormalized[1:89,]
wineTesting <- shuffledWineNormalized[90:178,]
wineTrainningSource <- shuffledWine[1:89, 1]
wineTestingSource <- shuffledWine[90:178, 1]'

#-----------------------------------------------------------------------

# type - okresla czy algorytm ma dzia³aæ jako np maszyna klasyfikujaca lub regresjii,
#       przykladowe dostepne typy: C-classification, nu-classification, eps-regression
# kernel - u¿ywane podczas treningu i przewidywania,
#       dostepne typy jader: liniowy, wielomianowy, promieniowy, 
# degree - stopien dla jadra wielomianu (domyslnie 3)
# gamma - parametr wymagany dla wszystkich j¹der z wyj¹tkiem liniowych(domyœlnie:1 / (wymiar danych))
# cost - Koszt naruszenia ograniczeñ (domyœlnie: 1) --- jest to "C" sta³a terminu regularyzacji w formule Lagrange'a.

classifier1 = svm(formula = wineTrainningSource ~ ., data = wineTrainning, type = 'C-classification', kernel = 'radial')
test_pred1 = predict(classifier1, type = 'response', newdata = wineTesting)
CrossTable(wineTestingSource, test_pred1, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

result <- table(wineTestingSource, test_pred1)
View(paste("Result [%]", round(sum(diag(result)) / sum(result) * 100), sep = " = "))