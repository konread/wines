'
odczytuje plik konfiguracyjny
'
wine = read.csv("data.csv", header = FALSE)

'
nadaje nazwy kolumna w tabeli
'
names(wine) = c("Cultivar", "Alcohol", "Malic_acid", "Ash", "Alkalinity_ash", "Magnesium", "Phenols", "Flavanoids", 
      "NF_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD", "Proline")

'
"ziarno" -> set.seed to taki generator liczb losowych, podajac konkretna wartosc mozemy zreprodukowac konkretna symulacje
' 
set.seed(9850)

'
runif(wartosc) -> zwraca losowe wartoœci z przedzialu (0,1), argument wejsciowy "wartosc" oznacza ile tych liczb ma byc wygenerowanych
nrow(wartosc) -> zlicza ile wierszy znajduje sie w argumencie wejsciowym "wartosc"
tworze tabele, ktora bedzie zbudowana jak tabela "wine", ale w komorki bedzie miala wpisane randomowe wartosci z przedzialu od 0 do 1
'
randomValues <- runif(nrow(wine))

'
przetasowuje wiersze tabeli "wine" wedlug tabeli "randomValues"
'

shuffledWine = wine[order(randomValues),]

'
normalizacja, czyli przeskalowanie wartosci do wartosci odpowiadajacej przedzialowi (0,1)
w tym przypdaku funkcja przyjmuje wektor wartosci i odpowiednio je przeskalowuje
np. normalize(c(10,20,30,40,50)) -> (0.00, 0.25, 0.50, 0.75, 1.0)
'

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

'
normalizuje wszystkie kolumny bo sa atrybutami, pomijam pierwsza kolumne bo jest numerem grupy
'

# zbiór przetasowany
# shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))

# zbiór nie przetasowany
# shuffledWineNormalized <- as.data.frame(lapply(wine[, 2:ncol(wine)], normalize))

# powiêkszony zbiór ucz¹cy
# wineTrainning <- shuffledWineNormalized[1:150,]
# wineTesting <- shuffledWineNormalized[151:178,]

# równe czêœci
# wineTrainning <- shuffledWineNormalized[1:89,]
# wineTesting <- shuffledWineNormalized[90:178,]

# powiêkszony zbiór testuj¹cy
# wineTrainning <- shuffledWineNormalized[1:23,]
# wineTesting <- shuffledWineNormalized[24:178,]

# dane nieznormalizowane, nieprzetasowane
# wineTrainning <- wine[1:150, 2:14]
# wineTesting <- wine[151:178, 2:14]

# dane nieznormalizowane, przetasowane
# wineTrainning <- shuffledWine[1:150, 2:14]
# wineTesting <- shuffledWine[151:178, 2:14]

# powiêkszony zbiór ucz¹cy
# wineTrainningSource <- shuffledWine[1:150, 1]
# wineTestingSource <- shuffledWine[151:178, 1]

# równe czêœci
# wineTrainningSource <- shuffledWine[1:89, 1]
# wineTestingSource <- shuffledWine[90:178, 1]

# powiêkszony zbiór testuj¹cy
# wineTrainningSource <- shuffledWine[1:23, 1]
# wineTestingSource <- shuffledWine[24:178, 1]

# dane nieznormalizowane, nieprzetasowane
# wineTrainningSource <- wine[1:150, 1]
# wineTestingSource <- wine[151:178, 1]

# dane nieznormalizowane, przetasowane
# wineTrainningSource <- shuffledWine[1:150, 1]
# wineTestingSource <- shuffledWine[151:178, 1]

require(class)

'
knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
train -- maciez lub lista wektorow przypadkow treningowych
test -- maciez lub lista wektorow przypadkow testowych
cl -- wspolczynnik klasyfikacji zbioru treningowego
k -- liczba rozwazanych sasiadow
l -- minimalne g³osowanie na podjêcie ostatecznej decyzji, w przeciwnym razie w¹tpienie
'

library(gmodels)

knnWineTestPred <- knn(wineTrainning, wineTesting, wineTrainningSource, k = 13)
#table(factor(knnWineTestPred))
result <- table(wineTestingSource, knnWineTestPred)

CrossTable(wineTestingSource, knnWineTestPred, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

View(paste("Result [%]", round(sum(diag(result)) / sum(result) * 100), sep = " = "))

#sum(knnWineTestPred == wineTestingSource) / length(wineTestingSource)*100
#library(ggplot2)
#ggplot(aes(shuffledWineNormalized$Alcohol, shuffledWineNormalized$Alkalinity_ash), data = shuffledWineNormalized) + geom_point(aes(color = factor(shuffledWine$Cultivar)))
'
KnnTestPrediction <- list()
accuracy <- numeric()

for (k in 1:100) {
   KnnTestPrediction[[k]] <- knn(wineTrainning, wineTesting, wineTrainningSource, k, prob = TRUE)
   accuracy[k] <- sum(KnnTestPrediction[[k]] == wineTestingSource) / length(wineTestingSource) * 100
}

plot(accuracy, type = "b", col = "dodgerblue", cex = 1, pch = 20,
     xlab = "k, liczba sasiadow", ylab = "% poprawnie sklasyfikowanych",
     main = "Dokladnosc kontra sasiedzi")
'