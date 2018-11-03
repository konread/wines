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
#---------------------------------------------------
# normalizowanie, zbiór przetasowany
#---------------------------------------------------

shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))

#---------------------------------------------------
# normalizowanie, zbiór nieprzetasowany
#---------------------------------------------------

# shuffledWineNormalized <- as.data.frame(lapply(wine[, 2:ncol(wine)], normalize))

#---------------------------------------------------

# wineTrainning <- shuffledWineNormalized[1:145,]
# wineTesting <- shuffledWineNormalized[146:178,]

# wineTrainning <- shuffledWineNormalized[1:89,]
# wineTesting <- shuffledWineNormalized[90:178,]

# wineTrainning <- shuffledWineNormalized[1:33,]
# wineTesting <- shuffledWineNormalized[34:178,]

#---------------------------------------------------

# wineTrainning <- wine[1:145, 2:14]
# wineTesting <- wine[146:178, 2:14]

# wineTrainning <- wine[1:89, 2:14]
# wineTesting <- wine[90:178, 2:14]

 wineTrainning <- wine[1:33, 2:14]
 wineTesting <- wine[34:178, 2:14]

#---------------------------------------------------

# wineTrainning <- shuffledWine[1:145, 2:14]
# wineTesting <- shuffledWine[146:178, 2:14]

# wineTrainning <- shuffledWine[1:89, 2:14]
# wineTesting <- shuffledWine[90:178, 2:14]

# wineTrainning <- shuffledWine[1:33, 2:14]
# wineTesting <- shuffledWine[34:178, 2:14]

#---------------------------------------------------

# wineTrainningSource <- shuffledWine[1:145, 1]
# wineTestingSource <- shuffledWine[146:178, 1]

# wineTrainningSource <- shuffledWine[1:89, 1]
# wineTestingSource <- shuffledWine[90:178, 1]

# wineTrainningSource <- shuffledWine[1:33, 1]
# wineTestingSource <- shuffledWine[34:178, 1]

#---------------------------------------------------

# wineTrainningSource <- wine[1:145, 1]
# wineTestingSource <- wine[146:178, 1]

# wineTrainningSource <- wine[1:89, 1]
# wineTestingSource <- wine[90:178, 1]

 wineTrainningSource <- wine[1:33, 1]
 wineTestingSource <- wine[34:178, 1]

training <- cbind(wineTrainning, wineTrainningSource)
testing <- cbind(wineTesting, wineTestingSource)

require(class)
library(gmodels)

tree <- rpart(training$wineTrainningSource ~ ., data = training, method = 'class')
tree.pred <- predict(tree, testing, type = "class")
result <- table(testing$wineTestingSource, tree.pred)

CrossTable(testing$wineTestingSource, tree.pred, prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)

View(paste("Result [%]", round(sum(diag(result)) / sum(result) * 100), sep = " = "))