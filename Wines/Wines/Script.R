# odczytuje plik konfiguracyjny
wine = read.csv("data.csv", header = FALSE)

#nadaje nazwy kolumna w tabeli
names(wine) = c("Cultivar", "Alcohol", "Malic_acid", "Ash", "Alkalinity_ash", "Magnesium", "Phenols", "Flavanoids", 
      "NF_phenols", "Proanthocyanins", "Color_intensity", "Hue", "OD", "Proline")

# "ziarno" -> set.seed to taki generator liczb losowych, podajac konkretna wartosc mozemy zreprodukowac konkretna symulacje 
set.seed(9850)

# runif(wartosc) -> zwraca losowe wartoœci z przedzialu (0,1), argument wejsciowy "wartosc" oznacza ile tych liczb ma byc wygenerowanych
# nrow(wartosc) -> zlicza ile wierszy znajduje sie w argumencie wejsciowym "wartosc"
# tworze tabele, ktora bedzie zbudowana jak tabela "wine", ale w komorki bedzie miala wpisane randomowe wartosci z przedzialu od 0 do 1
randomValues <- runif(nrow(wine))

# przetasowuje wiersze tabeli "wine" wedlug tabeli "randomValues"
shuffledWine = wine[order(randomValues),]

# normalizacja, czyli przeskalowanie wartosci do wartosci odpowiadajacej przedzialowi (0,1)
# w tym przypdaku funkcja przyjmuje wektor wartosci i odpowiednio je przeskalowuje
# np. normalize(c(10,20,30,40,50)) -> (0.00, 0.25, 0.50, 0.75, 1.0)
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

# normalizuje wszystkie kolumny bo sa atrybutami, pomijam pierwsza kolumne bo jest numerem grupy
shuffledWineNormalized <- as.data.frame(lapply(shuffledWine[, 2:ncol(shuffledWine)], normalize))
