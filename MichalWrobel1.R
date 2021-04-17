#1. Napisz funkcję sprawdzająca czy 1 liczba jest podzielna przez druga użyj - %%

sprawdz = function(liczba1,liczba2) 
{
  if (liczba1%%liczba2 == 0){
    paste(liczba1,"jest podzielna przez",liczba2)
    }else{
      paste(liczba1,"NIE jest podzielna przez",liczba2)
  }
  }

sprawdz(3,2)

#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu.

v <- c(120,90)
mean(v)

#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.
#Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.

data <- read.csv("dane.csv", header = TRUE, sep = ";")
colnames(data)
cor(data$waga, data$wzrost)

#Dane są w bardzo mocny sposób ze sobą skolerowane. im osoba wyższa, tym ma większą wagę. 

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika 
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. 
#w kolejnych wierszach zawartość wierszy ramki danych ( tyle wierszy ile podaliśmy w argumencie ile. 
#ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, domyślna wartością będzie 1)


  stworzDataFrame<-function(ile=1){
    naglowki<-readline(prompt="Podaj nazwy kolumn po spacji:")
    kolumny<-(strsplit(naglowki," "))
    as.vector(kolumny)
    macierz<-matrix(NA, nrow=ile, ncol=lengths(kolumny))
    df<-data.frame(macierz)
    names(df)<-t(unlist(kolumny))
    for(column in colnames(df)){
      for(i in 1:ile){
        input<-readline(prompt = "Podaj wartosc wiersza:")
        df[i, column]<-input
      }}
    View(df)
  }
stworzDataFrame()

#5 Napisz funkcję , która pobiera sciezke Katalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
#UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.

#liczZplikow <- function(sciezka, nazwaKolumny ,jakaFunkcja,DlaIluPlikow=1) {

library(plyr)

liczZplikow <- function(sciezka, nazwaKolumny ,jakaFunkcja = "mean",DlaIluPlikow=1) {
  
  setwd(sciezka)
  lista <-list.files()[1:DlaIluPlikow]

  dataset <- ldply(lista, read.csv, header=TRUE)

  ready_data = (dataset %>% select(nazwaKolumny)%>%na.omit())
  
  if (jakaFunkcja == "mean") mean(ready_data)
  else if (jakaFunkcja == "max" ) max(ready_data)
  else if (jakaFunkcja == "min") min(ready_data)
  else if (jakaFunkcja == "median") median(ready_data)
  
  #View(ready_data)
  }

#liczZplikow ("../smogKrakow", nazwaKolumny='X142_temperature' ,jakaFunkcja='max', DlaIluPlikow=2)
  


