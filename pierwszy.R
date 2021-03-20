install.packages("httr")
install.packages("jsonlite")

 library(httr)
 require(jsonlite)

endpoint<- "https://api.openweathermap.org/data/2.5/weather?q=Warszawa&appid=1765994b51ed366c506d5dc0d0b07b77"

getWeather <- GET(endpoint)
weatherText <- content(getWeather,as="text")
weatherText
weatherJSON <- fromJSON(weatherText,flatten=FALSE)
weatherJSON
weatherDF <- as.data.frame(weatherJSON)
weatherDF 

v1<-c(1,2,3,4,5,6,7,8,9,10)
v1<-as.integer(v1)
v2<-as.vector(c(2,4,3),mode="integer")

wynik<- v1-v2
class(wynik)
wynik<- v1*v2
class(wynik)
wynik<- v1+v2
class(wynik)
wynik<- v1/v2
class(wynik)



wynik<- v1%%v2
wynik<- v1%/%v2
