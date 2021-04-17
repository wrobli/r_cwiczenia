#install.packages("gtools")
library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)

remDr <- remoteDr(remoteServerAddr = "http://localhost", 
                                      port = 4445, 
                                      browserName = "chrome",
                                      newSession = TRUE)

remDr%>%go("https://www.otodom.pl/sprzedaz/mieszkanie/?page=")

wektorLinkow<-c()
wektorLinkowU<-wektorLinkow%>%unique()
mieszkania<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
  }
}



zrobWiersz<-function(w,wektorLinkowU,remDr){
  szczegoly<-NULL
  szczegoly<- remDr%>%go(wektorLinkowU[w])%>%findElements("class name","css-18h1kfv")
  print( wektorLinkowU[w] )
  listaSzczegolyOpis<-c()
  listaSzczegolyWartosci<-c()
  for(i in 1: length(szczegoly)){
    listaSzczegolyOpis<- c(listaSzczegolyOpis,szczegoly[[i]]%>%findElementsFromElement("class name","css-o4i8bk"))
    listaSzczegolyWartosci<-c(listaSzczegolyWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","css-1ytkscc"))
  }
  nazwyKolumn<- unlist( lapply(listaSzczegolyOpis,getElementText)%>% str_replace_all(":","") )
  wartosci<- unlist( lapply(listaSzczegolyWartosci,getElementText) )
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1
  
}
