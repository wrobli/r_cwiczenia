library(rvest)
library(xml2)
library(dplyr)
library(gtools)

# pobranie Linkow do scrapper-a

linki <- c()
for(i in 1:10){
  newUrl<- paste0("https://www.otomoto.pl/osobowe/alfa-romeo/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  linki<-c(linki, xml_attr(result,"href"))
}

linki_u <-linki%>%unique()


# utworzenie rekordu w bazie

zrobWierszRvest<- function(w,linki_u){
  newUrl<-linki_u[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".offer-price__number")%>%html_text()

  v<-page %>% xml_find_all('/html/body/div[4]/main/div[2]/div[1]/div[2]/div[1]/div[1]/div[3]/div[1]/*/*/*')%>%html_text()%>%na.omit()
  indexy<- seq(1,length(v),1)
  nazwyKolumn<- (v[indexy%%2==1])
  wartosci<-(v[indexy%%2==0])
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
  df1
}

#wyzerowanie data frame
samochody <- NULL

# petla uruchamiajaca + obsluga bledow
for(w in 1:length(linki_u)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,linki_u),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(samochody)){
    samochody<-df1
  }else{
    samochody<-smartbind(samochody,df1)
  }
}

#wyswietlenie wynikow
View(samochody)


