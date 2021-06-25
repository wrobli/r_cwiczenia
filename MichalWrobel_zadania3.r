library(DBI)
library(dplyr)
library(RSQLite)


# zadanie 1 - rankAccount 

rankAccount <- function(dataFrame,colName,groupName,valueSort,num){
  df <- read.csv(dataFrame)
  new_df <- df %>% 
  select(colName, valueSort) %>%
  filter(df[[colName]] == groupName) %>%
  arrange(desc(!!sym(valueSort))) %>%
  head(num)
  
  return (new_df)
}

rankAccount("konta.csv", "occupation", "NAUCZYCIEL", "saldo", 10)


# zadanie 2 Datachunk

rankAccountBigDatatoChunk <- function(filename, size, colName, groupName, valueSort, num){
  fileConnection <- file(description = filename, open = "r")
  data <- read.table(fileConnection,nrows = size, header=TRUE, fill = TRUE, sep = ',')
  columnsNames <- names(data)
  output <- NULL
  repeat{
    
    if (nrow(data) == 0){
      close(fileConnection)
      break
    }
    
    data <- na.omit(data)
    data <- read.table(fileConnection, nrows = size, col.names = columnsNames, fill = TRUE, sep = ',') 
    new_data <- data %>%
    select(colName, valueSort) %>%
    filter(data[[colName]] == groupName)%>%
    arrange(desc(!!sym(valueSort)))
    head(num)
    
    output <- rbind(output,new_data) %>% 
    arrange(desc(!!sym(valueSort))) %>%
    head(num)
  }
    return(output)
}
rankAccountBigDatatoChunk("konta.csv", 1000, "occupation", "NAUCZYCIEL", "saldo", 5) 


# zadanie 3 SQLite

tabelaZbazyDanych<-function(filepath,dbpath,tablename,size,sep=",",header=TRUE,delete=TRUE){
  ap<-!delete
  ov<-delete
  fileConnection<- file(description = filepath,open="r")
  dbConn<-dbConnect(SQLite(),dbpath)
  data<-read.table(fileConnection,nrows=size,header = header,fill=TRUE,sep=sep)
  columnsNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  repeat{
    if(nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    data<-read.table(fileConnection,nrows=size,col.names=columnsNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}


tabelaZbazyDanych("konta.csv", "konta.sqlite", "konta", 1000)

dbp <- "konta.sqlite"
con <- dbConnect(SQLite(),dbp)

dbGetQuery(con, "SELECT occupation, saldo FROM konta WHERE occupation = 'NAUCZYCIEL' ORDER BY saldo DESC LIMIT 10;")

dbDisconnect(con)
