library("feedeR")
library("tm")
library("httr")
library(readr)
library(RCurl)
library(XML)
xmlReader <- function(xml.url){
  script  <- getURL(xml.url)
  doc     <- xmlParse(script)
  titles<- xpathSApply(doc,'//item/title',xmlValue)
  descriptions<-xpathSApply(doc,'//item/description',xmlValue)
  pubdates<-xpathSApply(doc,'//item/pubDate',xmlValue)
  links <-xpathSApply(doc,'//item/link',xmlValue)
  dfa<-data.frame(cbind(titles,pubdates,links))
  dfa$titles<-as.character(dfa$titles)
  dfa$pubdates<-as.POSIXct(strptime(
    substr(pubdates, 0, nchar(pubdates)-4),  "%a, %d %b %Y %H:%M:%S"
  ))
  dfa$links<-as.character(dfa$links)
  return(dfa)
}

news <- read_csv("/newsDirectory/news.csv", col_names = FALSE)
colnames(news)<-c("url","source")
total <- data.frame()
for(i in 1:nrow(news))
{
  tryCatch(
    {
      #dftemp<-feed.extract(news$url[i])$items
      dftemp<-xmlReader(news$url[i])
      colnames(dftemp)<-c("title","date","link")
      dftemp$source<-NA
      dftemp$source<-news$source[i]
      total<-rbind(dftemp,total)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", news$url[i]))
      print(paste("Number: ",i,news$url[i]))
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", news$url[i]))
      return(NULL)
    },
    finally={
    }
  )
}
dftotal<-total
dftotal<-dftotal[complete.cases(dftotal), ]
dftotal$date <- as.POSIXct(dftotal$date, origin="1970-01-01", tz='CET')
dftotal <- dftotal[rev(order(dftotal$date)),]
dftotal<-unique(dftotal)
head(dftotal, 20)
fcon <- file("/var/www/R/output.html","w")
for (i in 1:nrow(dftotal))
{
  link1<-paste("<p>",as.Date(dftotal$date[i])," - <a href='", dftotal$link[i], "'>",dftotal$title[i],"</a>",dftotal$source[i],"</p>")
  writeLines(link1, con=fcon,sep="\n")
}
close(fcon)
