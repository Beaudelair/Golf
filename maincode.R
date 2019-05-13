installed.packages("dplyr")
library(dplyr)
library(tidyr)



PGA <- PGA_Data_Historical[-3]

PGA <- as_tibble(PGA)
PGA <- PGA %>% spread(key=Variable, value= Value) #transform data to a wide format 


#supprimer les lignes où il y a plus de 100NA 
PGA2[] <- lapply(PGA2, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

sapply(PGA2, class)
PGA3<- PGA2[rowSums(is.na(PGA2[,c(3:2074)])) <=1000, ]

#enlever les colonnes o? il reste encore 90% de NA
miss <- c()
for(i in 1:ncol(PGA3)) {
  if(length(which(is.na(PGA3[,i]))) > 0.90*nrow(PGA3)) miss <- append(miss,i) 
}
PGA4 <- PGA3[,-miss]

# git checkout -b new-feature
# git push origin new-feature
#Comment supprimer des variables qui contiennent un certain string, reste plus qu'? trouver comment en combiner plusieurs
install.packages("data.table")
library(data.table)
PGA5 <- PGA4[,!grepl("FedExCup", colnames(PGA4))]
setDT(PGA5)
ind = PGA5[, lapply(.SD, function(x) grepl("FedExCup", x, perl=TRUE))] 
PGA5[,which(colSums(ind) == 0), with = FALSE]

#création de la nouvelle base de donnée avec web scrapping

install.packages("rvest")
install.packages("selectr")
install.packages("xml2")
install.packages("XML")
install.packages("stringr")
install.packages("twitteR")
install.packages("purrr")
install.packages("tidytext")

library(xml2)
library(rvest)
library(stringr)
library(XML)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)

#première variable concernant la longueur du premier coup par joueur

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  Driving.Distance <-as_tibble(tbls_ls[[1]])
  
  Driving.Distance <- Driving.Distance[,-c(1,2,4,6)]
  Driving.Distance <- Driving.Distance %>% 
    rename(NAME = `PLAYER NAME`, 
           Attempted = `TOTAL DRIVES`, 
           "Driving Distance" = AVG.)
  
  Year <- nrow (Driving.Distance)
  Driving.Distance$Year <- rep(date, time= nrow (Driving.Distance))
  return(Driving.Distance)
}

Driving.distance.2018 <- scraping('https://www.pgatour.com/stats/stat.101.2018.html', 2018)
Driving.distance.2017 <- scraping('https://www.pgatour.com/stats/stat.101.2017.html', 2017)
Driving.distance.2016 <- scraping('https://www.pgatour.com/stats/stat.101.2016.html', 2016)
Driving.distance.2015 <- scraping('https://www.pgatour.com/stats/stat.101.2015.html', 2015)
Driving.distance.2014 <- scraping('https://www.pgatour.com/stats/stat.101.2014.html', 2014)
Driving.distance.2013 <- scraping('https://www.pgatour.com/stats/stat.101.2013.html', 2013)
Driving.distance.2012 <- scraping('https://www.pgatour.com/stats/stat.101.2012.html', 2012)
Driving.distance.2011 <- scraping('https://www.pgatour.com/stats/stat.101.2011.html', 2011)
Driving.distance.2010 <- scraping('https://www.pgatour.com/stats/stat.101.2010.html', 2010)
Driving.distance.2009 <- scraping('https://www.pgatour.com/stats/stat.101.2009.html', 2009)
Driving.distance.2008 <- scraping('https://www.pgatour.com/stats/stat.101.2008.html', 2008)


Driving.distance.final <- rbind(Driving.distance.2018,Driving.distance.2017,Driving.distance.2016,Driving.distance.2015,Driving.distance.2014,Driving.distance.2013,Driving.distance.2012,Driving.distance.2011,Driving.distance.2010,Driving.distance.2009,Driving.distance.2008)

#maintenant une variable concernant la précision, c'est à dire à combien de pourcent un drive touche un fairway
scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Driving.Accuracy <-as_tibble(tbls_ls[[1]])
  
  Driving.Accuracy <- Driving.Accuracy[,-c(1,2,4,6,7)]
  Driving.Accuracy <- Driving.Accuracy %>% 
    rename(NAME = `PLAYER NAME`, 
           "Accuracy in %" = `%`)
  
  Year <- nrow (Driving.Accuracy)
  Driving.Accuracy$Year <- rep(date, time= nrow (Driving.Accuracy))
  return(Driving.Accuracy)
}

Driving.Accuracy.2018 <- scraping('https://www.pgatour.com/stats/stat.102.2018.html', 2018)
Driving.Accuracy.2017 <- scraping('https://www.pgatour.com/stats/stat.102.2017.html', 2017)
Driving.Accuracy.2016 <- scraping('https://www.pgatour.com/stats/stat.102.2016.html', 2016)
Driving.Accuracy.2015 <- scraping('https://www.pgatour.com/stats/stat.102.2015.html', 2015)
Driving.Accuracy.2014 <- scraping('https://www.pgatour.com/stats/stat.102.2014.html', 2014)
Driving.Accuracy.2013 <- scraping('https://www.pgatour.com/stats/stat.102.2013.html', 2013)
Driving.Accuracy.2012 <- scraping('https://www.pgatour.com/stats/stat.102.2012.html', 2012)
Driving.Accuracy.2011 <- scraping('https://www.pgatour.com/stats/stat.102.2011.html', 2011)
Driving.Accuracy.2010 <- scraping('https://www.pgatour.com/stats/stat.102.2010.html', 2010)
Driving.Accuracy.2009 <- scraping('https://www.pgatour.com/stats/stat.102.2009.html', 2009)
Driving.Accuracy.2008 <- scraping('https://www.pgatour.com/stats/stat.102.2008.html', 2008)


Driving.Accuracy.final <- rbind(Driving.Accuracy.2018,Driving.Accuracy.2017,Driving.Accuracy.2016,Driving.Accuracy.2015,Driving.Accuracy.2014,Driving.Accuracy.2013,Driving.Accuracy.2012,Driving.Accuracy.2011,Driving.Accuracy.2010,Driving.Accuracy.2009,Driving.Accuracy.2008)


#bon maintenant variable un peu compliquée à expliquer on en parlera, mais en gros c'est combien de fois la balle arrive sur le green avec le bon nombre de coups imparti


scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  GIR <-as_tibble(tbls_ls[[1]])
  
  GIR <- GIR[,-c(1,2,4,6,7,8)]
  GIR <- GIR %>% 
    rename(NAME = `PLAYER NAME`, 
           "Green in regulation %" = `%`)
  
  Year <- nrow (GIR)
  GIR$Year <- rep(date, time= nrow (GIR))
  return(GIR)
}

GIR.2018 <- scraping('https://www.pgatour.com/stats/stat.103.2018.html', 2018)
GIR.2017 <- scraping('https://www.pgatour.com/stats/stat.103.2017.html', 2017)
GIR.2016 <- scraping('https://www.pgatour.com/stats/stat.103.2016.html', 2016)
GIR.2015 <- scraping('https://www.pgatour.com/stats/stat.103.2015.html', 2015)
GIR.2014 <- scraping('https://www.pgatour.com/stats/stat.103.2014.html', 2014)
GIR.2013 <- scraping('https://www.pgatour.com/stats/stat.103.2013.html', 2013)
GIR.2012 <- scraping('https://www.pgatour.com/stats/stat.103.2012.html', 2012)
GIR.2011 <- scraping('https://www.pgatour.com/stats/stat.103.2011.html', 2011)
GIR.2010 <- scraping('https://www.pgatour.com/stats/stat.103.2010.html', 2010)
GIR.2009 <- scraping('https://www.pgatour.com/stats/stat.103.2009.html', 2009)
GIR.2008 <- scraping('https://www.pgatour.com/stats/stat.103.2008.html', 2008)


GIR.FINAL <- rbind(GIR.2018,GIR.2017,GIR.2016,GIR.2015,GIR.2014,GIR.2013,GIR.2012,GIR.2011,GIR.2010,GIR.2009,GIR.2008)


#Proximity to Hole from 20-30 yards, c'est la à quelle distance du trou arrive un joueur depuis 20-30 m

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Distance.from.20.30 <-as_tibble(tbls_ls[[1]])
  
  Distance.from.20.30 <- Distance.from.20.30[,-c(1,2,4,6,8)]
  Distance.from.20.30 <- Distance.from.20.30 %>% 
    rename(NAME = `PLAYER NAME`,
           "Distance Left" = `AVG DTP`)
  
  Year <- nrow (Distance.from.20.30)
  Distance.from.20.30$Year <- rep(date, time= nrow (Distance.from.20.30))
  return(Distance.from.20.30)
}

Distance.from.20.30.2018 <- scraping('https://www.pgatour.com/stats/stat.380.2018.html', 2018)
Distance.from.20.30.2017 <- scraping('https://www.pgatour.com/stats/stat.380.2017.html', 2017)
Distance.from.20.30.2016 <- scraping('https://www.pgatour.com/stats/stat.380.2016.html', 2016)
Distance.from.20.30.2015 <- scraping('https://www.pgatour.com/stats/stat.380.2015.html', 2015)
Distance.from.20.30.2014 <- scraping('https://www.pgatour.com/stats/stat.380.2014.html', 2014)
Distance.from.20.30.2013 <- scraping('https://www.pgatour.com/stats/stat.380.2013.html', 2013)
Distance.from.20.30.2012 <- scraping('https://www.pgatour.com/stats/stat.380.2012.html', 2012)
Distance.from.20.30.2011 <- scraping('https://www.pgatour.com/stats/stat.380.2011.html', 2011)
Distance.from.20.30.2010 <- scraping('https://www.pgatour.com/stats/stat.380.2010.html', 2010)
Distance.from.20.30.2009 <- scraping('https://www.pgatour.com/stats/stat.380.2009.html', 2009)
Distance.from.20.30.2008 <- scraping('https://www.pgatour.com/stats/stat.380.2008.html', 2008)


Distance.from.20.30.FINAL <- rbind(Distance.from.20.30.2018,Distance.from.20.30.2017,Distance.from.20.30.2016,Distance.from.20.30.2015,Distance.from.20.30.2014,Distance.from.20.30.2013,Distance.from.20.30.2012,Distance.from.20.30.2011,Distance.from.20.30.2010,Distance.from.20.30.2009,Distance.from.20.30.2008)

#bon ensuite on s'interesse au petit jeu donc au putting, la c'est combien de putt sont pris en moyenne par parcours, le chiffre classique devrait être 36, si on compte 2 putt par trou

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Putts.per.round <-as_tibble(tbls_ls[[1]])
  
  Putts.per.round <- Putts.per.round[,-c(1,2,4,6,7,8)]
  Putts.per.round <- Putts.per.round %>%
    rename(NAME = `PLAYER NAME`,
           "Average # of putts" = `AVG`)
  
  Year <- nrow (Putts.per.round)
  Putts.per.round$Year <- rep(date, time= nrow (Putts.per.round))
  return(Putts.per.round)
}

Putts.per.round.2018 <- scraping('https://www.pgatour.com/stats/stat.119.2018.html', 2018)
Putts.per.round.2017 <- scraping('https://www.pgatour.com/stats/stat.119.2017.html', 2017)
Putts.per.round.2016 <- scraping('https://www.pgatour.com/stats/stat.119.2016.html', 2016)
Putts.per.round.2015 <- scraping('https://www.pgatour.com/stats/stat.119.2015.html', 2015)
Putts.per.round.2014 <- scraping('https://www.pgatour.com/stats/stat.119.2014.html', 2014)
Putts.per.round.2013 <- scraping('https://www.pgatour.com/stats/stat.119.2013.html', 2013)
Putts.per.round.2012 <- scraping('https://www.pgatour.com/stats/stat.119.2012.html', 2012)
Putts.per.round.2011 <- scraping('https://www.pgatour.com/stats/stat.119.2011.html', 2011)
Putts.per.round.2010 <- scraping('https://www.pgatour.com/stats/stat.119.2010.html', 2010)
Putts.per.round.2009 <- scraping('https://www.pgatour.com/stats/stat.119.2009.html', 2009)
Putts.per.round.2008 <- scraping('https://www.pgatour.com/stats/stat.119.2008.html', 2008)


Putts.per.round.FINAL <- rbind(Putts.per.round.2018,Putts.per.round.2017,Putts.per.round.2016,Putts.per.round.2015,Putts.per.round.2014,Putts.per.round.2013,Putts.per.round.2012,Putts.per.round.2011,Putts.per.round.2010,Putts.per.round.2009,Putts.per.round.2008)

#ensuite le score moyen par parcours

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Average.scoring <-as_tibble(tbls_ls[[1]])
  
  Average.scoring <- Average.scoring[,-c(1,2,4,6,7,8)]
  Average.scoring <- Average.scoring %>%
    rename(NAME = `PLAYER NAME`,
           "Average Scoring" = `AVG`)
  
  Year <- nrow (Average.scoring)
  Average.scoring$Year <- rep(date, time= nrow (Average.scoring))
  return(Average.scoring)
}

Average.scoring.2018 <- scraping('https://www.pgatour.com/stats/stat.120.2018.html', 2018)
Average.scoring.2017 <- scraping('https://www.pgatour.com/stats/stat.120.2017.html', 2017)
Average.scoring.2016 <- scraping('https://www.pgatour.com/stats/stat.120.2016.html', 2016)
Average.scoring.2015 <- scraping('https://www.pgatour.com/stats/stat.120.2015.html', 2015)
Average.scoring.2014 <- scraping('https://www.pgatour.com/stats/stat.120.2014.html', 2014)
Average.scoring.2013 <- scraping('https://www.pgatour.com/stats/stat.120.2013.html', 2013)
Average.scoring.2012 <- scraping('https://www.pgatour.com/stats/stat.120.2012.html', 2012)
Average.scoring.2011 <- scraping('https://www.pgatour.com/stats/stat.120.2011.html', 2011)
Average.scoring.2010 <- scraping('https://www.pgatour.com/stats/stat.120.2010.html', 2010)
Average.scoring.2009 <- scraping('https://www.pgatour.com/stats/stat.120.2009.html', 2009)
Average.scoring.2008 <- scraping('https://www.pgatour.com/stats/stat.120.2008.html', 2008)


Average.scoring.FINAL <- rbind(Average.scoring.2018,Average.scoring.2017,Average.scoring.2016,Average.scoring.2015,Average.scoring.2014,Average.scoring.2013,Average.scoring.2012,Average.scoring.2011,Average.scoring.2010,Average.scoring.2009,Average.scoring.2008)

#ensuite le score moyen par dernier round

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Last.round.scoring <-as_tibble(tbls_ls[[1]])
  
  Last.round.scoring <- Last.round.scoring[,-c(1,2,5,6)]
  Last.round.scoring <- Last.round.scoring %>%
    rename(NAME = `PLAYER NAME`,
           "Average last round Scoring" = `AVG`)
  
  Year <- nrow (Last.round.scoring)
  Last.round.scoring$Year <- rep(date, time= nrow (Last.round.scoring))
  return(Last.round.scoring)
}

Last.round.scoring.2018 <- scraping('https://www.pgatour.com/stats/stat.118.2018.html', 2018)
Last.round.scoring.2017 <- scraping('https://www.pgatour.com/stats/stat.118.2017.html', 2017)
Last.round.scoring.2016 <- scraping('https://www.pgatour.com/stats/stat.118.2016.html', 2016)
Last.round.scoring.2015 <- scraping('https://www.pgatour.com/stats/stat.118.2015.html', 2015)
Last.round.scoring.2014 <- scraping('https://www.pgatour.com/stats/stat.118.2014.html', 2014)
Last.round.scoring.2013 <- scraping('https://www.pgatour.com/stats/stat.118.2013.html', 2013)
Last.round.scoring.2012 <- scraping('https://www.pgatour.com/stats/stat.118.2012.html', 2012)
Last.round.scoring.2011 <- scraping('https://www.pgatour.com/stats/stat.118.2011.html', 2011)
Last.round.scoring.2010 <- scraping('https://www.pgatour.com/stats/stat.118.2010.html', 2010)
Last.round.scoring.2009 <- scraping('https://www.pgatour.com/stats/stat.118.2009.html', 2009)
Last.round.scoring.2008 <- scraping('https://www.pgatour.com/stats/stat.118.2008.html', 2008)


Last.round.scoring.FINAL <- rbind(Last.round.scoring.2018,Last.round.scoring.2017,Last.round.scoring.2016,Last.round.scoring.2015,Last.round.scoring.2014,Last.round.scoring.2013,Last.round.scoring.2012,Last.round.scoring.2011,Last.round.scoring.2009,Last.round.scoring.2008)



#le nombre de vicoire

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Victory <-as_tibble(tbls_ls[[1]])
  
  Victory <- Victory[,-c(1,2,4)]
  Victory <- Victory %>%
    rename(NAME = `PLAYER NAME`,
           "# of Victories" = `VICTORIES`)
  
  Year <- nrow (Victory)
  Victory$Year <- rep(date, time= nrow (Victory))
  return(Victory)
}

Victory.2018 <- scraping('https://www.pgatour.com/stats/stat.300.2018.html', 2018)
Victory.2017 <- scraping('https://www.pgatour.com/stats/stat.300.2017.html', 2017)
Victory.2016 <- scraping('https://www.pgatour.com/stats/stat.300.2016.html', 2016)
Victory.2015 <- scraping('https://www.pgatour.com/stats/stat.300.2015.html', 2015)
Victory.2014 <- scraping('https://www.pgatour.com/stats/stat.300.2014.html', 2014)
Victory.2013 <- scraping('https://www.pgatour.com/stats/stat.300.2013.html', 2013)
Victory.2012 <- scraping('https://www.pgatour.com/stats/stat.300.2012.html', 2012)
Victory.2011 <- scraping('https://www.pgatour.com/stats/stat.300.2011.html', 2011)
Victory.2010 <- scraping('https://www.pgatour.com/stats/stat.300.2010.html', 2010)
Victory.2009 <- scraping('https://www.pgatour.com/stats/stat.300.2009.html', 2009)
Victory.2008 <- scraping('https://www.pgatour.com/stats/stat.300.2008.html', 2008)


Victory.FINAL <- rbind(Victory.2018,Victory.2017,Victory.2016,Victory.2015,Victory.2014,Victory.2013,Victory.2012,Victory.2011,Victory.2010,Victory.2009,Victory.2008)


#nombre de fois où le joueur a fini dans le top 10

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Top10 <-as_tibble(tbls_ls[[1]])
  
  Top10 <- Top10[,-c(1,2,4,5,6)]
  Top10 <- Top10 %>%
    rename(NAME = `PLAYER NAME`)
  
  Year <- nrow (Top10)
  Top10$Year <- rep(date, time= nrow (Top10))
  return(Top10)
}

Top10.2018 <- scraping('https://www.pgatour.com/stats/stat.138.2018.html', 2018)
Top10.2017 <- scraping('https://www.pgatour.com/stats/stat.138.2017.html', 2017)
Top10.2016 <- scraping('https://www.pgatour.com/stats/stat.138.2016.html', 2016)
Top10.2015 <- scraping('https://www.pgatour.com/stats/stat.138.2015.html', 2015)
Top10.2014 <- scraping('https://www.pgatour.com/stats/stat.138.2014.html', 2014)
Top10.2013 <- scraping('https://www.pgatour.com/stats/stat.138.2013.html', 2013)
Top10.2012 <- scraping('https://www.pgatour.com/stats/stat.138.2012.html', 2012)
Top10.2011 <- scraping('https://www.pgatour.com/stats/stat.138.2011.html', 2011)
Top10.2010 <- scraping('https://www.pgatour.com/stats/stat.138.2010.html', 2010)
Top10.2009 <- scraping('https://www.pgatour.com/stats/stat.138.2009.html', 2009)
Top10.2008 <- scraping('https://www.pgatour.com/stats/stat.138.2008.html', 2008)


Top10.FINAL <- rbind(Top10.2018,Top10.2017,Top10.2016,Top10.2015,Top10.2014,Top10.2013,Top10.2012,Top10.2011,Top10.2010,Top10.2009,Top10.2008)




a<-full_join(GIR.FINAL, Putts.per.round.FINAL, Average.scoring.FINAL, Distance.from.20.30.FINAL, Driving.Accuracy.final, Driving.distance.final, Last.round.scoring.FINAL, Top10.FINAL, Victory.FINAL)
