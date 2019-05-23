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
library(tidyverse)
library(knitr)


#première variable concernant la longueur du premier coup par joueur

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  Driving.Distance <-as_tibble(tbls_ls[[1]])
  
  Driving.Distance <- Driving.Distance[,-c(1,2,4,6,7)]
  Driving.Distance <- Driving.Distance %>% 
    rename(NAME = `PLAYER NAME`, 
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
  
  Distance.from.20.30 <- Distance.from.20.30[,-c(1,2,4,6,7,8)]
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
  
  Average.scoring <- Average.scoring[,-c(1,2,6,7,8)]
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
  
  Top10 <- Top10[,-c(1,2,4,6,7,8)]
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




#variable sur les points, qui nous permet de voir s'il a fait des bons scores dans les tournois importants

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Money <-as_tibble(tbls_ls[[1]])
  
  Money <- Money[,-c(1,2,4,6)]
  Money <- Money %>%
    rename(NAME = `PLAYER NAME`)
  
  Year <- nrow (Money)
  Money$Year <- rep(date, time= nrow (Money))
  return(Money)
}

Money.2018 <- scraping('https://www.pgatour.com/stats/stat.109.2018.html', 2018)
Money.2017 <- scraping('https://www.pgatour.com/stats/stat.109.2017.html', 2017)
Money.2016 <- scraping('https://www.pgatour.com/stats/stat.109.2016.html', 2016)
Money.2015 <- scraping('https://www.pgatour.com/stats/stat.109.2015.html', 2015)
Money.2014 <- scraping('https://www.pgatour.com/stats/stat.109.2014.html', 2014)
Money.2013 <- scraping('https://www.pgatour.com/stats/stat.109.2013.html', 2013)
Money.2012 <- scraping('https://www.pgatour.com/stats/stat.109.2012.html', 2012)
Money.2011 <- scraping('https://www.pgatour.com/stats/stat.109.2011.html', 2011)
Money.2010 <- scraping('https://www.pgatour.com/stats/stat.109.2010.html', 2010)
Money.2009 <- scraping('https://www.pgatour.com/stats/stat.109.2009.html', 2009)
Money.2008 <- scraping('https://www.pgatour.com/stats/stat.109.2008.html', 2008)


Money.FINAL <- rbind(Money.2018,Money.2017,Money.2016,Money.2015,Money.2014,Money.2013,Money.2012,Money.2011,Money.2010,Money.2009,Money.2008)

#variable qui explique si le joueur est consistant ou non. C'est à dire s'il est capable de performer plusieurs tournois de suite en finissant dans les 70 meilleurs après deuc jours

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  
  Cuts <-as_tibble(tbls_ls[[1]])
  
  Cuts <- Cuts[,-c(1,2,5)]
  Cuts <- Cuts %>%
    rename(NAME = `PLAYER NAME`)
  
  Year <- nrow (Cuts)
  Cuts$Year <- rep(date, time= nrow (Cuts))
  return(Cuts)
}

Cuts.2018 <- scraping('https://www.pgatour.com/stats/stat.122.2018.html', 2018)
Cuts.2017 <- scraping('https://www.pgatour.com/stats/stat.122.2017.html', 2017)
Cuts.2016 <- scraping('https://www.pgatour.com/stats/stat.122.2016.html', 2016)
Cuts.2015 <- scraping('https://www.pgatour.com/stats/stat.122.2015.html', 2015)
Cuts.2014 <- scraping('https://www.pgatour.com/stats/stat.122.2014.html', 2014)
Cuts.2013 <- scraping('https://www.pgatour.com/stats/stat.122.2013.html', 2013)
Cuts.2012 <- scraping('https://www.pgatour.com/stats/stat.122.2012.html', 2012)
Cuts.2011 <- scraping('https://www.pgatour.com/stats/stat.122.2011.html', 2011)
Cuts.2010 <- scraping('https://www.pgatour.com/stats/stat.122.2010.html', 2010)
Cuts.2009 <- scraping('https://www.pgatour.com/stats/stat.122.2009.html', 2009)
Cuts.2008 <- scraping('https://www.pgatour.com/stats/stat.122.2008.html', 2008)


Cuts.FINAL <- rbind(Cuts.2018,Cuts.2017,Cuts.2016,Cuts.2015,Cuts.2014,Cuts.2013,Cuts.2012,Cuts.2011,Cuts.2010,Cuts.2009,Cuts.2008)


#extraction des données personnelles, concernant taille et tout ça

scraping <- function(link, date){
  webpage <- link %>% read_html()
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  
  Measurements <-as_tibble(tbls_ls[[1]])
  
  colnames(Measurements) <- as.character(unlist(Measurements[1,]))
  Measurements = Measurements[-1, ]
  
  return(Measurements)
}


Measurements.A <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/A_players.html')
Measurements.B <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/B_players.html')
Measurements.C <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/C_players.html')
Measurements.D <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/D_players.html')
Measurements.E <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/E_players.html')
Measurements.F <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/F_players.html')
Measurements.G <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/G_players.html')
Measurements.H <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/H_players.html')
Measurements.I <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/I_players.html')
Measurements.J <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/J_players.html')
Measurements.K <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/K_players.html')
Measurements.L <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/L_players.html')
Measurements.M <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/M_players.html')
Measurements.N <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/N_players.html')
Measurements.O <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/O_players.html')
Measurements.P <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/P_players.html')
Measurements.Q <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/Q_players.html')
Measurements.R <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/R_players.html')
Measurements.S <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/S_players.html')
Measurements.T <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/T_players.html')
Measurements.U <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/U_players.html')
Measurements.V <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/V_players.html')
Measurements.W <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/W_players.html')
Measurements.X <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/X_players.html')
Measurements.Y <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/Y_players.html')
Measurements.Z <- scraping('http://newsday.sportsdirectinc.com/golf/pga-players.aspx?page=/data/pga/players/Z_players.html')

Measure.final <- rbind(Measurements.A,Measurements.B,Measurements.C,Measurements.D,Measurements.E,Measurements.F,Measurements.G,Measurements.H,Measurements.I,Measurements.J,Measurements.K,Measurements.L,Measurements.M,Measurements.N,Measurements.O,Measurements.P,Measurements.Q,Measurements.R,Measurements.S,Measurements.T,Measurements.U,Measurements.V,Measurements.W,Measurements.X,Measurements.Y,Measurements.Z)

#Création de la première base de donnée
DB <-list(GIR.FINAL, Putts.per.round.FINAL, Average.scoring.FINAL, Distance.from.20.30.FINAL, Driving.Accuracy.final, Driving.distance.final, Last.round.scoring.FINAL, Top10.FINAL, Victory.FINAL, Money.FINAL,Cuts.FINAL)%>%
  reduce(left_join, by = c("NAME" = "NAME","Year" = "Year")) %>% as.tibble()

#Les noms ne sont pas  de la même façcon que dans les autres colonnes donc il faut changer ça


Measure.final <- Measure.final %>% 
  mutate(Players = sapply(strsplit(Players,","), function(x) rev(x) %>% paste0(collapse = " ")))%>% select("Players", "Height", "Weight", "DOB") %>% mutate(Players = map_chr(Players, str_squish)) 






#création des bases de données finales avec les jointures



DB1 <- left_join(DB, Measure.final, by = c("NAME" = "Players"))
library(measurements)
DB1 <- DB1 %>% filter(NAME != "Richard Johnson" & NAME != "Richard Lee")


#occupons nous des unités mtnt

DB1$`Driving Distance` <- round(conv_unit(DB1$`Driving Distance`,"yd","m"))

DB1 <- DB1 %>% separate(`Distance Left`, into = c("feet", "inch"))
#on rend numerique et on convertit les feets
DB1$feet <- as.numeric(DB1$feet)
DB1$feet <- conv_unit(DB1$feet,"ft","cm")

#pareil pour les inchs

DB1$inch <- as.numeric(DB1$inch)
DB1$inch <- conv_unit(DB1$inch,"inch","cm")
DB1$inch <- DB1$inch + DB1$feet

#Tout a été réuni reste plus qu'à supprimer db1$feet et renommer l'autre, je sais c'est pas élégant désolé Léo
#Ensuite faut faire la même pour height

DB1 <- DB1 %>% separate(`Height`, into = c("feet1", "inch1"))
#on rend numerique et on convertit les feets

DB1$feet1 <- as.numeric(DB1$feet1)
DB1$feet1 <- conv_unit(DB1$feet1,"ft","cm")

#pareil pour les inchs

DB1$inch1 <- as.numeric(DB1$inch1)
DB1$inch1 <- conv_unit(DB1$inch1,"inch","cm")
DB1$inch1 <- DB1$inch1 + DB1$feet1

#pas oublier de recopier la colonne et supprimer l'autre



DB1 <- DB1 %>% separate(`Weight`, into = c("Weight", "units"))
DB1$Weight <- as.numeric(DB1$Weight)
DB1$Weight <- conv_unit(DB1$Weight,"lbs","kg")

#mainteant les dates
DB1 <- DB1 %>% separate(`DOB`, into = c("Month", "Day", "Year1"), sep = "/")
DB1$Year1 <-as.numeric(DB1$Year1) +1900

DB1 <- DB1 %>%  mutate(Age = Year-Year1 )

#Refaire la database
library(readxl)
clean <- as.tibble(read_excel("clean2.xlsx"))
colnames(clean) <- clean[1, ]
clean <- clean[-1,]
clean1 <- clean %>% select(NAME,COUNTRY) %>% unique()

#clean2 <- clean1 %>% group_by(NAME) %>% summarise("NUmber"=count(NAME))
#kable(table(clean1$NAME))

DB1 <- left_join(DB1,clean1, by = c("NAME"="NAME"))


DBfinal <- DB1 %>% select("NAME", "Year","Age","inch1","Weight","COUNTRY","ROUNDS","Driving Distance","Accuracy in %", "Green in regulation %","inch","Average # of putts","TOTAL","Average Scoring","Average last round Scoring","MONEY","TOP 10","# of Victories")


DBfinal <- DBfinal %>%
  rename(
    "player" = NAME,
    "size" = inch1 ,
    "rounds" = ROUNDS ,
    "distance" = `Driving Distance`,
    "accuracy" =  `Accuracy in %`,
    "approach" = inch,
    "cuts" = TOTAL,
    "money" = MONEY,
    "country"= COUNTRY,
    "victories"= `# of Victories`,
    "weight" = Weight,
    "gir" = `Green in regulation %`,
    "putts"=`Average # of putts`,
    "score"= `Average Scoring`,
    "lscore"= `Average last round Scoring`,
    "year" = Year,
    "age" = Age
    )

#Victories = number of victories
#Money = Prize money earned
#Cuts = consecutive cuts made
#approach = Average distance left to the hole from a 20-30 meters shot
#accuracy = Driving Accuracy in % 
#distance = Average Driving Distance
#rounds = Round played this year
##"gir" = `Green in regulation %`,
##"putts"=`Average # of putts`,
##"score"= `Average Scoring`,
#"lscore"= `Average last round Scoring`

db2010<-DBfinal %>% filter(year == "2010")
dbother <- DBfinal %>% filter(year!= "2010")

db2010$money<-as.numeric(gsub(",","",db2010$money))
dbother$money<-substring(dbother$money,2)
dbother$money<-as.numeric(gsub(",","",dbother$money))

dbfinal1<-rbind(db2010,dbother)
dbfinal1$victories <-as.numeric(dbfinal1$victories)
dbfinal1$victories <- dbfinal1$victories %>% replace_na(0)




dbfinal1$age <- as.numeric(dbfinal1$age)
dbfinal1$size <- as.numeric(dbfinal1$size)
dbfinal1$weight <- as.numeric(dbfinal1$weight)
dbfinal1$distance <- as.numeric(dbfinal1$distance)

dbfinal1$year <-as.Date(paste(dbfinal1$year, "12", "31", sep = "-"))

dbfinal1$year <-format(as.Date(dbfinal1$year), "%d/%m/%Y")

typeof(dbfinal1$year)



save(dbfinal1, file = "dbfinal1.RData")





#A partir de maintenand on utilise db final pour tout il faut juste la lancer avec le code ci dessous

load("~/Golf/Golf/dbfinal1.RData")

