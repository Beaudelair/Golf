

#les modèles de léo

# In this script we will do the data exploration andd vizualisation

library(boot)
library(tidyverse)
library(caret)
library(neuralnet)
library(MASS)
library(plotly)
library(dplyr)
library(randomForest)
library(hablar)
library(class)
library(FactoInvestigate)
library(factoextra)
library(rnaturalearth)
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

install.packages("FactorMineR")

#Create dummy for victory 

load("~/Golf/Golf/dbfinal1.RData")



#stepwise selection logistic 

# Split the data into training and test set
dbfinal2 <- na.omit(dbfinal1)

set.seed(123)
training.samples <- dbfinal2$lag_vic %>% 
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- dbfinal2[training.samples, ]
test.data <- dbfinal2[-training.samples, ]


full.model <- glm(lag_vic ~ money + putts + distance + score+victories+ cuts+rounds+approach+size+age, data = train.data, family = binomial) %>% 
  stepAIC(trace = FALSE)
# Summarize the final selected model
summary(full.model)
# Make predictions
probabilities <- full.model %>% predict(train.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
# Model accuracy
mean(as.numeric(predicted.classes)==train.data$lag_vic)

cm<-confusionMatrix(data=as.factor(predicted.classes), 
                    reference=train.data$lag_vic)
cm$table



set.seed(123)


fit <- randomForest(as.factor(lag_vic) ~  money + putts + distance + score+victories+ cuts+rounds+approach+size+age,
                    mtry = 2,
                    data = train.data, na.action = na.roughfix)
varImpPlot(fit)


# Make predictions
probabilities <- fit %>% predict(test.data)

# Model accuracy
mean(as.numeric(predicted.classes)==train.data$lag_vic)

cm<-confusionMatrix(data=probabilities, 
                    reference=test.data$lag_vic)

library(class)
cl = train.data['lag_vic', drop = TRUE]
pr <- knn(lag_vic ~ money,train = train.data, test = test.data, cl, k=3)



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(lag_vic ~ money + score + victories + cuts + rounds + size + age, data = train.data, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
test_pred <- predict(knn_fit, newdata = test.data)

cm<-confusionMatrix(data=test_pred, 
                    reference=test.data$lag_vic)
cm$table




dbfinal2$lag_vic1<-as.numeric(as.character(dbfinal2$lag_vic))
dbfinal2$lag_vic <-as.factor(as.character(dbfinal2$lag_vic))
mtcars.pca <- Investigate(dbfinal1[res,c('score','money', 'victories', 'cuts','rounds', 'size', 'age', 'distance', 'lag_vic1', 'accuracy')],scale.unit=TRUE, ncp=5, graph=T)

typeof(dbfinal1[,20])
plot.PCA(mtcars.pca, choix="var")






##create the confucion matrix
tb <- table(pr,test_target)
nrow(train.data)

# function to obtain R-Squared from the data
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit<-glm(formula, data = d,family="binomial")
  return(coef(fit))
}

results <- boot(data=right, statistic=rsq,
                R=10000, formula = lag_vic~ money + putts + distance + score)

boot.ci(results, type="norm", index=5) # intercept
plot(results) # intercept

right$`TOP 10`
neuralnet((lag_vic == "1") ~ money + putts + distance, data = right, hidden = 1, hidden = c(3, 2), act.fct = softplus)


a <- dbfinal1 %>% mutate(dummy = Hmisc::cut2(dbfinal1$victories, c(0,1))) 
levels(a$dummy)[levels(a$dummy)=="0"] <- "No victory"
levels(a$dummy)[levels(a$dummy)=="1"] <- "victory"
levels(a$dummy)[levels(a$dummy)=="[1,6]"] <- "victory"
levels(a$dummy)

dbfinal1 <-a %>% group_by(player) %>%  mutate(lag_vic = lag(dummy, order_by = year))

arrange(right, year)

a<-glm(lag_vic~ money, data = right,family="binomial")
b <-summary(a)
b$coefficients



#graphique pour visualisation des datas

dbfinal1 %>% 
  ggplot(aes(x = NULL,y=distance))+
  ylab("Driving Distance")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Driving Distance Overview")+
  theme(plot.title = element_text(hjust = 0.5))

dbfinal1 %>% 
  ggplot(aes(x = NULL,y= c(as.numeric(score))))+
  ylab("Average Scoring")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Average scoring per round")+
  theme(plot.title = element_text(hjust = 0.5))

dbfinal1 %>% 
  ggplot(aes(x = NULL,y= c(as.numeric(putts))))+
  ylab("Average number of Putts")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Average number of putts per round")+
  theme(plot.title = element_text(hjust = 0.5))


dbfinal1 %>% 
  ggplot(aes(size,distance)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Average Driving Distance")+ xlab("Size")+
  ggtitle("Average driving distance compared to size")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

dbfinal1 %>% 
  ggplot(aes(weight, distance)) +
  geom_point(size=1, shape=1, color = "darkgreen")  +
  geom_smooth(method="loess",size=1.5,se=TRUE, fullrange=TRUE, level=0.95, color = "cornsilk3")+
  ylab("Average Driving Distance")+ xlab("Weight")+
  ggtitle("Average driving distance compared to weight")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

dbfinal1 %>% 
  ggplot(aes(age, distance))+
  geom_point(size=1, shape=1, color = "darkgreen")  +
  geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95,color = "cornsilk3")+
  ylab("Average Driving Distance")+ xlab("Age")+
  ggtitle("Average driving distance compared to Age")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

dbfinal1 %>% 
  ggplot(aes(size, accuracy)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Size")+
  ggtitle("Average driving accuracy compared to size")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

dbfinal1 %>% 
  ggplot(aes(weight, accuracy)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Weight")+
  ggtitle("Average driving accuracy compared to weight")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

dbfinal1 %>% 
  ggplot(aes(`age`, accuracy)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Age")+
  ggtitle("Average driving accuracy compared to age")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()




#faire un tableau avec les gagnants uniquement et renommer variables + tableau avec les 10 meilleurs joueurs au niveau des victoire total
#résoudre ce problème des dollars -done 
#sortir l'origine de chaque joueur aussi - done

#bon voila ce que tu voulais mon petit léo adoré


dbfinal2 <- dbfinal1 %>% rowwise() %>%filter(any(c(victories) %in% c(1:20)))


#group by player sum summarized win



dbfinal3 <- dbfinal1 %>%  filter(player == "Dustin Johnson" | player == "Tiger Woods" | player == "Bubba Watson"| player == "Jordan Spieth"| player == "Jason Day"| player == "Phil Mickelson"|
                                   player == "Rory Mcllroy"| player == "Justin Rose"| player == "Justin Thomas"| player == "Zach Johnson" ) %>% 
  mutate(year = as.Date(paste(as.character(substr(year, 1, 4)), 01, 01, sep = "-")))

all_year<- data.frame("player"=(rep(unique(dbfinal3$player),each=11)), "year"= (rep(2008:2018, 9))) %>% mutate(year = as.Date(paste(year, 01, 01, sep = "-")))

all_year<-full_join(dbfinal3,all_year, by = c("year" = "year", "player" = "player")) %>% mutate(player = as.factor(player))
all_year[is.na(all_year)] <- 0


all_year %>% ggplot(aes(year,victories, col= player ))+
  geom_line(size=0.6, alpha = 0.4)+
  theme_bw()+
  geom_point(size=2.5)+
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "red", "blue","yellow"))


all_year %>% group_by(player) %>% arrange(year) %>% mutate(Total_victory = cumsum(victories)) %>% unique() %>% 
  ggplot(aes(year,Total_victory, col= player ))+
  geom_line(size=0.6, alpha = 0.4)+
  theme_bw()+
  geom_point(size=2.5)+
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "red", "blue","yellow"))


all_year %>% group_by(player) %>% 
  arrange(year) %>% mutate(Total_victory = cumsum(victories)) %>% 
  unique() %>% 
  ggplot(aes(x=year, y=Total_victory, fill=player)) +
  geom_area(colour="black", size=.3, alpha=.7) +
  scale_fill_brewer(palette="Greens")+
  theme_bw()

#2x Zach Johnson !!!!!!!!!!!!!!!!!!!


#compare sum victories top10 and other 
top_10 <- dbfinal1 %>%  filter(player == "Dustin Johnson" | player == "Tiger Woods" | player == "Bubba Watson"| player == "Jordan Spieth"| player == "Jason Day"| player == "Phil Mickelson"|
                                 player == "Rory Mcllroy"| player == "Justin Rose"| player == "Justin Thomas"| player == "Zach Johnson" ) %>% 
  mutate(year = as.Date(paste(as.character(substr(year, 1, 4)), 01, 01, sep = "-"))) %>% group_by(year) %>% summarise(Victories_others= sum(victories, na.rm = TRUE))

others<- dbfinal1 %>%  filter(player != "Dustin Johnson" | player != "Tiger Woods" | player != "Bubba Watson"| player  !=  "Jordan Spieth"| player  != "Jason Day"| player  !=  "Phil Mickelson"|
                                player  !=  "Rory Mcllroy"| player  !=  "Justin Rose"| player  != "Justin Thomas"| player  !=  "Zach Johnson" ) %>% 
  mutate(year = as.Date(paste(as.character(substr(year, 1, 4)), 01, 01, sep = "-"))) %>% group_by(year) %>% summarise(Victories_others= sum(victories, na.rm = TRUE))






Freq_country <- dbfinal1 %>% select(country,player) %>%
  unique() %>% group_by(country) %>%
  summarize(`Number of player` = length(country)) %>% na.omit() %>% 
  mutate("freq" = (`Number of player`/ sum(`Number of player`))) %>% 
  mutate(country=replace(country, country=="RSA", "ZAF")) %>% 
  mutate(country=replace(country, country=="ZIM", "ZWE")) %>% 
  mutate(country=replace(country, country=="ENG", "GBR")) %>% 
  mutate(country=replace(country, country=="SCO", "GBR")) %>% 
  mutate(country=replace(country, country=="WAL", "GBR")) %>% 
  mutate(country=replace(country, country=="GER", "DEU")) %>% 
  mutate(country=replace(country, country=="DEN", "DNK")) %>% 
  mutate(country=replace(country, country=="FIJ", "FJI"))


library(countrycode)

library(sf)


library("rnaturalearth")
library("rnaturalearthdata")

WorldData <- map_data('world') 
WorldData$region<- countrycode(WorldData$region,'country.name', 'iso3c' )
joint<-full_join(Freq_country,WorldData, by = c("country"="region"))


ggplot(joint, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = `Number of player`))+
  scale_fill_viridis_c(option = "cividis", trans = "log2",name = waiver())+
  ggtitle("World map temperature of march 2010")+
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw()















#model uniweight, first before data exploration to binarize all the variables

#dbfinal1$dummy<- as.numeric(dbfinal2$dummy)


dbfinal1%>% 
  ggplot(aes(distance, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal1 %>%
  ungroup()  %>%
  mutate(distance_bin = Hmisc::cut2(dbfinal1$distance, 267)) 

levels(dbfinal2$distance_bin)
levels(dbfinal2$distance_bin)[levels(dbfinal2$distance_bin)=="[237,267)"] <- "0"
levels(dbfinal2$distance_bin)[levels(dbfinal2$distance_bin)=="[267,292]"] <- "1"
levels(dbfinal2$distance_bin)



#compare accuracy
dbfinal2 %>% 
  ggplot(aes(accuracy, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

#not enough difference
#for the putts now

dbfinal2%>% 
  ggplot(aes(putts, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal2 %>% 
  ungroup()  %>% 
  mutate(putts_bin = Hmisc::cut2(dbfinal2$putts, 28.8)) 

levels(dbfinal2$putts_bin)[levels(dbfinal2$putts_bin)=="[28.8,31.0]"] <- "0"
levels(dbfinal2$putts_bin)[levels(dbfinal2$putts_bin)=="[27.5,28.8)"] <- "1"
levels(dbfinal2$putts_bin)


#score
dbfinal2 %>% 
  ggplot(aes(score, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal2 %>% 
  ungroup()  %>% 
  mutate(score_bin = Hmisc::cut2(dbfinal2$score, 70.7)) 

levels(dbfinal2$score_bin)[levels(dbfinal2$score_bin)=="[70.7,74.4]"] <- "0"
levels(dbfinal2$score_bin)[levels(dbfinal2$score_bin)=="[68.1,70.7)"] <- "1"
levels(dbfinal2$score_bin)


#lscore
dbfinal2 %>% 
  ggplot(aes(lscore, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()


dbfinal2 <- dbfinal2 %>% 
  ungroup()  %>% 
  mutate(lscore_bin = Hmisc::cut2(dbfinal2$lscore, 70.7)) 

levels(dbfinal2$lscore_bin)[levels(dbfinal2$lscore_bin)=="[70.7,76.5]"] <- "0"
levels(dbfinal2$lscore_bin)[levels(dbfinal2$lscore_bin)=="[67.7,70.7)"] <- "1"
levels(dbfinal2$lscore_bin)

#money
dbfinal2 %>% 
  ggplot(aes(money, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal2 %>% 
  ungroup()  %>% 
  mutate(money_bin = Hmisc::cut2(dbfinal2$money, 1750000)) 
levels(dbfinal2$money_bin)
levels(dbfinal2$money_bin)[levels(dbfinal2$money_bin)=="[   24650, 1750000)"] <- "0"
levels(dbfinal2$money_bin)[levels(dbfinal2$money_bin)=="[ 1750000,12030465]"] <- "1"
levels(dbfinal2$money_bin)

#
dbfinal2 %>% 
  ggplot(aes(cuts, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal2 %>% 
  ungroup()  %>% 
  mutate(cuts_bin = Hmisc::cut2(dbfinal2$cuts, 8)) 

levels(dbfinal2$cuts_bin)[levels(dbfinal2$cuts_bin)=="[ 2, 8)"] <- "0"
levels(dbfinal2$cuts_bin)[levels(dbfinal2$cuts_bin)=="[ 8,43]"] <- "1"
levels(dbfinal2$cuts_bin)



#up

dbfinal2 %>% 
  ggplot(aes(victories, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()

dbfinal2 <- dbfinal2 %>%
  ungroup()  %>% 
  mutate(victories_bin = Hmisc::cut2(dbfinal2$victories, 1)) 

levels(dbfinal2$victories_bin)[levels(dbfinal2$victories_bin)=="0"] <- "0"
levels(dbfinal2$victories_bin)[levels(dbfinal2$victories_bin)=="[1,6]"] <- "1"
levels(dbfinal2$victories_bin)

#fitted value
dbfinal2$distance_bin <- as.numeric(as.character(dbfinal2$distance_bin))
dbfinal2$cuts_bin <-as.numeric(as.character(dbfinal2$cuts_bin))
dbfinal2$putts_bin <-as.numeric(as.character(dbfinal2$putts_bin))
dbfinal2$score_bin <-as.numeric(as.character(dbfinal2$score_bin))
dbfinal2$lscore_bin <-as.numeric(as.character(dbfinal2$lscore_bin))
dbfinal2$money_bin <-as.numeric(as.character(dbfinal2$money_bin))
dbfinal2$victories_bin <-as.numeric(as.character(dbfinal2$victories_bin))

dbfinal3<-dbfinal2[-which(is.na(dbfinal2$lag_vic)),]
dbfinal3[ "cuts_bin"][is.na(dbfinal3["cuts_bin"])] <- 0
dbfinal3["lscore_bin"][is.na(dbfinal3["lscore_bin"])] <- 0




dbfinal2 <- dbfinal3 %>% mutate("fitted" = 2*distance_bin+cuts_bin+putts_bin+4*score_bin+2*lscore_bin+4*money_bin+2*victories_bin)


dbfinal2 %>% 
  ggplot(aes(fitted, fill=dummy)) +
  geom_density(alpha=0.4)+
  theme_bw()



predicted.classes <- ifelse(dbfinal2$fitted >= 16, "1", "0")



library(e1071)
library(caret)
cm<-confusionMatrix(data=as.factor(predicted.classes), 
                    reference=as.factor(dbfinal2$lag_vic))

cm

