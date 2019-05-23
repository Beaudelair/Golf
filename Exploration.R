# In this script we will do the data exploration andd vizualisation
load("~/Golf/Golf/dbfinal1.RData")

library(hablar)

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
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Average Driving Distance")+ xlab("Weight")+
  ggtitle("Average driving distance compared to weight")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


dbfinal1 %>% 
  ggplot(aes(Age, distance)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
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
  ggplot(aes(`Age`, accuracy)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Age")+
  ggtitle("Average driving accuracy compared to age")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

library(Hmisc)




#faire un tableau avec les gagnants uniquement et renommer variables + tableau avec les 10 meilleurs joueurs au niveau des victoire total
#résoudre ce problème des dollars -done 
#sortir l'origine de chaque joueur aussi - done

#bon voila ce que tu voulais mon petit léo adoré
dbfinal1$victories <-as.numeric(dbfinal1$victories)

dbfinal2 <- dbfinal1 %>% rowwise() %>%filter(any(c(victories) %in% c(1:20)))


#group by player sum summarized win



dbfinal3 <- dbfinal1 %>%  filter(player == "Dustin Johnson" | player == "Tiger Woods" | player == "Bubba Watson"| player == "Jordan Spieth"| player == "Jason Day"| player == "Phil Mickelson"|
                                    player == "Rory Mcllroy"| player == "Justin Rose"| player == "Justin Thomas"| player == "Zach Johnson" )
