# In this script we will do the data exploration andd vizualisation
load("~/Golf/Golf/DBfinal.RData")
install.packages("hablar")
library(hablar)

#graphique pour visualisation des datas

DBfinal %>% 
  ggplot(aes(x = NULL,y=`Average Driving Distance`))+
  ylab("Driving Distance")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Driving Distance Overview")+
  theme(plot.title = element_text(hjust = 0.5))

DBfinal %>% 
  ggplot(aes(x = NULL,y= c(as.numeric(`Average Scoring`))))+
  ylab("Average Scoring")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Average scoring per round")+
  theme(plot.title = element_text(hjust = 0.5))

DBfinal %>% 
  ggplot(aes(x = NULL,y= c(as.numeric(`Average # of putts`))))+
  ylab("Average number of Putts")+
  geom_boxplot(fill="springgreen3")+
  theme_classic()+
  scale_x_continuous(labels = NULL)+
  scale_y_continuous()+
  ggtitle("Average number of putts per round")+
  theme(plot.title = element_text(hjust = 0.5))


DBfinal %>% 
  ggplot(aes(`Size in cm`, `Average Driving Distance`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Average Driving Distance")+ xlab("Size")+
  ggtitle("Average driving distance compared to size")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

DBfinal %>% 
  ggplot(aes(`Weight`, `Average Driving Distance`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Average Driving Distance")+ xlab("Weight")+
  ggtitle("Average driving distance compared to weight")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


DBfinal %>% 
  ggplot(aes(`Age`, `Average Driving Distance`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Average Driving Distance")+ xlab("Age")+
  ggtitle("Average driving distance compared to Age")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

DBfinal %>% 
  ggplot(aes(`Size in cm`, `Driving Accuracy in %`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Size")+
  ggtitle("Average driving accuracy compared to size")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

DBfinal %>% 
  ggplot(aes(`Weight`, `Driving Accuracy in %`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Weight")+
  ggtitle("Average driving accuracy compared to weight")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

DBfinal %>% 
  ggplot(aes(`Age`, `Driving Accuracy in %`)) +
  geom_point(size=2, shape=18, color = "red") +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)+
  ylab("Driving Accuracy")+ xlab("Age")+
  ggtitle("Average driving accuracy compared to age")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

library(Hmisc)

as.numeric(DBfinal$`Driving Accuracy in %`)
as.numeric(DBfinal$`Average Driving Distance`)
as.numeric(DBfinal$Weight)

rcorr(as.matrix(DBfinal[,3:14]))


#faire un tableau avec les gagnants uniquement et renommer variables + tableau avec les 10 meilleurs joueurs au niveau des victoire total
#résoudre ce problème des dollars
#sortir l'origine de chaque joueur aussi


db2010<-DBfinal %>% filter(Year == "2010")
dbother <- DBfinal %>% filter(Year!= "2010")

db2010$`Prize money earned`<-as.numeric(gsub(",","",db2010$`Prize money earned`))
dbother$`Prize money earned` <-substring(dbother$`Prize money earned`,2)
dbother$`Prize money earned`<-as.numeric(gsub(",","",dbother$`Prize money earned`))

DB2<-rbind(db2010,dbother)

save(DB2, file = "DB2.RData")
