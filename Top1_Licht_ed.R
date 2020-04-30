## R-Studio Skript zu Aquatische Oekologie Thema 'Licht'

#edit from Mike Smykala 
################################################################################
library(tidyverse) #Datenpaket laden um die Funktionen von Tidyverse benutzen zu koennen


rm(list=ls(all=TRUE)) #Global Environment aufraeuen, geht auch ueber den Besen neben Import Dataset 
dev.off() #loescht alle vorhandenen Grafiken 
cat("\014") #loescht alle eintraege in der Konsole geht auch mit STRG + L 

## Daten einladen
setwd("D:/Studium/Tutor Aquatische Ökologie/Thema2_Licht") #Working Directory setzen 
#oder manuell ueber Session -> Set Working Directory -> Choose Directory -> Datei Speicher Ort auswaehlen 
#Zeile aus der Console kopieren und im Skript einfuegen fuer spaetere Arbeiten am Skript 

data <-read.csv2("Top1_licht.csv", sep = ';') #Datein einlesen 


str(data) #gibt die Strucktur des Datensatz 
names(data) #gibt die Namen der Spalten 


# Frage: welche Treatments gibt es? Und wie ist unser Datensatz aufgebaut? 
# (Messungen pro Zeiteinheit, was wurde gemessen)



# 1. Uebersichtsplot
ggplot(data,aes(x=Tag,y=mean_intensity,col=as.factor(Treatment_Level)))+ 
  geom_point()
#okay Licht intensitaet bleibt die Tage ueber Konstant fuer die einzelnen Treatments



#2.Fluoreszenz ueber die Intensitaet plotten
ggplot(data,aes(x=mean_intensity,y=Fluoreszenz))+
  geom_point()


ggplot(data,aes(x=Tag,y=Fluoreszenz,col=as.factor(Treatment_Level)))+
  geom_point()


##3.Uebung: berechne die mittlere Fluoreszenzkonzentration u pro Tag fuer jedes Treatment
#sowie die Standardabweichung

data_mean <- data %>% # neuen Datensatz aus altem erstellen
  group_by(Tag, Treatment_Level, mean_intensity) %>% #es geht auch nur Treatment_Level oder nur mean_intensity
  summarise(mean_fl = mean(Fluoreszenz, na.rm = T), #zusammenfassen des Datensatzes und berechnen von mean
            sd_fl = sd(Fluoreszenz, na.rm = T)) #und sd, mit na.rm = T schmeisst man die NAs raus.
  
# plotten des Ergebnisses
ggplot(data_mean, aes(x = Tag, y = mean_fl, col = as.factor(Treatment_Level)))+
    geom_point()+
    geom_errorbar(aes(ymin = mean_fl - sd_fl, ymax = mean_fl + sd_fl)) #fügt errorbars hinzu in diesem Fall in y Richtung 


#- mittlere C_mg_l pro Tag oder ueber die Zeit/mittlere Intensität als Rate
data_mean <- data %>% # neuen Datensatz aus altem erstellen
  group_by(Tag, Treatment_Level, mean_intensity) %>% 
  summarise( mean_C = mean(C_mg_l,na.rm=T))

ggplot(data_mean, aes(x=mean_intensity,y=mean_C,col = as.factor(Treatment_Level)))+
  geom_point()

##3.Uebung: berechne die C/N ratio pro Tag fuer jedes Treatment

#- C/N ratios fuer jedes Treatment an jedem Tag 
data_mean <- data %>% # neuen Datensatz aus altem erstellen
  group_by(Tag, Treatment_Level, mean_intensity) %>% 
  summarise( CN_ratio = (mean(C_mg_l,na.rm=T)/mean(N_mg_l,na.rm=T)))

ggplot(data_mean, aes(x=Tag,y=CN_ratio,col = as.factor(Treatment_Level)))+
  geom_point()
##4.Uebung  betrachte die Fluoreszenz gegenüber der mittleren Lichtintensität nur fuer Tag 10 im barplot mit geom_col(),

data10 <- data %>% filter(Tag == 10) 
data_10 <- data10 %>% # neuen Datensatz aus altem erstellen
  group_by( Treatment_Level, mean_intensity) %>% 
  summarise( mean_flu = mean(Fluoreszenz,na.rm=T))


ggplot(data_10,aes(x=mean_intensity,y=mean_flu))+
  geom_col()

##5.Uebung versehe deine Grafen mit Achsenbeschriftungen und Titel änder die Schriftgröße sowie Farben und Themes 