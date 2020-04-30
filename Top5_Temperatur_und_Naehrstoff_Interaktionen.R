##########################################################################################################
## R-Studio Skript zu Aquatische Oekologie Thema 'Temperatur und Nährstoff Interaktionen '
#written by Mike Smykala
#2020.04.06
#V_1.2
##########################################################################################################
rm(list=ls(all=TRUE)) 
dev.off() 
library(tidyverse)
setwd("D:/Studium/Tutor Aquatische Ökologie/Topic6")
Data_planktodiversa <- read_delim("Data_planktodiversa.csv",";", escape_double = FALSE,trim_ws = TRUE,
                                  col_types = cols(Temperature = col_number(),TP = col_number(),
                                                   temperature = col_factor(levels = c("increase","ambient")),
                                                   nutrients = col_factor(levels = c("pulsed","decreasing")),
                                                   POC.Phyto.mol = col_number(),PON.Phyto.mol= col_number(),
                                                   POP.Phyto.mol = col_number(),Chl= col_number(),
                                                   Phyto.CP = col_number(), Phyto.CN = col_number(),
                                                   rich = col_number(),shan = col_number(), even= col_number()))
#if it works it works, but i don't know why 
##########################################################################################################
str(Data_planktodiversa)
View(Data_planktodiversa)
##########################################################################################################
data <- Data_planktodiversa %>% 
  group_by(Day, nutrients, temperature) %>% 
  summarise( mean_Temp = mean(Temperature,na.rm = T),
             mean_nutP = mean(TP,na.rm = T))
ggplot(data,aes(x=Day,y=mean_Temp,group=nutrients:temperature,col=temperature))+
  geom_line()+
  ylab('Temperatur [°C]')+
  theme_classic()+
  labs(col = "Temperatur Treatment")
ggplot(data,aes(x=Day,y=mean_nutP,group=nutrients:temperature,col=nutrients))+
  geom_line()+
  ylab(expression("Phosphat"~"["*mu*"M]"))+
  theme_classic()+
  labs(col = "Nutrient Treatment")+
  scale_color_hue(labels = c("constant","oligotrophication" ))
##########################################################################################################
#Aufgabe 1 berechne die mitlere Biomasse für die einzelnen Treatments und deren Standardabweichungen.
data <- Data_planktodiversa %>% 
  group_by(Day, nutrients, temperature) %>% 
  summarise( mean_Carbon = mean(POC.Phyto.mol,na.rm=T),
             sd_Carbon = sd(POC.Phyto.mol,na.rm=T),
             mean_Chl = mean(Chl,na.rm=T),
             sd_Chl = sd(Chl,na.rm=T),
             mean_CP = mean(Phyto.CP,na.rm=T),
             mean_CN = mean(Phyto.CN,na.rm=T),
             mean_Rich = mean(rich,na.rm=T),
             mean_Even = mean(even,na.rm=T))
##########################################################################################################
#Untersuche folgende Hyphothese:
#H1: Steigende Temperatur und  Nährstoff Reduktion  beeinflussen die Biomasseproduktion von Phytoplankton.
#Durch welchen Parameter lässt sich dies am besten veranschaulichen? 
ggplot(data,aes(x=Day,y=mean_Carbon,group=nutrients:temperature))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  geom_line(aes(linetype=nutrients:temperature),show_guide = FALSE)+
  ylab('Carbon [mM/L]')+
  theme_classic()+
  scale_shape_manual(name = 'Treatment',values =c(15, 16, 17,18),
                     labels = c('Warming','Control','Interaction','Eutrophication'))
ggplot(data,aes(x=Day,y=mean_Chl))+
  geom_point(size=3,aes(shape = nutrients:temperature))+
  scale_shape_manual(name = 'Treatment',values =c(15, 16, 17,18),
                     labels = c('Warming','Control','Interaction','Eutrophication'))+
  geom_line(aes(linetype=nutrients:temperature),show_guide = FALSE)+
  ylab('Chlorophyll  [mg/L]')+
  theme_classic()
##########################################################################################################
Meta=read.csv2('Metadata_file_planktodiversa.csv')
Meta=Meta[1:72,]
data_tidy <- gather(Meta, key = "Taxa", value = "Abundanz", -Sample.ID,-nutrients,-temperature,-Day,na.rm = T)
#Aufgabe 2 Berechne für die einzelnen Treatments die relative Abundanz und stelle diese in einer Rang Abundanz
# Grafik dar einfach logarithmiert.
RAD <- filter(data_tidy, Day == 44)
RAD <- group_by(RAD,nutrients,temperature,Taxa)
RAD <- summarise(RAD,mean_Abundanz= mean(Abundanz))
RAD <- arrange(RAD,desc(mean_Abundanz))
RAD <- mutate(RAD,rank = row_number())
RAD <- mutate(RAD,TotalAbundance = sum(mean_Abundanz),RelativeAbundance =100*mean_Abundanz/TotalAbundance,
              treatment=nutrients:temperature)#das hier ist nur um sich das Leben beim beschriften einfacher zu machen 

levels(RAD$treatment)[5]=c('Eutrophication')
levels(RAD$treatment)[6]=c('Interaction')
levels(RAD$treatment)[8]=c('Control')
levels(RAD$treatment)[9]=c('Warming')
ggplot(subset(RAD, RelativeAbundance>0), aes(x=rank, y=RelativeAbundance, group=treatment))+
  geom_point()+
  geom_text(aes(label=ifelse(RelativeAbundance>1, as.character(Taxa),'')), hjust=-0.05, vjust=0.3)+
  geom_line()+
  facet_wrap(~treatment)+
  scale_y_log10()+
  theme_light()+
  xlab('Rank')+
  ylab('Relative Abundance')
#Hypothese 2: Mit der Zeit nimmt die Biodiversität in allen Treatments ab, da diese isoliert sind.
data <- data_tidy %>% 
  group_by(Day, nutrients, temperature,Taxa) %>%
  summarise(MW_Abu = mean(Abundanz),na.rm=T)
data=ungroup(data)
data <- data %>%
  group_by(Day, nutrients, temperature) %>%
  mutate(sum_Abu=sum(MW_Abu),rel_Abu = MW_Abu/sum_Abu*100)
data = filter(data,rel_Abu>=1)
ggplot(data,aes(x=Day,y=rel_Abu,fill=Taxa))+
  geom_bar(position = 'stack',stat = 'identity')+
  facet_wrap(~nutrients~temperature) #beschriftung genau so wie bei der RAD 
##########################################################################################################


















