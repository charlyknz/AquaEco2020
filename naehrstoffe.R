
## Aquatische Oekologie

# ---------------------------- #
# Thema3_Naehrstoffe
# ---------------------------- #



#Daten loeschen, die eventuell noch in Global Environment sind
rm(list=ls(all=TRUE))     


#Datensatz naehrstoffe.csv aus Stud-IP runterladen

#Laden der benoetigten Packages
library(tidyverse)
library(scales)

#Daten einlesen
#Rechts oben Reiter 'Environment' -> 'Import Dataset' klicken
#From Text (readr) auswaehlen
#Browse -> Datensatz auswaehlen und Vorschau checken
#-> import

naehrstoffe <- read_delim("C:/Users/laura/Desktop/AquatischeÖko2020/Vorbereitung/Topic3_Nutrients/naehrstoffe.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

#Datensatz anzeigen lassen --> zeigt uns, wie R die Daten sieht
#(als numeric, character/string, integer, logical, factor, ...)
View(naehrstoffe) #oder
str(naehrstoffe)  


#Wir sehen Chlorophyllmessungen wurden 3 mal durchgefuehrt.
#Es bietet sich an, von den Messungen einen Mittelwert zu berechnen.
#Dafuer muessen die drei Spalten Chl_roh1-Chl_roh3 und ihre Werte zusammengefasst werden zu einer Spalte,
#die die Messungsnummer anzeigt (also 1-3) und zu einer Spalte, die den jeweiligen Wert (Chl_roh) dazu gibt.

#Wir nutzen gather()
data_tidy <- gather(naehrstoffe, key = "Messung", value = "Chl_roh", -Station, -Treatment, -Tag, -Replikat)
#Wir wollen eine neue Spalte 'Messungen', in die die Messnummer hinterlegt werden soll. --> key = 'Messung'
#Fuer jede Messnummer (Chl_roh1 bis Chl_roh3) woll der Chl_roh-Wert in die naechte Spalte mit Ueberschrift 'Chl__roh'--> value = 'Chl_roh'

#Mittelwert aus den 3 Chl_roh_Messungen berechnen
data_tidy <- data_tidy %>%
  group_by(Station, Tag, Treatment, Replikat) %>% #Bevor wir den Mittelwert der drei Messungen ausrechnen,
                                                  #muessen wir den Datensatz gruppieren
  summarize(Chl_roh_mean=mean(Chl_roh))

#Nun haben wir den Mittelwert der drei Chl_roh-Messungen.
#Um die Chl-Konzentration zu bekommen, muessen wir diesen *0.0631 rechnen.
data_tidy <- mutate(data_tidy, Chl_conc = Chl_roh_mean*0.0631)



####

#Frage im Skript:
#Welche Naehrstoffe/Kombinationen aus Naehrstoffen limitiert das Wachstum der natuerlichen Gemeinschaft?

#Also Frage: Was wollen wir auf der x und y Achse unseres Plots?
#auf x-Achse: Naehrstoffe/Kombinationen
#auf y-Achse: Wachstum der Gemeinschaft

#Zeitpunkt aussuchen --> Frage: Wachstum an welchem Tag betrachten?
#am Ende des Experiments, also Tag 7

#Auftrag: Erstelle Plot, in dem mittleres Wachstum pro Treatment als Punkt mit
#Standardabweichung eingezeichnet ist, ueber die Naehrstoffe/Kombinationen

tag7 <- data_tidy %>%
  filter(Tag == '7') %>%            #filtern, sodass nur Tag 7 bleibt
  group_by(Station, Treatment) %>%  #gruppieren nach Station und Treatment fuer naechste Zeile
  summarise(Chl_conc_mean = mean(Chl_conc),  #Mittelwert ausrechnen
            Chl_conc_sd = sd(Chl_conc))
  
#Wir ordnen dem Plot einen Namen zu. Der Plot wird erst ausgegeben, wenn wir den Namen aufrufen/ausfuehren
#Das ist praktisch zum Speichern spaeter.
Frage1 <- ggplot(tag7, aes(x=Treatment, y=Chl_conc_mean, col=as.factor(Station)))+
                 geom_point(size=3)+
                 geom_errorbar(aes(ymin = Chl_conc_mean - Chl_conc_sd, ymax = Chl_conc_mean + Chl_conc_sd))+ #fuegt errorbars hinzu
                 theme_bw()+
                 scale_colour_manual(name="Station", values = c("2"="blue", "4"="black", "6"="green"))+   #Legende
                 xlab('Naehrstoffe')+
                 ylab('Chlorophyll concentration')+
                 ggtitle('Effekt Naehrstofftreatments auf Chl conc')+
                 theme(plot.title = element_text(color="black", size=11, face="bold"),              #Achsenbeschriftung anpassen
                       axis.title.x = element_text(size=11, face="italic"),
                       axis.title.y = element_text(size=11, face="italic"))    #kursiv=italic

Frage1 #Plot anzeigen



#oder Boxplot ueber alle Stationen
ggplot(tag7, aes(x=Treatment, y=Chl_conc, group=Treatment))+
  geom_boxplot()+
  theme_classic()+
  xlab('Naehrstoffe')+
  ylab('Chlorophyll concentration')+
  ggtitle('Effekt Naehrstofftreatments auf Chl conc')




#Frage 2 im Skript:
#Wie unterscheidet sich das Wachstum über die Naehrstoffe and den verschiedenen Standorten (=Stationen)?

#Also Frage: Was wollen wir darstellen?
#verschiedene facets fuer Stationen

#Wir ordnen dem Plot einen Namen zu. Der Plot wird erst ausgegeben, wenn wir den Namen aufrufen/ausfuehren
#Das ist praktisch zum Speichern spaeter.
Frage2 <- ggplot(tag7, aes(x=Treatment, y= Chl_conc_mean, group=Station, color = as.factor(Station)))+
                  geom_point()+
                  geom_jitter()+
                  facet_wrap(~Station, ncol=1)+
                  geom_errorbar(aes(ymin = Chl_conc_mean - Chl_conc_sd, ymax = Chl_conc_mean + Chl_conc_sd))+ #fuegt errorbars hinzu
                  scale_color_manual(values = c('coral2', 'skyblue', 'darkgreen'),
                                     labels = c(' Station 2', 'Station 4', 'Station 6'))+
                  labs(y = 'mittlere Chlorophyll Konzentration', col = 'Station')+
                  theme_bw()

Frage2





#speichern der Plots
getwd()
setwd('C:/Users/laura/Desktop/AquatischeÖko2020/Thema3_Naehrstoffe')
ggsave(plot = Frage1, file = 'Frage1.png', width = 15, height = 10, units = c("cm"))
ggsave(plot = Frage2, file = 'Frage2.png', width = 10, height = 10, units = c("cm"))
