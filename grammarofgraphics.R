#### Script zum erstellen eines ggplots####

# Schritt 1: Packages laden
library(tidyverse)
library(scales)

# Schritt 2: Daten laden
syltZooplankton <- read_csv("Desktop/HIWI/AquatÖko/Daten_für_Praktikum/syltZooplankton.csv")
# Schritt 3: geladene Daten ueberpruefen
str(syltZooplankton)

# Schritt 4: Ueberlege... Was will ich darstellen?
##Erstelle einen stacked barplot, der zeigt welche Arten 
##im Mittel >10% zur Gesamtbiomasse beitragen
# berechne dazu die relative abundance pro habitat

# Schritt 5: Daten transformation
#
#gruppieren nach Datum und Habitat
data_rel<- group_by(syltZooplankton, habitat, date) 
#neue Spalte erstellen #relative Abundanz berechnen
data_rel<-  mutate(data_rel, summe = sum(abund, na.rm = T)) 
data_rel<-  mutate(data_rel,rel_abund = abund/summe ) 
#gruppieren nach Art und Habitat
data_rel <- group_by(data_rel,species, habitat)
#gemittelte Abundanz pro ARt und Habitat 
data_rel <- summarise(data_rel,mean_contribution = mean(rel_abund,na.rm = T))
#alle Arten ueber 10%
data_rel <-  filter(data_rel, mean_contribution > 0.1) 


#plot
  ggplot(data_rel, aes(x = habitat, y = mean_contribution, fill = species))+
  geom_col(col = 'black') + #geom = barplot mit schwarzer Umrandung
  scale_y_continuous(limits = c(0.0,1), breaks = seq(0.0,0.8,0.2))+ #Achsenticks einstellen
  labs(y = 'relative species contribution (over 10 %)', x = '')+ #Achsenbeschriftungen
  facet_wrap(~habitat, scales = 'free_x')+
  theme_bw()+ #veraendert ploteinstellung
    theme(legend.text = element_text(face = 'italic')) #schreibt Arten kursiv
    
#speichern des plots
getwd()
setwd()
ggsave(plot = last_plot(), file = 'rel_species.png')


##cheat sheet ggplot2
#https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf