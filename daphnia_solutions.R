######################### daphnien versuch ###########################
#Topic 4: Zooplankton

# 1. Packages laden
library(tidyverse)

##2. Daten einlesen -> Vorsicht hier ist das Dezimalzeichen ein Komma!
Top3_data_daphnia <- read_delim("~/Desktop/HIWI/AquatÖko/Daten_für_Praktikum/Topic3_Daphnien_DOM/Top3_data_daphnia.csv", 
                               ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                               trim_ws = TRUE)
#3. Daten anschauen
str(Top3_data_daphnia) 


######################### Rechenaufgaben #########################
#Aufgabe 1: Berechne das Trockengewicht pro daphnia
data <- mutate(Top3_data_daphnia, dryweight_per_daphnia = total_dry_weight/anzahl_daphnia)

# Aufgabe 2: Erstelle einen Datensatz, der nur den letzten Tag enthaelt
# dieser soll Grundlage fuer folgende Aufgaben sein: 

data8 <- filter(data, sampling == 8)


#Frage 1: 
#-Welchen Einfluss hat DOM auf das Trockengewicht von Daphnia magna? 
# Auftrag: erstelle einen facet-geteilten boxplot nach strain
# Zusatz: faerbe die boxplots nach den treatments


##zum benennen des strip textes
label_box <- c(BE = 'Belgien', DE = 'Deutschland', FI = 'Finnland', NO = 'Norwegen')

ggplot(data8, aes(x = treatment, y = dryweight_per_daphnia, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~strain, labeller = labeller(strain = label_box), scales = 'free_x')

#DOM Zugabe wirkt sich positiv auf die Biomasse aus
#--> Nahrungsverfuegbarkeit




#Frage 2:
#-Welche Rolle spielt die Herkunft der Daphnia magna fuer DOM-Toleranz? 
# Auftrag: erstelle einen barplot mit einem facet fuer jedes treatment

## Berechne dafuer den Mittelwert und die Standardabweichung 
#fuer das Trockengewicht der 5 Replikate pro Sampling und Treatment 
## Tipp: benutze summarise()

data_box <- group_by(data8, sampling, treatment, strain)
data_box <- summarise(data_box, mean_weight = mean(dryweight_per_daphnia, na.rm = T),
                  sd_weight = sd(dryweight_per_daphnia, na.rm = T)) 

################################## add daphnia image to a plot ##############################################
library(ggimage)
library(ggdark)
img <- list.files('~/Desktop/pics/',
                  pattern="png", full.names=TRUE)
imageTbl <- tibble(img = img,
                   treatment = c('O', 'H', 'I', 'L'))

daphnia <- data8%>% 
  filter(strain == 'NO') %>% 
  mutate(dw = total_dry_weight/anzahl_daphnia,
         img = img[2]) %>%
  group_by(treatment) %>%
  mutate(mean_weight = mean(dw, na.rm = T))%>%
  left_join(imageTbl, by = 'treatment')
daphnia$treatment = factor(daphnia$treatment, levels = c('O', 'L', 'I', 'H'))

daphniaplot <- ggplot(daphnia, aes(x = treatment, y = mean_weight))+
  geom_image(aes(image = img.y), size = 0.15)+
  scale_y_continuous(limits = c(0.02, 0.25), breaks = seq(0,0.2,0.1))+
  scale_x_discrete(limit = c('O', 'L', 'I', 'H'), labels = c('control', 'low', 'intermediate', 'high'))+
  labs(x = 'DOM addition', y = 'mean dry weight/daphnia')+
  theme( panel.background = element_rect(fill = NA), #loescht den Hintergrund meines Plots/ fuellt ihn mit nichts
         panel.grid.major.y = element_line(color='grey', linetype = 'dashed', size=0.2),
         panel.border= element_rect(colour = "black", fill=NA, size=0.5),
         strip.background = element_rect(color = 'black', fill = 'white'),
         legend.background = element_blank(),
         legend.title = element_text(hjust=3), #schiebt text nach links in der Legende
         legend.position  ='bottom',
         legend.key = element_blank(),
         text = element_text(size=15),
         axis.text = element_text(size = 14))
 
daphniaplot
setwd("~/Desktop/HIWI/AquatÖko")
ggsave(plot = daphniaplot, file = 'daphnia_plot.png')

#######################################################################################################################


##zum benennen des strip textes
label_bar <- c(H = 'High', L = 'Low', I = 'Intermediate', O = 'Control')

## !!!!Hinweis: die Daten muessen mit summarise() zusammengefasst werden, 
## sonst addiert R die Werte auf und die errorbars und means passen nicht.
ggplot(data_box, aes(x = strain, y = mean_weight, fill = strain))+
  geom_col()+
  geom_errorbar(aes(ymin = mean_weight - sd_weight, ymax = mean_weight + sd_weight ), width = .2)+
  facet_wrap(~treatment, scales = 'free', labeller = labeller(treatment = label_bar))+
  scale_fill_manual(values = c('coral2', 'skyblue', 'darkgreen', 'blue'),labels = c('Belgien', 'Deutschland', 'Finnland', 'Norwegen'))

## Es gibt Unterschiede zwischen den strains
# Belgien und Norwegen haben groesste Biomasse Produktion 
# bei hohem DOM --> am besten angepasst
# Habitat-check: BE kommt aus DOM reichen See
# FI am wenigsten Biomasseproduktion bei DOM Zugabe (kann es nicht nutzen)
##-> kommt aus kleinem Rockpool
