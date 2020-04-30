#Einladung der Daten
#12.02.2020

library(tidyverse)
library(RColorBrewer)
setwd("~/Uni Oldenburg M.Sc. MUWI/Basic ecological processes/R Files")
exp3=read.csv("bep_experiment_3.csv", dec=".", sep=";")
names(exp3)<-c("treatment","strain","replicate","date","sampling","number_daphnia","cellcounter")

data<- exp3 %>%
  group_by(treatment,strain,sampling) %>%
  mutate(mean_cellcounter=mean(cellcounter, na.rm=T),
         sd_cellcounter=sd(cellcounter, na.rm=T))

waage=read.csv2("bep_experiment_3_waage.csv", dec=",", sep=";")
#str(waage)
waage<-waage[-c(1:5),] #entfernt die ersten 5 Zeilen


data_w<- waage %>%
  group_by(treatment,strain) %>%
  summarize(mean_dry_weight=mean(dry_weight_per_daphnia, na.rm=T),
         sd_dry_weight=sd(dry_weight_per_daphnia, na.rm=T))


ggplot(data=data_w, aes(x=treatment, y=mean_dry_weight, col=strain, group=strain,
                        xlab="treatment",ylab="dry weight gain per individual [mg]"))+
  geom_point()+
  theme_dark()+
  geom_errorbar(aes(ymin=mean_dry_weight-sd_dry_weight, 
                    ymax=mean_dry_weight+sd_dry_weight ))
ggsave("dryweight_treatment.png")


#4 Boxplots je pro Strain
waage$treatment=factor(waage$treatment, levels=c("O","L","I","H"))

ggplot(data=waage, aes(x=treatment, y=dry_weight_per_daphnia, fill=treatment, group=treatment))+
  labs(x="treatment",y="dry weight gain per individual [mg]")+
  geom_boxplot()+
  facet_wrap(~strain, nrow=1)+
  scale_fill_brewer(palette="Reds")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))
  
ggsave("dryweight_treatment_boxplot.png")

#4 Boxplots je pro Treatment
ggplot(data=waage, aes(x=strain, y=dry_weight_per_daphnia, fill=strain, group=strain))+
  labs(x="strain",y="dry weight gain per individual [mg]")+
  geom_boxplot()+
  facet_wrap(~treatment, nrow=1)+
  theme_dark()

ggsave("dryweight_strain_boxplot.png")


#Plot für Pregnant Daphnia
ggplot(data=waage, aes(x=treatment, y=pregnant_daphnia, fill=treatment, group=treatment))+
  labs(x="treatment",y="pregnant individuals")+
  geom_boxplot()+
  facet_wrap(~strain, nrow=1)+
  scale_fill_brewer(palette="Reds")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))
  
ggsave("pregnant_treatment_boxplot.png")

#Plot für Pregnant Daphnia
ggplot(data=waage, aes(x=strain, y=pregnant_daphnia, fill=strain, group=strain))+
  labs(x="strain",y="pregnant individuals")+
  geom_boxplot()+
  facet_wrap(~treatment)+
  theme_dark()

ggsave("pregnant_strain_boxplot.png")

#Plot für degenerated Daphnia
ggplot(data=waage, aes(x=strain, y=degenerated_daphnia, fill=strain, group=strain))+
  labs(x="strain",y="degenerated individuals")+
  geom_boxplot()+
  facet_wrap(~treatment)+
  theme_dark()

ggsave("degenerated_strain_boxplot.png")

#Plot für Pregnant Daphnia
ggplot(data=waage, aes(x=treatment, y=degenerated_daphnia, fill=strain, group=treatment))+
  labs(x="treatment",y="degenerated individuals")+
  geom_boxplot()+
  facet_wrap(~strain)+
  theme_dark()

ggsave("degenerated_treatment_boxplot.png")


#Cellcounter Werte
cellcounter=read.csv2("bep_experiment_3_cellcounter.csv", dec=",", sep=";")
cellcounter<-cellcounter[-c(1:6),-c(8:10)]#unnötige Zeilen und Spalten entfernen
names(cellcounter)<-c("strain","treatment","replicate","date","sampling","cellcounter","corrected_cellcounter")

data_cell<- cellcounter[-c(61:76),] %>%
  group_by(treatment,strain,sampling) %>%
  mutate(mean_cellcounter=mean(cellcounter, na.rm=T),
         sd_cellcounter=sd(cellcounter, na.rm=T),
         se_cellcouner=se(cellcounter,na.rm=T))

data_cell$treatment=factor(data_cell$treatment, levels= c("O","L","I","H"))

ggplot(data=data_cell, aes(x=treatment, y=mean_cellcounter, col=strain, group=strain))+
  labs(x="treatment",y="cellcounter")+
  geom_point()+
  geom_errorbar(aes(ymin=mean_cellcounter-se_cellcounter, ymax=mean_cellcounter+se_cellcounter))
  #geom_boxplot()+
  facet_wrap(~sampling)+
  theme_bw()

