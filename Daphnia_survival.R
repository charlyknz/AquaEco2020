####################
##### survival #####
#install packages
#install.packages('survival)'
#usw

#load required packages
library(survival)
library(rms)
library(survminer)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(gridExtra)

#import data table 
#survival<-read.table("~/C:/Users/Nils/Desktop/MaUwi/Basic ecological processes/survival/survival.csv",h=T,sep=";",dec=",")
survival<-read.csv("C:/Users/Nils/Desktop/MaUwi/Basic ecological processes/survival/survival.csv", sep=";")
names(survival) <- c('treatment','strain', 'replicate','indi','dead','survival_days')

#Level ändern
levels(survival$treatment)
survival$treatment <- factor(survival$treatment,levels=c('only','low','intermediate','high'))
BE<-subset(survival, strain=="BE")
DE<-subset(survival, strain=="DE")
FI<-subset(survival, strain=="FI")
NO<-subset(survival, strain=="NO")
View(survival)
#filter for dead animals, which dead for natural causes
BE$surv1BE<-with(BE, Surv(survival_days,dead==1))
DE$surv1DE<-with(DE, Surv(survival_days,dead==1))
FI$surv1FI<-with(FI, Surv(survival_days,dead==1))
NO$surv1NO<-with(NO, Surv(survival_days,dead==1))
#head(survival) # see df

par(mfrow=c(1,1)) #adjust plot options
km1BE<-survfit(surv1BE~treatment,data=BE,conf.type="log-log")
km1DE<-survfit(surv1DE~treatment,data=DE,conf.type="log-log")
km1FI<-survfit(surv1FI~treatment,data=FI,conf.type="log-log")
km1NO<-survfit(surv1NO~treatment,data=NO,conf.type="log-log")
#write function to calculate survival 
km1BE
km1DE
km1FI
km1NO


#plot survival
par(mfrow=c(4,1))
splots <- list()
splots[[1]] <- ggsurvplot(km1BE, data = BE, ylim=c(0.7,1),alpha=7/10,legend="right",pval = TRUE,pval.coord=c(1,0.8),xlab="Days",palette=c("#ffa199","#bd3b31","#730800","#300906"))
splots[[2]] <- ggsurvplot(km1DE, data = DE, ylim=c(0.7,1),alpha=7/10,legend="right",pval = TRUE,pval.coord=c(1,0.8),xlab="Days",palette=c("#ffa199","#bd3b31","#730800","#300906"))
splots[[3]] <- ggsurvplot(km1FI, data = FI, ylim=c(0.7,1),alpha=7/10,legend="right",pval = TRUE,pval.coord=c(1,0.8),xlab="Days",palette=c("#ffa199","#bd3b31","#730800","#300906"))
splots[[4]] <- ggsurvplot(km1NO, data = NO, ylim=c(0.7,1),alpha=7/10,legend="right",pval = TRUE,pval.coord=c(1,0.8),xlab="Days",palette=c("#ffa199","#bd3b31","#730800","#300906"))


arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, risk.table.height = 0.5)


