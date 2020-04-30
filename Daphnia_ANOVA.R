#Statistische Analysen

aov<-aov(dry_weight_per_daphnia ~treatment*strain, data=waage)
summary(aov)
TukeyHSD(aov)

Belgien<-subset(waage,strain=="BE")
aov1<-aov(dry_weight_per_daphnia ~treatment, data=Belgien)
summary(aov1)
TukeyHSD(aov1)

Deutschland<-subset(waage,strain=="DE")
aov2<-aov(dry_weight_per_daphnia ~treatment, data=Deutschland)
summary(aov2)
TukeyHSD(aov2)

Finnland<-subset(waage,strain=="FI")
aov3<-aov(dry_weight_per_daphnia ~treatment, data=Finnland)
summary(aov3)
TukeyHSD(aov3)

Norwegen<-subset(waage,strain=="NO")
aov4<-aov(dry_weight_per_daphnia ~treatment, data=Norwegen)
summary(aov4)
TukeyHSD(aov4)

Only<-subset(waage,treatment=="O")
aov5<-aov(dry_weight_per_daphnia ~strain, data=Only)
summary(aov5)
TukeyHSD(aov5)

Low<-subset(waage,treatment=="L")
aov6<-aov(dry_weight_per_daphnia ~strain, data=Low)
summary(aov6)
TukeyHSD(aov6)

Intermediate<-subset(waage,treatment=="I")
aov7<-aov(dry_weight_per_daphnia ~strain, data=Intermediate)
summary(aov7)
TukeyHSD(aov7)

High<-subset(waage,treatment=="H")
aov8<-aov(dry_weight_per_daphnia ~strain, data=High)
summary(aov8)
TukeyHSD(aov8)

aov9<-aov(pregnant_daphnia ~treatment*strain, data=waage)
summary(aov9)
TukeyHSD(aov9)

preg_Belgien<-subset(waage,strain=="BE")
aov10<-aov(pregnant_daphnia ~treatment, data=preg_Belgien)
summary(aov10)
TukeyHSD(aov10)

preg_Deutschland<-subset(waage,strain=="DE")
aov11<-aov(pregnant_daphnia ~treatment, data=preg_Deutschland)
summary(aov11)
TukeyHSD(aov11)

preg_Finnland<-subset(waage,strain=="FI")
aov12<-aov(pregnant_daphnia ~treatment, data=preg_Finnland)
summary(aov12)
TukeyHSD(aov12)

preg_Norwegen<-subset(waage,strain=="NO")
aov13<-aov(pregnant_daphnia ~treatment, data=preg_Norwegen)
summary(aov13)
TukeyHSD(aov13)

preg_Only<-subset(waage,treatment=="O")
aov14<-aov(pregnant_daphnia ~strain, data=preg_Only)
summary(aov14)
TukeyHSD(aov14)

preg_low<-subset(waage,treatment=="L")
aov15<-aov(pregnant_daphnia ~strain, data=preg_low)
summary(aov15)
TukeyHSD(aov15)

preg_intermediate<-subset(waage,treatment=="I")
aov16<-aov(pregnant_daphnia ~strain, data=preg_intermediate)
summary(aov16)
TukeyHSD(aov16)

preg_high<-subset(waage,treatment=="H")
aov17<-aov(pregnant_daphnia ~strain, data=preg_high)
summary(aov17)
TukeyHSD(aov17)


cor.test(waage$dry_weight_per_daphnia, waage$pregnant_daphnia, method="pearson")

aov18<-aov(daphnia ~treatment*strain, data=waage)
summary(aov18)
TukeyHSD(aov18)
