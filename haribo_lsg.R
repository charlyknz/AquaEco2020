
## Aquatische Oekologie

# ---------------------------- #
# HARIBO Colorado - Versuch
# ---------------------------- #



#Daten loeschen, die eventuell noch in Global Environment sind
rm(list=ls(all=TRUE))     


#Wir wiederholen gleich die Funktionen von gestern, aber damit das anschaulicher ist laden wir erst die Daten ein.


#Datensatz haribo.csv aus Stud-IP runterladen und dann einlesen
#Rechts oben Reiter 'Environment' -> 'Import Dataset' klicken
#From Text (readr) auswaehlen
#Browse -> Datensatz auswaehlen -> open
#Vorschau angucken als Qualitaetskontrolle


#Datensatz anzeigen lassen --> zeigt uns, wie R die Daten sieht (als numeric, character/string, integer, logical, factor)
View(haribo) #oder
str(haribo)  


#Laden der benoetigten Packages
library(tidyverse)


#Wir haben gesehen, Data is not tidy!
#Erinnerung:
#Tidy data = Jede Variable ist eine Spalte!
#            Jede Beobachtung ist eine Zeile!
#            Jede Beobachtungseinheit ist eine Tabelle!
#Also aus breiter Tabelle eine lange machen (Faustregel).

#Frage: Was muessen wir aeandern, damit Daten tidy werden?
# --> Namen muessen eine Variable werden


#Make it tidy with gather()
haribo_tidy <- gather(haribo, key = "Site", value = "Abundance", -Species, na.rm = T)
#Wir wollen neue Column 'Site'(=KEY), in die unsere Namen kommen.
#Fuer jede Site/Person wollen wir die Abundance jeder Art haben(=VALUE).
#-Species, da wir diese Spalte behalten wollen



#haribo_tidy Datensatz anschauen
#dafuer haribo_tidy in Console schreiben und ausfuehren oder mit View(haribo_tidy) anzeigen lassen





#Wiederholung tidyverse-Funktionen von gestern

filter() #filtert den Datensatz nach Kriterien die bei einer Varibalen erfuellt sein soll (mit ==)
filter(haribo_tidy, Species=="Fledermaus orange")

mutate() #erstellt eine neue Spalte, die sich i.d.R. aus bereits vorhandenen Spalten berechnet
mutate(haribo_tidy, Ueberschrift=Abundance+100)

group_by() #Datensatz gruppieren, nun werden alle Rechnungen "gruppengetrennt" durchgefuehrt.
group_by(haribo_tidy, Species)
ungroup()  #Gruppierung aufloesen

summarise() #mit summarise werden die numerischen Variablen eines Datensatzes zusammengefasst
#Wenn der Datensatz vorher mit group_by gruppiert wurde, wird die Zusammenfassung der gewuenschten
#Variablen fuer jede Gruppe einzeln berechnet.
summarise(haribo_tidy, Ueberschrift = mean(Abundance))

select()  #Spalten ausgewaehlen oder mit "-" im Vorzeichen aus Datensatz
select(haribo_tidy, Species, Abundance) #gleiches Ergebnis wie (negativ formuliert):
select(haribo_tidy, -Site)

slice()   #Zeilen auswählen
slice(haribo_tidy, 1:10)

arrange() #Sortiert den Datensatz aufsteigend nach Variablen oder mit desc(Variable) absteigend
arrange(haribo_tidy, Abundance)






#Schritt 1
#Erstelle Rangabundanzdiagramm mit deinen Daten

#Erstmal aus dem Gesamtdatensatz die eigenen Daten herausziehen
mydata <- filter(haribo_tidy, Site == 'Laura')  #Nutze 'filter', um deine Daten aus dem df zu ziehen
#mydata in Console eingeben und anschauen

#Fuer das Rang-Abundanz-Diagramm muessen wir die Arten nach ihren Haeufigkeiten sortieren
RAD <- arrange(mydata, desc(Abundance))     #Sortiere die Abundanzen in absteigender (descending) Reihenfolge
#RAD in Console anschauen 
  
#Jetzt muessen wir rechnen. Wir brauchen neue Spalten, deren Eintraege sich aus alten Spalten berechnen.
#Dafuer nutzen wir mutate()
#Die Ueberschrift der neuen Spalte steht vor dem =.
#Hinter dem Gleichheitszeichen steht, wie die Werte in der Spalte errechnet werden sollen, bzw wie sie sich zusammensetzen.
RAD <- mutate(RAD, Rank = row_number())        

RAD <- mutate(RAD, TotalAbundance = sum(Abundance))    

RAD <- mutate(RAD, RelativeAbundance = Abundance/TotalAbundance)    #Relative Abundanz errechnen, indem Abundanz der Art durch Gesamtabundanz geteilt wird
#RAD in Console eingeben und anschauen




### Plotten mit ggplot ####

##?ggplot anschauen

#Wir sehen: ggplot visualisiert unsere Daten.
#wir muessen ggplot zunaechst sagen, welchen data frame R nutzen soll mit data =
#aes steht fuer aesthetics. Mit aes(x= , y=) definieren wir die Variablen, die auf die Achsen sollen
#mit + werden weitere Komponenten angefuegt, z.B. ueber geom_point() unsere Datenpunkte
#die Datenpunkte sollen durch linien verbunden werden, daher nutzen wir geom_line()

ggplot(data= RAD, aes(x=Rank, y=Abundance))+              #ggplot mit verschiedenen Layers
  geom_point()+                                           #Punkte dazu (in den Klammern kann man Groesse, Form, Farbe etc spezifizieren --> fuer Hilfe '?geom_point' eingeben)
  geom_line()                                             #Verbinden der Punkte durch Linien (auch hier sind zahlreiche Moeglichkeiten zur Spezifikation --> '?geom_line')


#Mit geom_text() etc koennen wir Textlabel hinzufuegen, damit wir wissen, welche Art welche ist
ggplot(data= RAD, aes(x=Rank, y=Abundance))+              #ggplot mit verschiedenen Layers
  geom_point()+                                           #Punkte dazu (in den Klammern kann man Groesse, Form, Farbe etc spezifizieren --> fuer Hilfe '?geom_point' eingeben)
  geom_text(aes(label=Species), hjust=-0.05, vjust=0.3)+  #Textlabel --> Eintraege der Spalte 'Species' sollen als Label fungieren, hjust= und vjust= veraendert die Position der Label
  geom_line() 


#Jetzt habt ihr eine Idee, wie ihr die Funktionen anwenden koennt, 
#um Rang-Abundanz-Diagramme zu erstellen.

#Wir teilen uns nun auf in Tutorengruppen und  bearbeiten folgende Fragen aus dem Skript:

"
- Wie sieht ein solchen RA Diagramm fuer einen Standort (e.g. eine Packung=Person) aus?
- Wie ändert sich die Verteilung, wenn mehrere Packungen 
#(e.g. Standorte in einem Habitat) betrachtet werden?
- Wie sieht die Verteilung in unterschiedlichen Habitaten 
#(Haribocolorado mini oder maxi oder vegane Alternative) aus?
- Ist die Betrachtung absoluter oder relativer Abundanzen sinnvoller?
- Gibt es dominante Arten und sind dies immer dieselben?
"















'ODER, aber auch nicht besser:

library(ggrepel)
#ggplot
ggplot(RAD, aes(x=Rank, y=Abundance, label=Species))+   
  geom_point()+   
  geom_text_repel(aes(label=Species))+
  geom_text(aes(label=Species),hjust=-0.1, vjust=0.5)+
  geom_line()    '







#Schritt 2
#Erstelle Rangabundanzdiagramm aus allen Daten (alle Daten gemittelt)
#--> Welche Art dominiert in allen Habitaten/Tueten?

haribo_tidy2 <- group_by(haribo_tidy, Species)  #gruppiere den Datensatz, sodass die Abundanzen gleicher Arten miteinander addiert werden koennen (naechster Schritt)

over_sites <- summarise(haribo_tidy2, Abundance = sum(Abundance))  #Summe der einzelnen Abundanzen berechnen

over_sites <- over_sites%>%
  arrange(desc(Abundance))%>%
  mutate(Rank= row_number())%>% 
  mutate(TotalAbundance = sum(Abundance))%>%
  mutate(RelativeAbundance=Abundance/TotalAbundance)


#ggplot
ggplot(over_sites, aes(x=Rank, y=RelativeAbundance, label=Species))+   
  geom_point()+ 
  geom_text(aes(label=ifelse(RelativeAbundance>0.05, as.character(Species),'')), hjust=-0.05, vjust=0.3)+
  geom_line()  







##########Graphische Loesung###############
#Vergleiche, welche Arten in welchem Habitat dominiert

einzeln <- haribo_tidy %>%
  group_by(Site)

einzeln <- einzeln %>%
  arrange(desc(Abundance))%>%
  mutate(Rank= row_number())%>%
  mutate(TotalAbundance = sum(Abundance))%>%
  mutate(RelativeAbundance = Abundance/TotalAbundance) 


'#ggplot
ggplot(einzeln, aes(x=Rank, y=RelativeAbundance, group=Site))+
  geom_point()+
  geom_text(aes(label=ifelse(RelativeAbundance>0.08, as.character(Species),'')), hjust=-0.05, vjust=0.3)+
  geom_line()+                
  facet_wrap(~Site, ncol=4)+
  theme_bw()'

#ggplot ohne facet_wrap and theme
ggplot(einzeln, aes(x=Rank, y=RelativeAbundance, group=Site, colour=Site))+
  geom_point()+
  geom_text(aes(label=ifelse(RelativeAbundance>0.08, as.character(Species),'')), hjust=-0.05, vjust=0.3)+
  geom_line()



############### Alternative Loesung ###############
aufgabe4 = arrange(haribo_tidy, Site, desc(Abundance))
aufgabe4 = mutate(group_by(aufgabe4,Site), rank = row_number())
aufgabe4 = filter(aufgabe4, rank %in% c(1:3))

auswertung = summarise(group_by(aufgabe4, Species), domSpecies = n())
auswertung = arrange(auswertung, desc(domSpecies))


###########################################################################

##### ODER Schritt 1 auch mit Pipe-Operator: ######



#Schritt 1
#Erstelle Rangabundanzdiagramm mit deinen Daten

mydata <- filter(haribo_tidy, Site == 'Laura')  #Nutze 'filter', um deine Daten aus dem df zu ziehen

RAD <- arrange(mydata, desc(Abundance))%>%      #Sortiere die Abundanzen in absteigender (descending) Reihenfolge
  mutate(Rank= row_number())%>%            #Fuege neue Spalten mit 'mutate' hinzu 
  mutate(TotalAbundance = sum(Abundance))%>%    #--> die Ueberschrift der neuen Spalte steht vor dem =. Hinter dem Gleichheitszeichen steht, wie die Werte in der Spalte errechnet werden sollen, bzw wie sie sich zusammensetzen
  mutate(RelativeAbundance=Abundance/TotalAbundance)    #Relative Abundanz errechnen, indem Abundanz der Art durch Gesamtabundanz geteilt wird

#ggplot
ggplot(data= RAD, aes(x=Rank, y=Abundance))+              #ggplot mit verschiedenen Layers
  geom_point()+                                           #Punkte dazu (in den Klammern kann man Groesse, Form, Farbe etc spezifizieren --> fuer Hilfe '?geom_point' eingeben)
  geom_text(aes(label=Species), hjust=-0.05, vjust=0.3)+  #Textlabel --> Eintraege der Spalte 'Species' sollen als Label fungieren, hjust= und vjust= veraendert die Position der Label
  geom_line()                                             #Verbinden der Punkte durch Linien (auch hier sind zahlreiche Moeglichkeiten zur Spezifikation --> '?geom_line')
