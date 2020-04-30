#Praktikum Aquatische Oekologie
#1.4.2020
#Einlesen und verarbeiten von Daten in RStudio
#Laden der notwendigen packages
library(tidyverse)
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#Schritt 1:
#Einladen der Daten
#Dies kann entwder per Befehlszeile "read_csv()" o.A. erfolgen
#oder "per Klick" ueber Import Dataset.
#Werden die Daten ueber Import Dataset eingelesen empfiehlt es sich
#anschliessend aus der Console den Zeilenbefehl ins Skript zu kopieren
#um bei einem erneuten Oeffnen den Einlesevorgang zu beschleunigen.
#Wichtig ist: Veraendert sich der Ort der Datei, muss der Dateipfad
#logischerweise angepasst werden.
syltZooplankton <- read_csv("Documents/KRILL_UFZ/PraktikumAquatischeOeko/syltZooplankton.csv")

#Schritt 2:
#Ueberpruefen, ob die Daten richtig eingeladen wurden. 
#Werden Variablen (Spalten) richtig voneinander getrennt?
#Stimmen die Dateiformate der einzelnen Variablen?
View(syltZooplankton) #oeffnet Datensatz innerhalb von RStudio
str(syltZooplankton) #gibt Ueberblick ueber Daten, seine Struktur, Dimension etc.

#Selten liegen die Daten in einem Format vor, das wir sofort visualisieren
#oder analysieren koennen. In der Regel muessen die Daten zunaechst bereinigt,
#zusammengefasst oder gefiltert werden. Hierfuer bietet das "tidyverse" eine
#Handvoll sehr nuetzlicher Funktionen, die das Datenhandling erheblich erleichtern.
#Fuer eine moeglichst frustfreien Ablauf, sollten die Daten nach Moeglichkeit
#bereits "tidy" sein. Sprich:
#Jede Variable ist eine Spalte
#Jede Beobachtung ist eine Zeile
#(Jede Messgruppe ist eine eigener Datensatz)

#Die wichtigsten Funktionen im Schnellueberblick:
#Einschub: Was alle Funktionen des tidyverse gemeinsam haben, ist dass sie 
#an erster Stelle die Information erwarten, auf welchen Datensatz sich die 
#Funktion beziehen soll. Praktische Beispiele folgen unten.
filter() #filtert den Datensatz nach Kriterien die bei einer oder mehreren
#Variablen erfuellt werden sollen. z.B. Nur Daten fuer die Nordsee am 30. Tag
#im Jahr. Hierfuer benoetigt ihr logische Abfragen!

mutate() #erstellt eine neue Spalte, die sich i.d.R. aus bereits vorhandenen
#Spalten berechnet. Muss sie aber nicht!

group_by()
ungroup()
#mit group_by koennt ihr R mitteilen, dass in eurem Datensatz Gruppen
#vorhanden sind, fuer die z.B. statistische Operationen jeweils getrennt angewendet
#werden sollen. z.B. die mittleren Abundanzen aller Arten pro Jahr gerechnet, um Jahre
#miteinander zu vergleichen. Wenn der Datensatz gruppiert wurde, werden in der Folge
#alle Rechnungen "gruppengetrennt" durchgefuehrt. Wenn dies nicht mehr erwuenscht ist,
#kann die Gruppierung mit ungroup() aufgehoben werden.

summarise() #mit summarise werden die numerischen Variablen eines Datensatzes zusammengefasst
#Wenn der Datensatz vorher mit group_by gruppiert wurde, wird die Zusammenfassung der gewuenschten
#Variablen fuer jede Gruppe einzeln berechnet.

select() #kann verwendet werden, wenn einzelne Spalten ausgewaehlt oder 
#entfernt (mit "-" im Vorzeichen) werden sollen.

arrange() #Sortiert den Datensatz aufsteigend nach Variablen

slice() #um nur erwuenschte Zeilen aus dem Datensatz zu behalten

#---------------------------------------------------------------------------#
#Anwendung der oben eingefuehrten Funktionen um den Helgoland-Datensatz
#zusammenzufassen. Uns koennten eine Reihe von Fragen interessieren:
#z.B. Wie viel Zooplankton kommt durchschnittlich pro Monat vor?
#Welche taxonomische Gruppe traegt insgesamt (/jaehrlich/monatlich) zur Gemeinschaft bei
#Erfolgen die Blueten von pelagischem Zooplankton sowie Larven benthischer Bewohner zur gleichen Zeit?
#und noch viele mehr... Ueberlegt euch eure eigenen Fragen und uebt mit den oben
#Welche taxonomischen Gruppen benthischer Organismen produzieren die fuenf haeufigsten Larven?
#eingefuehrten Funktionen. Im Zweifel googelt eure Probleme und helft euch gegenseitig.
#---------------------------------------------------------------------------#
#Frage 1: Wie viel Zooplankton kommt durchschnittlich pro Monat vor.
#Loesungsweg:
#Was ist gesucht: Der Mittelwert der monatlichen Summe aller gezaehlten Individuen unabhaengig ihrer taxonomischen
#Einteilung. 
#Weg dorthin: Zunaechst berechne ich die Summe dere Abundanzen fuer jeden Monat in jedem Jahr:
#Damit ich eine Summe fuer jeden Monat in jedem Jahr getrennt berechnen kann, muss ich den 
#Datensatz vorher entsprechend gruppieren. Um den Originaldatensatz syltZooplankton nicht zu 
#veraendern, erstelle ich fuer diese Aufgabe einen neuen mit dem Namen 'monatsdurchschnitt'
monatsdurchschnitt <- group_by(syltZooplankton, month, year)
#Nun berechne ich die Summe der einzelnen Abundanzen und fasse sie mit summarise zusammen
monatsdurchschnitt <- summarise(monatsdurchschnitt, totalAbund = sum(abund))
#Nun muss ich noch die Mittelwerte dieser Abundanzen berechnen - sprich das Jahr interessiert
#uns jetzt nicht mehr. Daher muss der Datensatz jetzt nur noch nach Monat und nich tmehr nach 
#Jahr gruppiert werden.
monatsdurchschnitt <- group_by(monatsdurchschnitt, month)
monatsdurchschnitt <- summarise(monatsdurchschnitt, meanAbund = mean(totalAbund))
#Darstellen der Ergebnisse:
ggplot(data = monatsdurchschnitt, aes(x = month, y = meanAbund)) +
  geom_col()
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#Frage 2: Welche taxonomischen Gruppen benthischer Organismen produzieren die fuenf haeufigsten Larven?
#Loesungsweg:
#Was ist gesucht: Die fuenf haeufigsten "species" aus der Gruppe "benthic"
#Weg dorthin: filter den Datensatz, sodass er nur noch benthische Arten enthaelt
benthosLarven <- filter(syltZooplankton, habitat == 'benthic')
#gruppiere den Datensatz, sodass die Abundanzen gleicher Arten miteinander addiert werden
benthosLarven <- group_by(benthosLarven, species)
#Summe der einzelnen Abundanzen berechnen
benthosLarven <- summarise(benthosLarven, totalAbund = sum(abund))
#Sortiere die Abundanzen in absteigender Reihenfolge
benthosLarven <- arrange(benthosLarven, desc(totalAbund))
#(Vielleicht nicht notwendig: Behalte nur die ersten fuenf Zeilen)
benthosLarvenTop5 <- slice(benthosLarven, 1:5)
#Darstellen der Ergebnisse:
ggplot(data = benthosLarvenTop5, aes(x = species, y = totalAbund)) +
  geom_col()
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#Der Pipe Operator: Wie ihr seht, werden die eingefuehrten Funktionen i.d.R. aufeinanderfolgend 
#verwendet, um einen Datensatz zusammenzufassen. Damit nicht fuer jeden Einzelschritt
#neue Objekte abgespeichert werden muessen und fuer eine erheblich bessere Lesbarkeit, kann man mit
#dem Pipe-Operator die Funktionen aneinanderreihen:
#Dies hat einige Folgen:
#1. in der ersten Zeile wird der Datensatz spezifiziert, auf den sich die Folgefunktionen
#beziehen sollen
#2. in den Funktionen muss nicht mehr der Datensatz spezifiziert werden, auf denen sich die 
#Funktion beziehen soll (siehe Punkt 1)
#. der Code wird als Block ausgefuehrt und nicht mehr zeilenweise
#Beispiel: Aufgabe 1 unter Verwendung des Pipe-Operators:
monatsdurchschnittPipe <- syltZooplankton %>% 
  group_by(month, year) %>% 
  summarise(totalAbund = sum(abund)) %>% 
  group_by(month) %>% 
  summarise(meanAbund = mean(totalAbund))

#Darstellen der Ergebnisse:
ggplot(data = monatsdurchschnittPipe, aes(x = month, y = meanAbund)) +
  geom_col()

#---------------------------------------------------------------------------#
#Aufgabe 2:
benthosLarvenTop5Pipe <- syltZooplankton %>% 
  filter(habitat == 'benthic') %>% 
  group_by(species) %>% 
  summarise(totalAbund = sum(abund)) %>% 
  arrange(desc(totalAbund)) %>% 
  slice(1:5)

ggplot(data = benthosLarvenTop5Pipe, aes(x = species, y = totalAbund)) +
  geom_col()
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#