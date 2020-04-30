############################### Grundlagenskript##################################################
## Charlotte Kunze

# was ist R studio und wofuer wird es genutzt?
## "Rstudio is and integrated development environment, or IDE, for R programming"
## R Statistikprogramm, Rstudio Benutzeroberflaeche

# Oberflaeche:
## Rscript 
## Console 
## Environment
## Files, Plots, Packages, Help, Viewer


# working directories

## Generelle Sachen:
# Ausfuehren von Befehlen --> Tastenkombination Steuerung und Enter (bei Mac Command und Enter)
# Umlaute
# Speichern von Scripten (UTF8)




############################### Introduction Start ######################################################
#1. Zuweisungen
#Zuweisungen erfolgen mithilfe von Zuweisungsoperatoren wie dem Pfeil <- oder dem Gleichzeichen = 
#z.B. 
variable <- 1
a=1 
b=2

b #Abfrage
# Environment checken!


# 2. Formate die eine Variable annehmen kann
a=1         #ganze Zahl (integer)
b=0.3       #reelle Zahl (real)
a='hello'   #Textobjekt (character) ->"" oder ''
a=TRUE      #alternative boelean variable (logical) --> wie ein Schalter, entweder TRUE (auch T) oder FALSE (F)

#3. Rechnen mit Variablen
a+b
a-b
a*b
a/b

#die Ergebnisse der Rechnungen zu neuen Variablen zuweisen 
d <- a+b
d <- a-b
d <- a*b
M = a/b 

d # um Ergebnis einsehen zu koennen -> erneutes ausfuehren


# 4. Vektoren
#Vektoren koennen wir variablen verschiedene Formate annehmen (homogen)

#Zuweisung
Vector1=c(1,2,3,4,5) # Vektor erstellen mit c(), Elemente werden durch , getrennt  
Vector1=c(1:5)       # 1:5 (alle Zahlen von einschließlich 1 bis 5)

vec2=c("Wort1", "Wort2", "Wort3") # character - "" oder '' 
vec3=c(TRUE, FALSE, TRUE, T, F) # logical 
vec4=c(4:8) #numerical


##--# Uebung:erstelle einen Vektor mit folgenden Elementen 1,2,3,4,5,10,11,12,13,14,15
vec1 <- c(1,2,3,4,5,10,11,12,13,14,15)
#oder
vec1 <- c(1:5, 10:15)

# Rechenoperationen mit Vektoren
# Rechnen mit Vektoren erfolgt nach dem gleichen Prinzip wie das Rechnen mit anderen Variablen

vec1 <- 1:6
vec2 <- 1:3
vec3 <- vec1 + vec2
vec3 <- vec1 - vec2 
vec4 <- vec1 * vec2 
vec3 <- vec1 / vec2
# Die Ergebnisse lassen sich wieder folgendermassen ausgeben: 
vec3 
vec4 


# 5. Das Erstellen einer Matrix 
## Def: Matrix = zweidimensionale Anordnung von Vektoren
# wichtig: alle Elemente muessen einheitliches Format haben (numerisch, character oder logical)

mat1=matrix(c(1,2,3,1,2,3),  #Elemente
            ncol = 2,        # ncol gibt die Anzahl der Spalten 
            dimnames = list(c("Reihe1", "Reihe2", "Reihe3"), # dimnames benennt meine Zeilen und Spalten    
                            c("Spalte1", 'Spalte2')))  # es gilt: ZZ SS - Zeile zuerst, Spalte spaeter
mat1 

##--# Uebung: Erstelle eine 2x2 Matrix mit folgenden Spalten: 
## Spalte 1: 4,3,4 #Spalte 2: 4,2,4
## betitel die Zeilen wie folgt: Z1, Z2, Z3 
## die Spalten sollen wie folgt betitelt werden: s1, s2 

mat2=matrix(c(4,3,4,4,2,4),
            ncol=2,  #R fuellt spaltenweise den Vektor, also von oben nach unten und dann die naechste Spalte
            dimnames = list(c("Z1", "Z2", "Z3"), # Zeilenueberschrift
                            c("s1","s2"))) # Spaltenueberschrfit
# Haeufige Fehler: 
matrix(c(4,3,4), c(4,2,4), ncol=2,
       dimnames = list(c("Z1", "Z2", "Z3"), # Zeilenueberschrift
                       c("s1","s2"))) #hier entsteht ein roter Fehler in meiner Console, da ich zwei Vektoren uebergebe 



# 6. Hilfe finden bei R Studio 
?matrix #mit dem Fragezeichen vor dem Befehl, oeffnet sich ein Hilfefenster in Rstudio



# 7. Erstellen eines Dataframes (df abegekuerzt) 
## Ein Dataframe ist wie eine Matrix, kann aber aus Elementen mit verschiedenen Formaten bestehen
#  heterogen

## zB
dat1 <- data.frame(x1 = 1:3, 
                   x2 = 2:4, 
                   x3 = c('wort1','wort2','wort3')) 


#nun ein anwendungsbezogenes Beispiel

df=data.frame(land=c("Deutschland", "Schweiz", "Niederlande"),   
              pop=c(82.52e6, 8.48e6, 17.13e6), 
              bip_per_capita=c(44550,80591,48346)) 

str(df)

# Ansprechnen einer Spalte innerhalb eines dataframe:
df$land  #oder
df[,1]
# speichern in neuer variable
a = df$land



#Einen Wert aus df ziehen
#Ansprechen des Wertes der in der 1. Zeile 3. Spalte steht 
df[1,3] 


##--## Uebung: erstelle eine neue Spalte in deinem df mit dem gesamt BIP (Produkt aus bip_per_capital und pop)
df$bip= df$pop*df$bip_per_capital #df$bip kreiert neue Spalte mit gewuenschten Elementen 


# plotten der bip_per_capita pro land
plot(x = df$land, y = df$bip_per_capita)


## NA = missing values
df=data.frame(land=c("Deutschland", "Schweiz", "Niederlande"),   
              pop=c(82.52e6, 8.48e6, 17.13e6), 
              bip_per_capita=c(44550,80591,NA)) 

# NA suchen im df
is.na(df)

# Uebung: erstelle eine neue Spalte mit der Summe der bip_per_capita aller Laender
# nutze die Funkiton sum()
df$sum = sum(df$bip_per_capita)
df #gibt NA daher:

df$sum = sum(df$bip_per_capita, na.rm = T)
df

# 8. Packages
# installieren von packages
# laden von packages

install.packages('tidyverse')
library(tidyverse)

## Update Rstudio! 