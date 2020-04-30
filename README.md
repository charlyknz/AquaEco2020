# AquaEco2020
Data and R files used in the Aquatic Ecology course 2020 at the University of Oldenburg
All files are in German language

The course was comprised of 6 sessions
session 1: General introduction into Rstudio
           introducing R (matrix, vector, df, variables,values and formats) Intro_Teil1_lsg.R
           upload Data (tidyverse and basic functions) Intro_DatenInR.R and syltZooplankton.csv as data

session 2: Topic 1 Light data 
           Sweden Incubation experiment data on light intensity and fluorescence absorption or carbon content 
           Top1_Licht_ed.R uses Top1_Licht.csv

session 3: Rank Abundance Diagrams RAD
           haribo 'experiment' data 
           harbio_lsg.R uses haribo.csv
           
session 4: Nutrient data
           Data were collected during the UOP practical course in summer 2019
           naehrstoffe.R uses naehrstoffe.csv
           
session 5: Nutrient and Temperature effects 
           Complex dataset on nutrient and temperature data, extracted from the publication Verbeek et al. 2018 Global Change Biology
           Top5_Temperatur_und_Naehrstoff_Interaktionen.R uses Data_planktodiversa.csv +  Metadata_file_planktodiversa.csv 
           additional exercises are stored in zusatz.R using the same data

session 6: Daphnia and DOM experiment
           daphnia_solutions.R uses data_daphnia.csv 
           the original Data are stored in bep_experiment_3_waage.csv and were collected during the BEP practical course in February 2020
           Daphnia_additional_Data.R and Daphnia_ANOVA.R contain furthter inspirations, what to do with these data
           using Daphnia_survival.R and the Daphnia_survival.csv it is possible to conduct a survival analysis.
           
Additional session: Introducing the grammar of graphics 
                    grammarofgraphics.R introduces the grammar of graphics within ggplot and shows how to add them to a plot
                    also creates the graphs discussed in the grammarofgraphics.pdf file 
                    Using the pdf as an introduction, the students were asked to dismantle the graph in its components 
