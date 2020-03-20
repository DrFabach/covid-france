

### il faut réussit à mettre en forme la base de données data 

# Créer une base de données décés
#- En ligne : chaque departement
#- En variable 
#        le nom du departement (dep-67), 
#        puis l'ensemble des jour à partir du 24 janvier et dans les cellules la somme cumulé des déces

# Créer une base de données cas

#- En ligne : chaque departement
#- En variable 
#        le nom du departement (dep-67), 
#        puis l'ensemble des jour à partir du 24 janvier et dans les cellules la somme cumulé des cas


library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(RCurl)
library(plotly)
library(viridis)
library(tidyverse)

URL <- getURL("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv")
data <- read.csv(text = URL, check.names = F,stringsAsFactors = F)
data<- data%>%filter(granularite == "departement")%>%select(- source_nom,-source_url,-granularite)
data<- unique(data)
data[is.na(data)]<-0
data<- data%>%group_by(date, maille_code)%>%summarise(cas_confirmes = max(cas_confirmes), deces = max(deces))
