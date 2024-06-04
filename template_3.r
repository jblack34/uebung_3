#Schritt 0: Packages laden


#Import Excel
library(readxl)

# Import shapefile
install.packages("sf")
library(sf)

# Import colors 
install.packages("RColorBrewer")
library(RColorBrewer)

# Import colors 
install.packages("viridis")
library(viridis)

#Fehlerüberprüfung - function "anti_join" + "mutate" zum erstellen der Variable Wahlbeteiligung
install.packages("dplyr")
library(dplyr)

#Package für einfache Karten
install.packages("tmap")
library(tmap)

install.packages("Rcpp")
library(Rcpp)


# Arbeitsplatz aufräumen/leeren
rm(list = ls())

# Arbeitsverzeichnis festlegen
getwd()
setwd("C:/Users/j-sch/VU_PolKom/uebung_3/")
getwd()


#Schritt 1: Daten laden

# NRW2019 Wahlergebnisse
NRW19data <- read_excel("./data_3/NRW2019_RWKErg.xlsx")

# NRW-Wahlkreiseinteilung
NRW_geo <- sf::st_read("./data_3/RWK2019/", stringsAsFactors = FALSE)


#Schritt 2: Daten in benötigte Form bringen

attach(NRW19data)


#Angleichung von Variablennamen

names(NRW_geo)[2] <- "NAME"
names(NRW_geo)[1] <- "ID"


#Überprüfung auf Probleme beim Zusammenführen

problems <- anti_join(NRW_geo, NRW19data, by = "NAME")


#Datensätze zusammenführen

NRW_map_data <- merge(NRW_geo, NRW19data, by = "NAME")



#Variable Wahlbeteiligung erstellen

attach(NRW19data)

# basic R
NRW_map_data$Wahlbeteiligung <- (NRW19data$abg.Stimmen/Wahlberechtigte)*100
#Variable Wahlbeteiliung auf eine Dezimalstelle runden 
NRW_map_data$Wahlbeteiligung <- round(NRW19data$Wahlbeteiligung, digits=1)

# auf selbe Art und Weise wie bei der Beteiligungsrate die Stimmenanteile der einzelnen Parteien berechnen !
# ...

# tidyverse
NRW19data <- NRW19data %>%
  mutate(Wahlbeteiligung = (abg.Stimmen/Wahlberechtigte)*100)



#Schritt 3: Karten erstellen


#Wahlbeteiligung
tm_shape(NRW_map_data) +
  tm_polygons("Wahlbeteiligung",
              palette = ("magma"),
              style="order", 
              title="Wahlbeteiligung \nin den Regionalwahlkreisen")
tmap_save()

#Umgekehrte Farben
tm_shape(NRW_map_data) +
  tm_polygons("Wahlbeteiligung",
              palette = ("-magma"),
              style="order", 
              title="Wahlbeteiligung \nin den Regionalwahlkreisen")
tmap_save()

#Parteiergebnisse

tm_shape(NRW_map_data) +
  tm_polygons("ÖVPpct",
              palette = "Greys",
              style="pretty", 
              title="ÖVP Wahlergebnis \nin Prozent")
savePlot()

# Parteiergebnisse
# mit Grauskala
tm_shape(NRW_map_data) +
  tm_polygons("ÖVPpct",
              palette = "Greys",
              style="pretty", 
              title="ÖVP Wahlergebnis \nin Prozent")


# Farbpaletten: Blues, Greens, Purples, Spectral (und viele mehr)

tm_shape(NRW_map_data) +
  tm_polygons("SPÖpct",
              palette = "Reds",
              style="pretty", 
              title="SPÖ Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 0.5)


# Breite der Grenzen verändern -> tm_borders(lwd = 0.5)

tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 2)


# Gebietslabels -> tm_text

tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 2) +
  tm_text("ID.x", size=1)


# Quellenangaben -> tm_legend

tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 2) +
  tm_text("ID.x", size=1) +
  tm_layout("BM f. Inneres (2019), Ergebnis der NRW 2019",
            legend.title.size = 0.5,
            legend.text.size = 0.3,
            title.position = c('left', 'top'),
            legend.position = c("left", "top"), 
            legend.bg.color = "white",
            legend.bg.alpha = 1)


#Möchte man eine einfache interaktive Karten erstellen, kann man den tmap Modus umstellen. 
#"plot" = einfache Grafik, "view" = interaktive Grafik

tmap_mode("view")
tmap_last()
#tmap_options(check.and.fix = TRUE)


## Extravagante Farbpaletten

# Wes Anderson
# https://rforpoliticalscience.com/2020/07/26/make-wes-anderson-themed-graphs-with-wesanderson-package-in-r/
install.packages("wesanderson")
library(wesanderson)


# Queen's Gambit
# https://github.com/rmvpaeme/queensgambit

devtools::install_github("rmvpaeme/queensgambit")
library("queensgambit")



# offizielle Parteifarben
# color codes

# ÖVP
# SPÖ
# Grüne
# NEOS
# FPÖ



## Abspeichern der Grafik

# 1. Open a pdf file
pdf("test.pdf") 

# 2. Create a plot
tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 2) +
  tm_text("ID.x", size=1) +
  tm_layout("BM f. Inneres (2019), Ergebnis der NRW 2019",
            legend.title.size = 0.5,
            legend.text.size = 0.3,
            title.position = c('left', 'top'),
            legend.position = c("left", "top"), 
            legend.bg.color = "white",
            legend.bg.alpha = 1)
tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(lwd = 2) +
  tm_text("ID.x", size=1)


# 3. Close the pdf file
dev.off() 




# analog mit anderen Grafikformaten


## Übung 3: Landkarten mit Ergebnissen der NRW 2017


# Grafikdesign mit Base R oder mit GGPlot2
# Wilkinson et al, The Grammar of Graphics


# https://ggplot2.tidyverse.org/
# https://exts.ggplot2.tidyverse.org/gallery/

## GGplot2





