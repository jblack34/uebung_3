#Schritt 0: Packages laden

# Import Excel
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
NRW_map_data$Wahlbeteiligung <- round(NRW_map_data$Wahlbeteiligung, digits=1)

# tidyverse
NRW19data <- NRW19data %>%
  mutate(Wahlbeteiligung = (abg.Stimmen/Wahlberechtigte)*100)


# Stimmenanteile der einzelnen Parteien berechnen
NRW_map_data$ÖVPpct <- (NRW19data$ÖVP / NRW19data$abg.Stimmen) * 100
NRW_map_data$GRÜNEpct <- (NRW19data$GRÜNE / NRW19data$abg.Stimmen) * 100

# Stimmenanteile auf eine Dezimalstelle runden
NRW_map_data$ÖVPpct <- round(NRW_map_data$ÖVPpct, digits = 1)
NRW_map_data$GRÜNEpct <- round(NRW_map_data$GRÜNEpct, digits = 1)

# tidyverse
NRW19data <- NRW19data %>%
  mutate(Wahlbeteiligung = (abg.Stimmen / Wahlberechtigte) * 100,
         ÖVPpct = (ÖVP / abg.Stimmen) * 100,
         GRÜNEpct = (GRÜNE / abg.Stimmen) * 100) %>%
  mutate(across(c(Wahlbeteiligung, ÖVPpct, GRÜNEpct), round, 1))


#Karten erstellen
# 1. Open a pdf file
pdf("Schwarz_VU3_SS24.pdf") 

#Wahlbeteiligung
tm_shape(NRW_map_data) +
  tm_polygons("Wahlbeteiligung",
              palette = ("-magma"),
              style="pretty", 
              title="Wahlbeteiligung \nin den Regionalwahlkreisen")+
  tm_shape(NRW_map_data) +
  tm_borders(
    col = 000001,
    lwd = 1.5)
#tmap_save()

# Stimmenanteil ÖVP 
tm_shape(NRW_map_data) +
  tm_polygons("ÖVPpct",
              palette = "Greys",
              style="pretty", 
              title="ÖVP Wahlergebnis \nin Prozent")
#tmap_save()

# Stimmenanteil GRÜNE
tm_shape(NRW_map_data) +
  tm_polygons("GRÜNEpct",
              palette = "Greens",
              style="pretty", 
              title="Die Grünen Wahlergebnis in Prozent") +
  tm_shape(NRW_map_data) +
  tm_borders(
    col = 000001,
    lwd = 1.5) 
#tmap_save()

# 3. Close the pdf file
dev.off() 
