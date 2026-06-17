# global.R - laddas en gång vid appstart.
#
# Bägge apparnas data + logik källas in här. Apparna har inga delade globala
# objektnamn (flik 2:s objekt är prefixade med handel_), så de kan dela samma
# globala miljö utan att skriva över varandra.

library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(DT)
library(shinyWidgets)
library(openxlsx)
library(plotly)
library(htmltools)
library(here)
library(tidyr)

# Servrar först (laddar data + definierar funktioner), sedan UI-objekten.
source("foretag_server.R", encoding = "utf-8")
source("handel_server.R",  encoding = "utf-8")
source("foretag_ui.R",     encoding = "utf-8")
source("handel_ui.R",      encoding = "utf-8")