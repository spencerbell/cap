##This should detect and install missing packages before loading them - hopefully!
library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(raster)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(rsconnect)
library(ggmap)
library(purrr)
library(sf)
library(gridExtra)
library(sjPlot)
library(sp)

source("mod_shpPoly.R")
source("chooser.R")



list.of.packages <- c("shiny","ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
