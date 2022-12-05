
library(shinyWidgets)
library(shinythemes)
library(shiny)
library(DT)
library(dplyr)
library(tools)
library(leaflet)
library(sf)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(maps)
library(mapdata)
library(sp)
library(tiff)
library(GADMTools)

linebreaks <- function(n){HTML(strrep(br(), n))}
minYear = 1900
maxYear = 2050

valMaxYear <- 2050
valMinYear <- 1900

currentFileOri <- NULL
currentFileRes <- NULL
disease_Ori <- NULL
disease_Res <- NULL

disease_Ori_full <- NULL
disease_Res_full <- NULL
controlDis <- FALSE
column_sequence <- c('famN', 'yearOfBirth', 'yearOfDeath', 'sex', 'mother_id',
                     'father_id', 'anoinicio', 'district_ori', 'county_ori',
                     'district_res', 'county_res')

pallete_colors_red <- rev(c("#ffffcc","#ffeda0","#fed976",
                            "#feb24c","#fd8d3c","#fc4e2a",
                            "#e31a1c","#bd0026","#800026"))

numbersPercentiles <- c(0.90,0.80,0.70,0.60,0.50,0.40,0.30,0.20,0.10)

locations_ori_all <- NULL
locations_res_all <- NULL

locations_ori_filtered <- NULL
locations_res_filtered <- NULL


per_district_ori_all <- NULL
per_county_ori_all <- NULL
per_district_res_all <- NULL
per_county_res_all <- NULL

per_district_ori_filtered <- NULL
per_county_ori_filtered <- NULL
per_district_res_filtered <- NULL
per_county_res_filtered <- NULL


geo_per_district_ori_all <- NULL
geo_per_district_res_all <- NULL

geo_per_district_ori_filtered <- NULL
geo_per_district_res_filtered <- NULL


per_district_ori_decades_all <- NULL
per_district_res_decades_all <- NULL

per_district_ori_decades_filtered <- NULL
per_district_res_decades_filtered <- NULL

per_decade_all <- NULL
per_decade_all <- NULL

per_decade_filtered <- NULL
per_decade_filtered <- NULL

per_each_county_ori_filtered <- NULL

per_year_all <- NULL
per_year_filtered <- NULL

currentInterval <- 0
maxInterval <- NULL

