library(tidyverse)
library(magrittr)
library(mapboxapi)
library(mapdeck)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(fontawesome)
library(dashboardthemes)
library(esquisse)
library(RColorBrewer)
library(scales)

leaflet_design_list <- list(
  'Esri' = list(
    'World Street Map' = 'Esri.WorldStreetMap',
    'Satellite Image' = 'Esri.WorldImagery'
  ),
  'OpenStreetMap' = list(
    'Street Map' = 'OpenStreetMap.Mapnik',
    'Topographic Map' = 'OpenTopoMap'
  ),
  'CartoDB' = list(
    'Dark Grey' = 'CartoDB.DarkMatter',
    'Simple Grey' = 'CartoDB.Positron',
    'Simple Blue' = 'CartoDB.Voyager'
  )
)

################################################
# Default parameters
################################################
color_scheme_list <- list(
  "Viridis" = list(
    "viridis" = viridis_pal(option = "viridis")(10),
    "magma" = viridis_pal(option = "magma")(10),
    # "inferno" = viridis_pal(option = "inferno")(10),
    "plasma" = viridis_pal(option = "plasma")(10)
  ),
  "Brewer" = list(
    "Spectral" = brewer_pal(palette = "Spectral")(8),
    "RdYlGn" = brewer_pal(palette = "RdYlGn")(8),
    "RdYlBu" = brewer_pal(palette = "RdYlBu")(8),
    "RdGy" = brewer_pal(palette = "RdGy")(8),
    "BrBG" = brewer_pal(palette = "BrBG")(8),
    "YlGn" = brewer_pal(palette = "YlGn")(8),
    "Greens" = brewer_pal(palette = "Greens")(8),
    "YlGnBu" = brewer_pal(palette = "YlGnBu")(8),
    "Purples" = brewer_pal(palette = "Purples")(8),
    "PuRd" = brewer_pal(palette = "PuRd")(8),
    "Reds" = brewer_pal(palette = "Reds")(8),
    "Oranges" = brewer_pal(palette = "Oranges")(8),
    "YlOrRd" = brewer_pal(palette = "YlOrRd")(8),
    "Greys" = brewer_pal(palette = "Greys")(8)
    # "RdBu" = brewer_pal(palette = "RdBu")(8),
    #"YlOrBr" = brewer_pal(palette = "YlOrBr")(8),
    # "PuBuGn" = brewer_pal(palette = "PuBuGn")(8),
    # "PuBu" = brewer_pal(palette = "PuBu")(8),
    # "OrRd" = brewer_pal(palette = "OrRd")(8),
    # "RdPu" = brewer_pal(palette = "RdPu")(8),
    # "GnBu" = brewer_pal(palette = "GnBu")(8),
    # "BuPu" = brewer_pal(palette = "BuPu")(8),
    # "BuGn" = brewer_pal(palette = "BuGn")(8),
    # "Blues" = brewer_pal(palette = "Blues")(8)
    )
)

################################################
# Default parameters
################################################
leaflet_desing_default <- 'Esri.WorldStreetMap'
color_scheme_default <- 'viridis'
opacity_default <- 0.5
by_time_default <- 5
max_time_default <- 20
profile_default <- 'walking'

lng_default <- 139.76738621613336
lat_default <- 35.68214410600612



################################################
# Default isochrones
################################################
iso <- mb_isochrone(location = c(lng_default, lat_default),
                    profile = profile_default,
                    time = seq(from = by_time_default, 
                               to = max_time_default, 
                               by = by_time_default)) %>%
  st_as_sf() 


times <- sort(unique(iso$time))
n <- length(times)
time_label <- paste0(c(0, times[1:(n-1)]), ' - ', times)
iso %<>%
  arrange(time) %>%
  mutate(time = factor(time_label, levels = time_label)) 


suppressMessages({
  iso_union_default <- iso %>%
    group_by(time) %>%
    summarise() %>%
    st_difference() %>%
    arrange(desc(time))
})

################################################
# Location of jump function
################################################

city <- tibble(`Tokyo` = c('Japan', 35.68214410600612, 139.76738621613336),
               `Tachikawa` = c('Japan', 35.69787946257096, 139.41358984081072),
               `Osaka` = c('Japan', 34.70266176943028, 135.49589695994277),
               `Kyoto` = c('Japan', 34.98559043888351, 135.75869490844502),
               `Okinawa` = c('Japan', 26.214472727506, 127.67937670830706),
               `Kochi` = c('Japan', 33.56713516079578, 133.5437854909894),
               `New York` = c('U.S.', 40.750541638305485, -73.99346969863792),
               `Washington D.C.` = c('U.S.', 38.89816521371992, -77.03656155926848),
               `San Francisco` = c('U.S.', 37.79305367198945, -122.39732130250127),
               `London` = c('Europe', 51.49542048334222, -0.14382702967026195),
               `Paris` = c('Europe', 48.85810088074671, 2.3472963483416445),
               `Rome` = c('Europe', 41.900616427857, 12.501092046169344),
               `Madrid` = c('Europe', 40.40579943933338, -3.6883746698143796),
               `Singapore` = c('Asia', 1.2871993062371327, 103.85444945367126),
               `Hong Kong` = c('Asia', 22.28451595597855, 114.15807745969856),
               `Bangkok` = c('Asia', 13.739565461325324, 100.5168168597269),
               `Delhi` = c('Asia', 28.614712117235836, 77.19942029388051),
               `Accra` = c('Africa', 5.5487514440665375, -0.21160193454941822),
               `Cape Town` = c('Africa', -33.92079030264826, 18.424777039793295),
               `Cairo` = c('Africa', 30.063599923189884, 31.24705253520849),
               `Sydney` = c('Oceania', -33.86102882683645, 151.21061333427474),
               `Melbourne` = c('Oceania', -37.81786328827631, 144.96710673262317),
               `Auckland` = c('Oceania', -36.8484328351554, 174.7789689108702)) %>%
  mutate(coord = c('region', 'lat', 'lng')) %>%
  gather(name, value, -coord) %>%
  spread(coord, value) %>%
  arrange(desc(name)) %>%
  mutate(lng = as.numeric(lng),
         lat = as.numeric(lat))

region_list <- c('Japan', 'Africa', 'Asia', 'Europe', 'Oceania', 'U.S.')

region_default <- 'Japan'
city_default <- 'Tokyo'

# city_list <- list(
#   'Japan' = list('Tokyo', 'Kyoto', 'Kochi'),
#   'U.S.' = list('New York', 'San Francisco'),
#   'Asia' = list('Delhi', 'Singapore'),
#   'Europe' = list('Paris', 'Rome', 'London')
# )
