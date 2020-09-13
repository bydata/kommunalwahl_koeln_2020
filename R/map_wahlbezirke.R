library(tidyverse)
library(osmdata)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(tictoc)
library(sf)


# Area of Cologne
coords_cgn <- getbb("Koeln, Germany")
coords_cgn

# calculate center from location matrices
calculate_center <- function(coords) {
  x_center <- mean(coords[1, ])
  y_center <- mean(coords[2, ])
  list(lon = x_center, lat = y_center)
}

# Mer losse den Dom in Kölle - to center map on cathedral
coords_cathedral <- getbb("Koelner Dom, Koeln", featuretype = "street") %>% 
  calculate_center() %>% 
  as_tibble()

# load shapefile of Cologne's voting districts
# source: https://www.offenedaten-koeln.de/dataset/stimmbezirk
#shape_url <- "https://www.offenedaten-koeln.de/sites/default/files/2013-06-10_shape_stimmbezirk_0.zip"
# shape_filepath <- "input/2013-06-10_shape_stimmbezirk_0.zip"
# shape_url <- "https://www.offenedaten-koeln.de/sites/default/files/2014-04-02_shape_stimmbezirk.zip"
# shape_filepath <- "input/2014-04-02_shape_stimmbezirk.zip"
# unzip(shape_filepath, exdir = "input")
# shapes <- st_read("input/Shape Stimmbezirk/Stimmbezirk.shp", stringsAsFactors = FALSE)


wb_shape_url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Wahlgliederung_15/MapServer/2/query?where=objectid%20is%20not%20null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=%2A&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=pjson"
wb_shape_filepath <- "input/wahlbezirke.json"
if (!file.exists(wb_shape_filepath)) {
  download.file(wb_shape_url, destfile = wb_shape_filepath, mode = "wb")
}

# import shapefile
shapes <- st_read(wb_shape_filepath, stringsAsFactors = FALSE, quiet = TRUE)

# transform coordinates into lat/long
shapes_trans <- st_transform(shapes, "+proj=longlat")


# function to create labels for Rat and Bezirk
create_labels_1 <- function(NUMMER, NAME, counted, stimmbezirke, turnout, CDU, SPD, GRÜNE, FDP, LINKE, AfD, wahl) {
  htmltools::HTML(
    glue::glue(
      "<b>{wahl}</b><br>
        <b>Wahlbezirk {NUMMER}, {NAME}</b><br>
        {counted} von {stimmbezirke} Stimmbezirken ausgezählt<br>
        <br>
        <b>Wahlbeteiligung</b>: {turnout} %<br>
        <b>CDU</b>: {CDU} %<br>
        <b>SPD</b>: {SPD} %<br>
        <b>Grüne</b>: {GRÜNE} %<br>
        <b>FDP</b>: {FDP} %<br>
        <b>DIE LINKE</b>: {LINKE} %<br>
        <b>AfD</b>: {AfD} %<br>
        "
    )
  )
}

# function to create labels for Rat and Bezirk
create_labels_2 <- function(NUMMER, NAME, counted, stimmbezirke, turnout, 
                            `Reker, Einzelbewerberin`, 
                            `Kossiski, SPD`,
                            `Detjen, DIE LINKE`,
                            `Cremer, AfD`,
                            wahl
                            ) {
  htmltools::HTML(
    glue::glue(
      "<b>{wahl}</b><br>
        <b>Wahlbezirk {NUMMER}, {NAME}</b><br>
        {counted} von {stimmbezirke} Stimmbezirken ausgezählt<br>
        <br>
        <b>Wahlbeteiligung</b>: {turnout} %<br>
        <b>Henriette Reker, Einzelbewerberin</b>: {`Reker, Einzelbewerberin`} %<br>
        <b>Andreas Kossiski, SPD</b>: {`Kossiski, SPD`} %<br>
        <b>J&ouml;rg Detjen</b>: {`Detjen, DIE LINKE`} %<br>
        <b>Christer Cremer, AfD</b>: {`Cremer, AfD`} %
        "
    )
  )
}

# scrape election results
source("R/scrape_results.R")

# join election results with shapes
# shapes_trans_elect <- shapes_trans %>% 
#   left_join(results$ratswahl, by = "NUMMER", suffix = c("", ".y")) %>% 
#   select(-NAME.y) %>% 
#   left_join(results$bezirkswahl, by = "NUMMER", suffix = c(".rat", ".bezirk")) %>% 
#   left_join(results$obwahl, by = "NUMMER", suffix = c("", "ob")) %>% 
#   mutate(NAME = NAME.rat) %>% 
#   select(-NAME.rat, -NAME.bezirk)



shapes_trans_elect <- map(results, 
                          ~shapes_trans %>% 
                            left_join(.x, 
                                      by = "NUMMER", 
                                      suffix = c("", ".y")) %>% 
                            select(-NAME.y)
                          )


party_colors <- tribble(
  ~party, ~color,
  "CDU", "black",
  "SPD", "red",
  "GRÜNE", "green",
  "FDP", "yellow",
  "DIE LINKE", "purple",
  "AfD", "brown"
)


get_leading_party <- function(df) {
  leading_party <-  df %>% 
    select(-OBJECTID, -LINK, -counted, -stimmbezirke, -eligible, -turnout, -valid) %>% 
    pivot_longer(cols = c(-NUMMER, -NAME, -geometry),
                 names_to = "party",
                 values_to = "share"
                 ) %>% 
    group_by(NUMMER) %>% 
    filter(rank(-share) == 1) %>% 
    ungroup() %>% 
    select(-geometry) %>% 
    arrange(NUMMER)
  
  df %>% 
    inner_join(leading_party) %>% 
    left_join(party_colors, by = "party")
}

shapes_trans_elect <- 
  map(shapes_trans_elect, get_leading_party)



# create labels
# shapes_trans_elect_labels <- shapes_trans_elect %>% 
#   mutate(label_ratswahl = pmap(list(
#     NUMMER, NAME, counted.rat, stimmbezirke.rat, turnout.rat, CDU.rat, 
#     SPD.rat, GRÜNE.rat, FDP.rat, `DIE LINKE.rat`, AfD.rat, "Ratswahl"
#   ), create_labels_1),
#   label_bezirkswahl = pmap(list(
#     NUMMER, NAME, counted.bezirk, stimmbezirke.bezirk, turnout.bezirk, CDU.bezirk, 
#     SPD.bezirk, GRÜNE.bezirk, FDP.bezirk, `DIE LINKE.bezirk`, AfD.bezirk, "Bezirksvertretungswahl"
#   ), create_labels_1),
#   label_obwahl = pmap(list(
#     NUMMER, NAME, counted, stimmbezirke, turnout, 
#     `Reker, Einzelbewerberin`,
#     `Kossiski, SPD`, `Detjen, DIE LINKE`, `Cremer, AfD`,
#     "Oberbürgermeisterwahl"
#   ), create_labels_2),
#   ) 

shapes_trans_elect_labels <- list()

shapes_trans_elect_labels$ratswahl <- shapes_trans_elect$ratswahl %>% 
  mutate(label = pmap(list(
    NUMMER, NAME, counted, stimmbezirke, turnout, CDU, 
    SPD, GRÜNE, 
    FDP = NA, 
    `DIE LINKE`, AfD, 
    "Ratswahl"
  ), create_labels_1))
  
  
shapes_trans_elect_labels$bezirkswahl <- shapes_trans_elect$bezirkswahl %>%   
  mutate(label = pmap(list(
    NUMMER, NAME, counted, stimmbezirke, turnout, CDU, 
    SPD, GRÜNE, FDP, `DIE LINKE`, AfD, "Bezirksvertretungswahl"
  ), create_labels_1))
  
  
shapes_trans_elect_labels$obwahl <- shapes_trans_elect$obwahl %>% 
  mutate(label = pmap(list(
    NUMMER, NAME, counted, stimmbezirke, turnout, 
    `Reker, Einzelbewerberin`,
    `Kossiski, SPD`, `Detjen, DIE LINKE`, `Cremer, AfD`,
    "Oberbürgermeisterwahl"
  ), create_labels_2),
  )

#glimpse(shapes_trans_elect_labels$ratswahl)


# create map
m <- leaflet(options = leafletOptions(worldCopyJump = FALSE, dragging = TRUE)) %>%
  addTiles() %>% 
  addProviderTiles(options = tileOptions(noWrap = TRUE), 
                   provider = "CartoDB", 
                   group = "CartoDB") %>%
  setView(lng = coords_cathedral[1, ]$lon,
          lat = coords_cathedral[1, ]$lat,
          zoom = 11) %>%
  # limit map bounds to Cologne area
  setMaxBounds(lng1 = coords_cgn[1,1], lng2 = coords_cgn[1,2], lat1 = coords_cgn[2,1], lat2 = coords_cgn[2,2]) 
#%>% 
  # legend with party colors
 # addLegend(pal = ~party_colors$color, values = ~party_colors$party)
  

m <- m %>% 
  addPolygons(data = shapes_trans_elect_labels$ratswahl,
              # fillColor = ~colorNumeric("PiYG", turnout)(turnout),
              fillColor = ~color,
              label = ~label,
              color = "red", 
              weight = 0.5,
              group = "Ratswahl") %>% 
  addPolygons(data = shapes_trans_elect_labels$bezirkswahl,
              # fillColor = ~colorNumeric("PiYG", turnout)(turnout),
              fillColor = ~color,
              label = ~label,
              color = "blue", 
              weight = 0.5,
              group = "Bezirksvertreterwahl") %>% 
  addPolygons(data = shapes_trans_elect_labels$obwahl, 
              fillColor = ~colorNumeric("PiYG", turnout)(turnout),
              label = ~label,
              color = "green", 
              weight = 0.5,
              group = "Oberbürgermeisterwahl") %>% 
  addLayersControl(baseGroups = c("Ratswahl", "Bezirksvertreterwahl", "Oberbürgermeisterwahl"))
  
# htmlwidgets::saveWidget(m, "output/map_kommunalwahl.html")
