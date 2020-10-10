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
      "<b>Wahlbezirk {NUMMER}, {NAME}</b><br>
        {counted} von {stimmbezirke} Stimmbezirken ausgezählt<br>
        <br>
        <b>Wahlbeteiligung</b>: {turnout} %<br>
        <br>
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
      "<b>Wahlbezirk {NUMMER}, {NAME}</b><br>
        {counted} von {stimmbezirke} Stimmbezirken ausgezählt<br>
        <br>
        <b>Wahlbeteiligung</b>: {turnout} %<br>
        <br>
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
  "SPD", "#E3000F",
  "GRÜNE", rgb(100, 161, 45, maxColorValue = 255),
  "FDP", rgb(245, 228, 21, maxColorValue = 255),
  "DIE LINKE", "purple",
  "AfD", rgb(0, 158, 224, maxColorValue = 255),
  "Volt" = "grey30",
  "Sonstige" = "grey10"
) %>% 
  mutate(party = factor(party, levels = c("CDU", "SPD", "GRÜNE", "FDP", "DIE LINKE", "AfD", "Volt", "Sonstige")))

party_color_pal <- c(
  "SPD" = "#E3000F",
  "CDU" = "black",
  "GRUENE" = rgb(100, 161, 45, maxColorValue = 255),
  "DIE LINKE" = "purple",
  "FDP"  = rgb(245, 228, 21, maxColorValue = 255),
  "AfD" = rgb(0, 158, 224, maxColorValue = 255),
  "Volt" = "grey30",
  "Sonstige" = "grey10"
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

# for OB election set colors manually

shapes_trans_elect$obwahl <- shapes_trans_elect$obwahl %>% 
  mutate(color = case_when(
    party == "Reker, Einzelbewerberin" ~ "black",
    party == "Kossiski, SPD" ~ party_color_pal["SPD"],
    party == "Detjen, DIE LINKE" ~ party_color_pal["DIE LINKE"],
    party == "Cremer, AfD" ~ party_color_pal["AfD"]
  ))


shapes_trans_elect_labels <- list()
shapes_trans_elect_labels$ratswahl <- shapes_trans_elect$ratswahl %>% 
  mutate(label = pmap(list(
    NUMMER, NAME, counted, stimmbezirke, turnout, CDU, 
    SPD, GRÜNE, 
    FDP, 
    `DIE LINKE`, 
    AfD = NA, 
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



## Include vote share plots as popups ==============================================

library(leafpop)

# load fonts (import in advace via font_import if necessary)
library(extrafont)
loadfonts(quiet = TRUE)


create_plot <- function(df) {
  # get name of Wahlbezirk for plot title 
  wb_name <- df %>% slice(1) %>% pull(NAME)
  # get turnout for subtitle
  turnout <- df %>%
    pull(turnout)
  turnout <- scales::percent(turnout / 100, accuracy = 0.1)
  
  # transform dataframe
  df <- df %>%
    tibble() %>%
    select(-OBJECTID, -NUMMER, -LINK:-eligible, -valid, -party, -share, -color, -label, -geometry) %>% 
    pivot_longer(cols = c(-NAME, -turnout),
                 names_to = "party",
                 values_to = "share"
    ) %>%
    left_join(party_colors, by = "party") %>%
    # leafpop seems to have an issue with special characters
    mutate(party = str_replace(party, "\U00DC", "UE"),
           party = fct_inorder(party),
           NAME = str_replace(NAME, "\U00FC", "ue")
           ) 
  
  # limit candidates shown for OB election
  items <- distinct(df, party) %>% pull(party)
  if ("Reker, Einzelbewerberin" %in% items) {
    df <- df %>% 
      filter(party %in% c("Kossiski, SPD", "Detjen, DIE LINKE", "Reker, Einzelbewerberin", "Cremer, AfD")) %>% 
      mutate(party = fct_rev(str_replace(party, "Einzelbewerber(in)?", "Einzelb.")))
  }
  
  p <- df %>%
    ggplot(aes(fct_rev(party), share)) +
    geom_col(aes(fill = party), width = 0.25, show.legend = FALSE) +
    geom_text(aes(label = scales::percent(share / 100, accuracy = 0.1)),
              hjust = 0, nudge_y = 2, size = 3, color = "grey25", family = "Open Sans Light") +
    coord_flip(ylim = c(0, 60)) +
    labs(title = wb_name,
         subtitle = glue::glue("Wahlbeteiligung: {turnout}") ) +
    theme_void(base_family = "Open Sans Light") +
    theme(axis.text.y = element_text(hjust = 0, size = 9),
          text = element_text(color = "grey25"),
          plot.margin = margin(t = 4, r = 16, b = 6, l = 8),
          plot.title = element_text(family = "Open Sans SemiBold"),
          plot.subtitle = element_text(margin = margin(t = 4, b = 8)),
          plot.title.position = "plot"
    )
  
  if ("Reker, Einzelbewerberin" %in% items) {
    # pick the relevant party colors
    p <- p + 
      scale_fill_manual(values = unname(party_color_pal[c("CDU", "SPD", "DIE LINKE", "AfD")]))
  } else {
    p <- p + scale_fill_manual(values = party_color_pal)
  }
  
  # return plot in a list 
  list(p)
}

shapes_trans_elect_plots <- shapes_trans_elect
for (i in 1:nrow(shapes_trans_elect$ratswahl)) {
  shapes_trans_elect_plots$ratswahl$plot[i] <- create_plot(shapes_trans_elect_labels$ratswahl[i, ]) 
  shapes_trans_elect_plots$bezirkswahl$plot[i] <- create_plot(shapes_trans_elect_labels$bezirkswahl[i, ])
  shapes_trans_elect_plots$obwahl$plot[i] <- create_plot(shapes_trans_elect_labels$obwahl[i, ]) 
}


# create base map
m_base <- leaflet(options = leafletOptions(worldCopyJump = FALSE, dragging = TRUE)) %>%
  addTiles() %>% 
  addProviderTiles(options = tileOptions(noWrap = TRUE), 
                   provider = "CartoDB", 
                   group = "CartoDB") %>%
  setView(lng = coords_cathedral[1, ]$lon,
          lat = coords_cathedral[1, ]$lat,
          zoom = 11) %>%
  # limit map bounds to Cologne area
  setMaxBounds(lng1 = coords_cgn[1,1], lng2 = coords_cgn[1,2], lat1 = coords_cgn[2,1], lat2 = coords_cgn[2,2])


# add layers with election results
m <- m_base %>% 
  addPolygons(data = shapes_trans_elect_plots$ratswahl,
              fillColor = ~color,
              popup = ~popupGraph(plot, width = 150, height = 160, type = "svg"),
              color = "#777777", 
              weight = 0.5,
              group = "Ratswahl") %>% 
  addPolygons(data = shapes_trans_elect_plots$bezirkswahl,
              fillColor = ~color,
              popup = ~popupGraph(plot, width = 150, height = 160, type = "svg"),
              color = "#777777", 
              weight = 0.5,
              group = "Bezirksvertretungswahl") %>% 
  addPolygons(data = shapes_trans_elect_plots$obwahl,
              fillColor = ~color,
              popup = ~popupGraph(plot, width = 240, height = 150, type = "svg"),
              color = "#777777", 
              weight = 0.5,
              group = "Oberbürgermeisterwahl") %>% 
  addLayersControl(baseGroups = c("Ratswahl", "Bezirksvertretungswahl", "Oberbürgermeisterwahl"),
                   overlayGroups = c("Wahlbeteiligung"),
                   options = layersControlOptions(collapsed = FALSE))
