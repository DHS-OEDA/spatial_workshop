# Leaflet Map Demo
library(tidyverse)
library(leaflet)
library(viridis) # better colors
library(rgdal)
library(rmapshaper)
library(microbenchmark)
library(tidycensus)
library(leaflet.extras)

# Read in shapefile using rgdal package
shp <-
  readOGR('shapes/tl_2017_41_tract.shp', GDAL1_integer64_policy = TRUE)

# basemap in Leaflet

m <- leaflet()
m %>% addTiles() # Default is OpenStreetMap

# third party tiles
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
# more options here http://leaflet-extras.github.io/leaflet-providers/preview/index.html

# also, you can add Web Map Service tiles.

m %>% addTiles() %>% setView(-93.65, 42.085, zoom = 4) %>% addWMSTiles(
  "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
  layers = "nexrad-n0r-900913",
  options = WMSTileOptions(format = "image/png", transparent = TRUE),
  attribution = "Weather data © 2012 IEM Nexrad"
)

# Lines and Shapes

# first initialize a color palette
pal <- colorQuantile(palette = "Blues",
                     domain = shp@data$ALAND)

pal_s <- colorQuantile(palette = "Blues",
                       domain = shp_s@data$ALAND)


leaflet(shp) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~ pal(shp@data$ALAND),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  )



leaflet(shp_s) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~ pal_s(shp_s@data$ALAND),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    )
  )




# performance consideration
# simplify the detailed shapefiles
# using rmapshaper package here


shp_s <- rmapshaper::ms_simplify(shp)

# slower method here

microbenchmark(
  leaflet(shp) %>%
    addPolygons(
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1,
      fillOpacity = 0.5,
      fillColor = ~ pal(shp@data$ALAND),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      )
    )
)

# faster with smaller shp

microbenchmark(
  leaflet(shp_s) %>%
    addPolygons(
      color = "#444444",
      weight = 1,
      smoothFactor = 0.5,
      opacity = 1,
      fillOpacity = 0.5,
      fillColor = ~ pal_s(shp_s@data$ALAND),
      highlightOptions = highlightOptions(
        color = "white",
        weight = 2,
        bringToFront = TRUE
      )
    )
)

# adding in our own data

acs_vars <- load_variables(2015, "acs5", cache = FALSE)

pop <-
  get_acs(
    geometry = FALSE,
    geography = "tract",
    state = "OR",
    variables = "B01003_001E"
  ) # total population

group_quarters <-
  get_acs(
    geometry = FALSE,
    geography = "tract",
    state = "OR",
    variables = "B26001_001E"
  ) # group quarters population

pov_vars <- c("B17001_002E", "B17001_031E")

poverty <-
  get_acs(
    geometry = FALSE,
    geography = "tract",
    state = "OR",
    variables = pov_vars
  )

# make a data.frame of our census data

census_data <-
  pop %>% mutate(population = estimate) %>% mutate(name = NAME) %>%  select(GEOID, name, population)

group_quarters <-
  group_quarters %>% select(GEOID, estimate) %>% mutate(groupQuarters_population = estimate) %>% select(-estimate)
poverty_below <-
  poverty %>% filter(variable %in% 'B17001_002') %>% mutate(pop_below_fpl = estimate) %>% select(GEOID, pop_below_fpl)
poverty_above <-
  poverty %>% filter(variable %in% 'B17001_031') %>% mutate(pop_above_fpl = estimate) %>% select(GEOID, pop_above_fpl)

census_data <- census_data %>% left_join(group_quarters, by = "GEOID")
census_data <- census_data %>% left_join(poverty_below, by = "GEOID")
census_data <- census_data %>% left_join(poverty_above, by = "GEOID")

head(census_data)


census_data$GEOID <- as.factor(census_data$GEOID)
# merge our data into the shapefile we're using with the map

shp_s_merged <-
  merge(shp_s, census_data, by.x = "GEOID", by.y = "GEOID")

shp_s_merged@data$high_groupQuarters <-
  factor(
    ifelse(
      shp_s_merged@data$groupQuarters_population / shp_s_merged@data$population >=
        .2,
      "Yes",
      "No"
    )
  )
shp_s_merged@data$poverty_percent <-
  round((
    shp_s_merged@data$pop_below_fpl / (
      shp_s_merged@data$pop_above_fpl + shp_s_merged@data$pop_below_fpl
    )
  ) * 100, 2)
# new color palettes

pop_pal <-
  colorNumeric(palette = "Blues", domain = shp_s_merged@data$population)
pop_gq_pal <-
  colorFactor(topo.colors(2), shp_s_merged@data$high_groupQuarters)
pov_pal <-
  colorQuantile(palette = "viridis", shp_s_merged@data$poverty_percent)

# map it
leaflet(shp_s_merged) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 1,
  fillOpacity = 0.5,
  fillColor = ~ pop_pal(shp_s_merged@data$population),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  group = "Population"
) %>% 
  addLegend("bottomright", pal = pop_pal, values = ~population, 
            title = "Population (ACS 5yr. Estimate, 2015)",
            opacity = 1)


# add in popups

labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  shp_s_merged@data$name, shp_s_merged@data$population
) %>% lapply(htmltools::HTML)

leaflet(shp_s_merged) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 1,
  fillOpacity = 0.5,
  fillColor = ~ pop_pal(shp_s_merged@data$population),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"), 
    textsize = "15px", direction = "auto"
  ),
  group = "Population"
) %>% 
  addLegend("bottomright", pal = pop_pal, values = ~population, 
            title = "Population (ACS 5yr. Estimate, 2015)",
            opacity = 1)

# combine it all into a single map

leaflet(shp_s_merged) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 1,
  fillOpacity = 0.5,
  fillColor = ~ pop_pal(shp_s_merged@data$population),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"), 
    textsize = "15px", direction = "auto"
  ),
  group = "Population"
) %>% 
  addPolygons(color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              fillOpacity = 0.5,
              fillColor = ~ pop_gq_pal(shp_s_merged@data$high_groupQuarters),
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
    
  ), group="Group Quarters") %>% 
  addPolygons(color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1,
              fillOpacity = 0.5,
              fillColor = ~ pov_pal(shp_s_merged@data$poverty_percent),
              highlightOptions = highlightOptions(
                color = "white",
                weight = 2,
                bringToFront = TRUE
                
              ), group="Percent Poverty") %>% 
  addLayersControl(
    baseGroups = c("Population", "Group Quarters", "Percent Poverty"),
    options = layersControlOptions(collapsed = FALSE)
  )
  
