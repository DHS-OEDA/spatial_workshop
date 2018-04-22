
# Installing packages
# You need to get the development version of ggplot2. 
# And likely need to install the development version of rlang 
devtools::install_github("tidyverse/ggplot2", force=TRUE)
devtools::install_github("r-lib/rlang", force=TRUE)


# Loading packages
library(leaflet)
library(viridis)
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(spdep)

# Read in data
tracts <- read_sf("data/tl_2017_41_tract.shp")
class(tracts)
str(tracts)
tracts <- st_transform(tracts, crs = "+proj=longlat +datum=WGS84")

# Census API
api_key <- "986c9fe3c45cd70815e79cc04162f917ffdd5792" # replace with yours when you get one
tidycensus::census_api_key(api_key, overwrite = TRUE, install = TRUE)
options(tigris_use_cache = TRUE)
readRenviron("~/.Renviron")

# utility function that may help with package issues -----
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
# detachAllPackages() to run this

# static plot of our raw data
ggplot(tracts) + geom_sf()



# subset the sf object for the counties we're focusing on
mycounties <- c("005", "051", "067")
tracts %>% filter(COUNTYFP %in% mycounties) -> pdx
# check our subset
ggplot(pdx) + geom_sf()
# throw the polygons into a leaflet map
leaflet() %>% addPolygons(data=pdx) %>% addTiles()


# load in the variable names for the 2015 ACS (currently the most recent data for tidycensus)
acs_vars <- load_variables(2015, "acs5", cache = FALSE)

# get some relevant variables
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


# construct our dataset from the acs variables we pulled from the census
census_data <-
  pop %>% mutate(population = estimate) %>% mutate(name = NAME) %>%  select(GEOID, name, population)
group_quarters <-
  group_quarters %>% select(GEOID, estimate) %>% mutate(groupQuarters_population = estimate) %>% select(-estimate)
poverty_below <-
  poverty %>% filter(variable %in% 'B17001_002') %>% mutate(pop_below_fpl = estimate) %>% select(GEOID, pop_below_fpl)
poverty_above <-
  poverty %>% filter(variable %in% 'B17001_031') %>% mutate(pop_above_fpl = estimate) %>% select(GEOID, pop_above_fpl)

census_data <-
  census_data %>% left_join(group_quarters, by = "GEOID")
census_data <-
  census_data %>% left_join(poverty_below, by = "GEOID")
census_data <-
  census_data %>% left_join(poverty_above, by = "GEOID")
head(census_data)

# subset our census data to just the counties we're looking at
census_data$county <- substring(as.character(census_data$GEOID), 3, 5) # split the GEOID into substrings starting at position 3, ending at 5 (counties)
census_data <- census_data %>% filter(., county %in% mycounties) # filter for county in mycounties
head(census_data$county) # sanity check... looks right

pdx %>% left_join(census_data, by="GEOID") -> pdx2 # join our data with the sf object 

# quick ggplot2 check of the data
ggplot(pdx2, aes(fill=(pop_below_fpl/population))) + geom_sf() + scale_fill_viridis(option="B")


# quick ggplot2 check of the data
# make our proportion variable
pdx2$poverty_proportion <- pdx2$pop_below_fpl/pdx2$population
# remove no population area

pdx2 %>% filter(population !=0, !is.na(population)) -> pdx2

# hotspot analysis

pdx3 <- as(pdx2, "Spatial")
class(pdx3)
shp_nb <- poly2nb(pdx3, queen = T)
shp_nb_listw <- nb2listw(shp_nb, style = "W", zero.policy = TRUE)



pdx3@data$LocalG <- localG(x = pdx3@data$poverty_proportion, listw = shp_nb_listw, zero.policy = T)

convert.z.score<-function(z, one.sided=NULL) {
  if(is.null(one.sided)) {
    pval = pnorm(-abs(z));
    pval = 2 * pval
  } else if(one.sided=="-") {
    pval = pnorm(z);
  } else {
    pval = pnorm(-z);                                                                                 
  }
  return(pval);
}   

pdx3@data$LocalG_p <- convert.z.score(pdx3@data$LocalG, one.sided = FALSE)

p.adjust(pdx3@data$LocalG_p, method = "fdr", n=length(pdx3@data$LocalG_p))

pdx3@data$LocalG_p_fdr <- p.adjust(pdx3@data$LocalG_p, method = "fdr", n=length(pdx3@data$LocalG_p))
pdx3@data$LocalG_category <- ifelse(pdx3@data$LocalG_p <= .01, "99% Confidence", ifelse(pdx3@data$LocalG_p <= .05 & pdx3@data$LocalG_p > .01, "95% Confidence",
                                                                                        ifelse(pdx3@data$LocalG_p <=.1 & pdx3@data$LocalG_p >= .05, "90% Confidence", "Not Significant")))

pdx3@data$LocalG_fdr_category <- ifelse(pdx3@data$LocalG_p_fdr <= .01,
                                        "99% Confidence",
                                        ifelse(pdx3@data$LocalG_p_fdr <= .05 & pdx3@data$LocalG_p_fdr > .01,
                                               "95% Confidence",
                                               ifelse(pdx3@data$LocalG_p_fdr <=.1 & pdx3@data$LocalG_p_fdr >= .05, "90% Confidence", "Not Significant")))



pdx3@data$high_groupQuarters <-
  factor(
    ifelse(
      pdx3@data$groupQuarters_population / pdx3@data$population >=
        .2,
      "Yes",
      "No"
    )
  )
pdx3@data$poverty_percent <-
  round((
    pdx3@data$pop_below_fpl / (
      pdx3@data$pop_above_fpl + pdx3@data$pop_below_fpl
    )
  ) * 100, 2)



# new color palettes

pop_pal <-
  colorNumeric(palette = "viridis", domain = pdx3@data$population)
pop_gq_pal <-
  colorFactor("Set1", pdx3@data$high_groupQuarters)
pov_pal <-
  colorQuantile(palette = "viridis", pdx3@data$poverty_percent)
localG_pal <- colorFactor(palette = "plasma",
                          domain = factor(pdx3@data$LocalG_fdr_category))

# map it
leaflet(pdx3) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 1,
  fillOpacity = 0.5,
  fillColor = ~ pop_pal(pdx3@data$population),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  group = "Population"
) %>%
  addLegend(
    "bottomright",
    pal = pop_pal,
    values = ~ population,
    title = "Population (ACS 5yr. Estimate, 2015)",
    opacity = 1
  )


# add in popups

labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  pdx3@data$name,
  pdx3@data$population
) %>% lapply(htmltools::HTML)

labels_gc <- sprintf(
  "<strong>%s</strong><br/>%g percent of population in group quarters",
  pdx3@data$name,
  round(
    pdx3@data$groupQuarters_population / pdx3@data$population,
    2
  ) * 100
) %>% lapply(htmltools::HTML)

labels_pov <- sprintf(
  "<strong>%s</strong><br/>%g percent of population below FPL",
  pdx3@data$name,
  pdx3@data$poverty_percent
) %>% lapply(htmltools::HTML)


labels_localG <- sprintf(
  "<strong>%s</strong><br>Poverty hotspot: %s", 
  pdx3@data$name, 
  pdx3@data$LocalG_fdr_category
) %>% lapply(htmltools::HTML)


leaflet(pdx3) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~ pop_pal(pdx3@data$population),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    ),
    group = "Population"
  ) %>%
  addLegend(
    "bottomright",
    pal = pop_pal,
    values = ~ population,
    title = "Population (ACS 5yr. Estimate, 2015)",
    opacity = 1
  )

# combine it all into a single map

leaflet(pdx3) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 1,
  fillOpacity = 0.5,
  fillColor = ~ pop_pal(pdx3@data$population),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  ),
  group = "Population"
) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~ pov_pal(pdx3@data$poverty_percent),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
      
    ),
    label = labels_pov,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
    ,
    group = "Percent Poverty"
  ) %>%
  addPolygons(
    color = "#444444",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1,
    fillOpacity = 0.5,
    fillColor = ~ localG_pal(pdx3@data$LocalG_fdr_category),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
      
    ),
    label = labels_localG,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
    ,
    group = "Poverty Hotspots"
  ) %>% 
  addLayersControl(
    baseGroups = c("Population",
                   # "Group Quarters",
                   "Percent Poverty",
                   "Poverty Hotspots"),
    options = layersControlOptions(collapsed = FALSE)
  )
