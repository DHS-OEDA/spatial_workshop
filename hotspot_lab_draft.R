# Installing packages
# You need to get the development version of ggplot2. 
# And likely need to install the development version of rlang 
install.packages("devtools")
devtools::install_github("r-lib/rlang", force=TRUE) # do first
library(rlang) # load the library before installing ggplot2 development version
devtools::install_github("tidyverse/ggplot2", force=TRUE) # now install this
install.packages("leaflet")
install.packages("viridis")
install.packages("tidyverse")
install.packages("sf")
install.packages("tidycensus")
install.packages("spdep")

# Note: Linux users may need to install additional 
# libraries to get rgdal, sf, and other spatial packages to work


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
# Transform the crs (projection and datum)
tracts <- st_transform(tracts, crs = "+proj=longlat +datum=WGS84")

# Census API ####################################################
# This section commented out for the workshop. You can
# save your own copy of the census API key here and use the tidycensus
# package to pull census data on your own. 

# api_key <- "" # replace with yours when you get one
# tidycensus::census_api_key(api_key, overwrite = TRUE, install = TRUE)
# readRenviron("~/.Renviron") # run after saving the key to read it in to the current
# R session. 
######################################################################################


options(tigris_use_cache = TRUE)
######################################################################################
# utility function that may help with dependency issues -----
# this will remove all but the base R packages so you can load them in a proper order
# putting rlang before ggplot2, and then loading the rest

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
# detachAllPackages() to run this
#####################################################################################
# static plot of our raw data
ggplot(tracts) + geom_sf()



# subset the sf object for the counties we're focusing on
mycounties <- c("005", "051", "067") # using FIPS codes for the counties that make up PDX 
tracts %>% filter(COUNTYFP %in% mycounties) -> pdx
# check our subset of the data
ggplot(pdx) + geom_sf()
# throw the polygons into a leaflet map
leaflet() %>% addPolygons(data=pdx) %>% addTiles()

####################################################################################################
# This section is commented out for the workshop to avoid everyone downloading data from the census API
# at the same time. When you revisit this script, remove the comment (#) and run this section with
# your own census API key to pull the data, and then join and merge it. 


## load in the variable names for the 2015 ACS (currently the most recent data for tidycensus)
# acs_vars <- load_variables(2015, "acs5", cache = FALSE)

# get some relevant variables
# pop <-
#   get_acs(
#     geometry = FALSE,
#     geography = "tract",
#     state = "OR",
#     variables = "B01003_001E"
#   ) # total population
# 
# group_quarters <-
#   get_acs(
#     geometry = FALSE,
#     geography = "tract",
#     state = "OR",
#     variables = "B26001_001E"
#   ) # group quarters population
# 
# pov_vars <- c("B17001_002E", "B17001_031E")
# 
# poverty <-
#   get_acs(
#     geometry = FALSE,
#     geography = "tract",
#     state = "OR",
#     variables = pov_vars
#   )
# 
# 
# # construct our dataset from the acs variables we pulled from the census
# census_data <-
#   pop %>% mutate(population = estimate) %>% mutate(name = NAME) %>%  select(GEOID, name, population)
# group_quarters <-
#   group_quarters %>% select(GEOID, estimate) %>% mutate(groupQuarters_population = estimate) %>% select(-estimate)
# poverty_below <-
#   poverty %>% filter(variable %in% 'B17001_002') %>% mutate(pop_below_fpl = estimate) %>% select(GEOID, pop_below_fpl)
# poverty_above <-
#   poverty %>% filter(variable %in% 'B17001_031') %>% mutate(pop_above_fpl = estimate) %>% select(GEOID, pop_above_fpl)
# 
# census_data <-
#   census_data %>% left_join(group_quarters, by = "GEOID")
# census_data <-
#   census_data %>% left_join(poverty_below, by = "GEOID")
# census_data <-
#   census_data %>% left_join(poverty_above, by = "GEOID")
# head(census_data)
# 
# write_csv(census_data, 'data/census_data.csv')
###################################################################################################

# READ IN CENSUS DATA
# For the workshop, I've already run the past lines of code up to line 135
# So all you need to do is load in this .csv of the prepared data. 

census_data <- read_csv('data/census_data.csv')

# Data exploration
str(census_data)
head(census_data)

# fields are:
# GEOID - The ID field of the tract
# name - the name of the census tract
# population - ACS 5yr (2015) population of the tract
# groupQuarters_population - The population living in group quarters in the tract. 
# pop_below_fpl - The number of people living under the 2015 Federal Poverty Limit
# pop_above_fpl - The number of people living above the 2015 Federal Poverty Limit 

##############################################################################################
# Spatial analysis of data------
# subset our census data to just the counties we're looking at
census_data$county <- substring(as.character(census_data$GEOID), 3, 5) # split the GEOID into substrings starting at position 3, ending at 5 (counties)
census_data <- census_data %>% filter(., county %in% mycounties) # filter for county in mycounties
table(census_data$county) # sanity check... looks right

# convert our GEOID field in census_data to character instead of numeric
# to make it match with the GEOID field in pdx
census_data$GEOID <- as.character(census_data$GEOID)

pdx %>% left_join(census_data, by="GEOID") -> pdx2 # join our data with the sf object 

# quick ggplot2 check of the data
ggplot(pdx2, aes(fill=(pop_below_fpl/population))) + geom_sf() + scale_fill_viridis(option="B")
# note: you can view this image in it's own window by using the export button
# this will help you see it better on small screen devices



# make our proportion variable
pdx2$poverty_proportion <- pdx2$pop_below_fpl/pdx2$population
# remove no population area

pdx2 %>% filter(population !=0, !is.na(population)) -> pdx2

# hotspot analysis using the spdep package
# First, we need to convert the new sf object into the older sp object
# because spdep hasn't updated to the new standard yet.
# For this we use the as() function. And for the sake of keeping our files separated, 
# we'll make a new pdx3 file. 

pdx3 <- as(pdx2, "Spatial")
class(pdx3) # it's a sp object

# Create neighbors list using poly2nb. 

shp_nb <- poly2nb(pdx3, queen = T)
# now assign the spatial weights for the neighbors list
shp_nb_listw <- nb2listw(shp_nb, style = "W", zero.policy = TRUE)
# assign the localG value to the data of the sp object
# note that sp objects are different than sf objects
# and the data is stored as a level of a list instead of a data.frame
# This is why you have to access it by using pdx3@data$VARIABLENAME.
pdx3@data$LocalG <- localG(x = pdx3@data$poverty_proportion, listw = shp_nb_listw, zero.policy = T)

###########################################################################################
# function to convert z-scores to p-values
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

# use our custom function to convert the z-scores to p-values
pdx3@data$LocalG_p <- convert.z.score(pdx3@data$LocalG, one.sided = FALSE)

# false discovery rate correction
p.adjust(pdx3@data$LocalG_p, method = "fdr", n=length(pdx3@data$LocalG_p))

# save the FDR corrected scores here
pdx3@data$LocalG_p_fdr <- p.adjust(pdx3@data$LocalG_p, method = "fdr", n=length(pdx3@data$LocalG_p))
# now we create categorical variables for those values
pdx3@data$LocalG_category <- ifelse(pdx3@data$LocalG_p <= .01, "99% Confidence", ifelse(pdx3@data$LocalG_p <= .05 & pdx3@data$LocalG_p > .01, "95% Confidence",
                                                                                        ifelse(pdx3@data$LocalG_p <=.1 & pdx3@data$LocalG_p >= .05, "90% Confidence", "Not Significant")))

pdx3@data$LocalG_fdr_category <- ifelse(pdx3@data$LocalG_p_fdr <= .01,
                                        "99% Confidence",
                                        ifelse(pdx3@data$LocalG_p_fdr <= .05 & pdx3@data$LocalG_p_fdr > .01,
                                               "95% Confidence",
                                               ifelse(pdx3@data$LocalG_p_fdr <=.1 & pdx3@data$LocalG_p_fdr >= .05, "90% Confidence", "Not Significant")))

# High group quarters population tracts
# pdx3@data$high_groupQuarters <-
#   factor(
#     ifelse(
#       pdx3@data$groupQuarters_population / pdx3@data$population >=
#         .2,
#       "Yes",
#       "No"
#     )
#   )
# create a percent poverty variable from our counts of population and pop below fpl
pdx3@data$poverty_percent <-
  round((
    pdx3@data$pop_below_fpl / (
      pdx3@data$pop_above_fpl + pdx3@data$pop_below_fpl
    )
  ) * 100, 2)


#######################################################################################
# Define color palettes based on our data
pop_pal <-
  colorNumeric(palette = "viridis", domain = pdx3@data$population)
# pop_gq_pal <-
#   colorFactor("Set1", pdx3@data$high_groupQuarters)
pov_pal <-
  colorQuantile(palette = "viridis", pdx3@data$poverty_percent)
localG_pal <- colorFactor(palette = "plasma",
                          domain = factor(pdx3@data$LocalG_fdr_category))


##### LEAFLET MAP ######################################################################
# map it in leaflet

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


# add in popups for the map when you hover over the tract
# note: we can include html in the sprintf statement and then 
# apply it using the htmltools package

labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  pdx3@data$name,
  pdx3@data$population
) %>% lapply(htmltools::HTML)

# labels_gc <- sprintf(
#   "<strong>%s</strong><br/>%g percent of population in group quarters",
#   pdx3@data$name,
#   round(
#     pdx3@data$groupQuarters_population / pdx3@data$population,
#     2
#   ) * 100
# ) %>% lapply(htmltools::HTML)

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
  ) %>% addLegend(
    "bottomright",
    pal = pop_pal,
    values = ~ population,
    title = "Population (ACS 5yr. Estimate, 2015)",
    opacity = 1,
    group = "Population"
  ) %>% 
  addLayersControl(
    baseGroups = c("Population",
                   # "Group Quarters",
                   "Percent Poverty",
                   "Poverty Hotspots"),
    options = layersControlOptions(collapsed = FALSE)
  )
