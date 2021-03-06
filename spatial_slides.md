Spatial data visualization with R and Friends
========================================================
author:Aaron C Cochran 
date: 04-10-2018
autosize: true


Download class materials
======================================================

- Download the zip and extract it in a folder you're going to use

www.github.com/DHS-OEDA/spatial_workshop

![](img/github_download.png)

Creating the project directory
=======================================================

File >> New Project >> Existing directory >> point it to your directory


Installing packages
========================================================


```r
install.packages("devtools")

devtools::install_github("r-lib/rlang", force = TRUE)

library(rlang) # important to load this before installing dev version of ggplot2

devtools::install_github("tidyverse/ggplot2", force=TRUE) # development version!

devtools::install_github("ropensci/plotly") # development version of plotly

install.packages(c("leaflet", "viridis", "sf","sp", "tidyverse", "spdep", "rgdal"))
```


Maps in R
========================================================

Many options for maps
* ggplot2 - static maps
* ggplotly - interactive maps using plotly.js, built on D3.js
* mapview - simplified leaflet
* leaflet - presentation-grade maps


Today's workshop
========================================================

1. Introduce Leaflet 
2. Demo leaflet & ggplot2 (`1_Leaflet.Rmd` & `1_Leaflet.html`)
3. Hotspot analysis lab

> All materials will remain available on the course's github site:

>>github.com/DHS-OEDA/spatial_workshop

