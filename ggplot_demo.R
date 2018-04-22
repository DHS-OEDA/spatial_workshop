# static map demo in R
library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)


devtools::install_github("tidyverse/ggplot2")
require(ggplot2)

racevars <- c(White = "P0050003", 
              Black = "P0050004", 
              Asian = "P0050006", 
              Hispanic = "P0040003")

multnomah <- get_decennial(geography="tract", variables = racevars, state="OR", county = "Multnomah County", geometry=TRUE, summary_var = "P0010001")

head(multnomah)

labels <- c("P0050003" = "White", "P0050004" = "Black", "P0050006" = "Asian", "P0040003" = "Any Hispanic")

multnomah %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable, labeller=labeller(variable = labels)) +
  geom_sf() +
  coord_sf(crs = 26910) + 
  scale_fill_viridis(option = "C") +
  scale_color_viridis(option = "C") + 
  ggtitle('Select racial and ethnic demographics, Multnomah County')


