library(sf)
library(dplyr)
library(ggplot2)

census <- st_read(dsn = "data/ca_census_shp", stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075")

precincts <- st_read(dsn = "data/sf_precinct_shp",
                     layer = "SF_DOE_Precincts_2017",
                     stringsAsFactors = FALSE)

ggplot(census2) + geom_sf()
ggplot(precincts2) + geom_sf()

# foot-unit projection designed for SF, so it's the one I'll use
census2 <- st_transform(census, 7132)
precincts2 <- st_transform(precincts, 7132)

combined <- st_intersection(census2, precincts2)
