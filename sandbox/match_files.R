# this file implements some different methods for assigning
# demographic data to San Francisco election precincts


# load and organize data #####

library(sf)
library(dplyr)
library(ggplot2)

precincts <- st_read(dsn = "data/sf_precinct_shp",
                     layer = "SF_DOE_Precincts_2017",
                     stringsAsFactors = FALSE) %>%
  st_transform(7132)

census <- st_read(dsn = "data/ca_census_shp", stringsAsFactors = FALSE) %>%
  filter(COUNTY == "075") %>%
  st_transform(7132) %>% # foot-unit projection designed for SF, so it's the one I'll use
  filter(lengths(st_within(st_centroid(.), precincts)) > 0) %>% # pulls uninhabited Farallon Islands
  mutate(test_const = 5,
         test_weight = 5,
         test_char = "char")

ggplot(census) + geom_sf()
ggplot(precincts) + geom_sf()
ggplot() + geom_sf(data = census) + geom_sf(data = precincts)

# function for weighted joins #####
# takes in two sf objects (polygons?) and returns the combination,
# but weighted by area for the specified variables (character vector)

add_areas <- function(col) {
  if(col %in% weighted) return(weighted.mean(col, area2))
  else if(length(unique(col)) == 1) return(unique(col))
  else return(NA)
  
}

# this is in the process of working?
# the join & weight is fine
# but the re-aggregation isn't quite working yet
# problems because you want to sum the weighted within each precinct, but keep the constants constant
weighted_left_join <- function(data1, data2, weighted) {
  x <- dplyr::mutate(data1, index1 = 1:nrow(data1),
                     area1 = as.numeric(sf::st_area(geometry)))
  y <- dplyr::mutate(data2, index2 = 1:nrow(data2),
                     area2 = as.numeric(sf::st_area(geometry)))
  both <- sf::st_intersection(x,y) %>%
    mutate(intersect_area = as.numeric(sf::st_area(geometry))) %>%
    group_by(index2) %>%
    mutate(summed_area = sum(intersect_area)) %>%
    ungroup() %>%
    mutate(overage = area2 - intersect_area) %>%
    mutate_at(weighted, function (x) x * .$intersect_area / .$overall_area)

}



# join areas #####

combined <- st_intersection(precincts2, census2) %>%
  mutate(area = as.numeric(st_area(geometry))) # units are US survey feet ^2

ggplot(combined) + geom_sf()
