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

# make weighted join function #####

#helper function
combine <- function(x) {
  if (length(unique(x)) == 1) as.character(unique(x)) else "MULTI"
}

# actual function
# is there a good way to check the math on this?

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
    mutate_at(weighted, function (x) x * .$intersect_area / .$summed_area)
  
  weighted_df <- both %>%
    group_by(index1) %>%
    summarize_at(weighted, sum)
  unweighted_df <- both %>%
    group_by(index1) %>%
    summarize_at(vars(-one_of(c(weighted, "index1"))), combine)
  
  bind_cols(weighted_df, unweighted_df) %>%
    select(-geometry1, -index11, -index1) %>%
    st_sf() %>%
    select(PREC_2017, everything()) %>%
    arrange(PREC_2017)

}

big_guy <- weighted_left_join(precincts, census, c("CENSUSAREA", "test_weight"))

# test this??? #####
# turns out drawing arbitrary polygons is difficult

l1 <- rbind(c(0,0),c(0,6))
l2 <- rbind(c(0,6),c(6,6))
l3 <- rbind(c(6,6),c(6,0))
l4 <- rbind(c(6,0),c(0,0))

l5 <-rbind(c(3,0),c(3,6))
l6 <-rbind(c(0,3),c(6,3))
lines <- st_multilinestring(list(l1,l2,l3,l4,l5,l6))

box4 <- st_cast(lines, "POLYGON")



shape <- st_polygon(list(box1, box2, box3, box4))
