# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggmap)
library(maptools)
library(rgdal)
library(geosphere)


# read data ---------------------------------------------------------------

mData <-
  read_csv('Hackett-820.csv')


# subset to good records and rename fields --------------------------------

mData_sub <-
  mData %>%
  filter(
    is.na(`gps:hdop`)|`gps:hdop` <= 10,
    is.na(`gps:satellite-count`)|`gps:satellite-count` > 5) %>%
  transmute(
    id = `event-id`,
    dt = `timestamp`,
    long = `location-long`,
    lat = `location-lat`,
    speed = `ground-speed`,
    heading = heading
  )


# sample points to  one record (randomly chosen) per day ------------------

points <-
  mData_sub %>%
  mutate(
    y = year(dt),
    d = yday(dt)
  ) %>%
  group_by(y, d) %>%
  sample_n(1)


# plot records ------------------------------------------------------------

points %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point()

# spatial point clusters ---------------------------------------------------

# Make points spatial:

points_sp <-
  SpatialPointsDataFrame(
    points[,3:4], 
    data = points, 
    proj4string = CRS("+proj=longlat +datum=WGS84")
  )

# Calculate distance matrix and cluster:

hc <- 
  distm(points_sp) %>%
  as.dist() %>%
  hclust(method = 'complete')


# Set distance for unique cluster (in meters):

d <- 15000

# Add cluster id field to point layer:

points_sp$clust <- cutree(hc, h = d)

# Extract data and collect a sample point from each cluster:

point_clusters <-
  points_sp@data %>%
  group_by(clust) %>%
  sample_n(1)

# points to kml -----------------------------------------------------------

outName <- 'test5.kml'

SpatialPointsDataFrame(
  point_clusters[,3:4], 
  data = point_clusters, 
  proj4string = CRS("+proj=longlat +datum=WGS84")
) %>%
  writeOGR(
    dsn=outName,
    layer = "data",
    driver="KML"
  )


