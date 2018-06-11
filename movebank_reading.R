# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggmap)
library(maptools)
library(rgdal)
library(geosphere)


# points to line function rpubs.com/walerke/points_to_line ----------------

tracks <- points_spdf@data

makeLines <-
  function(tracks)
  {
    
    # Make into a spatial lines object
    
    spLines <-
      Line(coordinates(tracks)) %>%
      list %>%
      Lines(ID = 'x') %>%
      list %>%
      SpatialLines(
        proj4string = CRS(
          proj4string(point_clusters_sp)
        )
      )
    
    # Return spatial lines dataframe:
      
      SpatialLinesDataFrame(
        spLines,
        data = data.frame(z = 1),
        match.ID = FALSE
      )
  }

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
  sample_n(1) %>%
  ungroup

# Make spatial:

point_clusters_sp <-
  SpatialPointsDataFrame(
    point_clusters[,3:4], 
    data = point_clusters, 
    proj4string = CRS("+proj=longlat +datum=WGS84")
  )

# points to kml -----------------------------------------------------------

# Provide information for point tooltip:

descriptionOutput <-
  point_clusters %>%
  ungroup %>%
  transmute(
    dOut = paste(
      '<p><b>date:</b> ', as.Date(dt),'<br>',
      '<b>long:</b> ', long, '<br>',
      '<b>lat:</b> ', lat,
      '</p>')
  ) %>%
  .$dOut
  
# KML output of points

kmlPoints(
  point_clusters_sp,
  kmlfile = 'test_point.kml',
  icon = 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png',
  name = '',
  description = descriptionOutput)

 
# plotKML::kml(
#   point_clusters_sp, 
#   file = 'test.kml', 
#   shape = 'http://maps.google.com/mapfiles/kml/shapes/placemark_circle_highlight.png', 
#   alpha = 1, 
#   size = 1, 
#   color = "#ffffff")

# lines -------------------------------------------------------------------

makeLines(point_clusters_sp) %>%
  kmlLines(
    kmlfile = 'test_line.kml',
    col = '#ffff00',
    lwd = 2
  )





