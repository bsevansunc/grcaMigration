# set up ------------------------------------------------------------------

options(stringsAsFactors = FALSE)

# Smart installer will check list of packages that are installed, install any
# necessary package that is missing, and load the library:

smartInstallAndLoad <- function(packageVector){
  for(i in 1:length(packageVector)){
    package <- packageVector[i]
    if(!package %in% rownames(installed.packages())){
      install.packages(packageVector[i],repos="http://cran.rstudio.com/",
                       dependencies=TRUE)
    }
  }
  lapply(packageVector, library, character.only = TRUE)
}

# Load and potentially install libraries:

smartInstallAndLoad(c('sp', 'geosphere', 'stringr', 'tidyverse', 'ggplot2',
                      'lubridate', 'leaflet', 'raster', 'rgeos'))

select <- dplyr::select

# make dummy wintering data for movebank ----------------------------------

# This section generates dummy data to load onto movebank to extract a
# background of environmental variables.

# Dummy data are 1000 random samples. Samples are drawn from:
# - Winter dates (month November through February)
# - A bounding box within the latitude and longitude of all GRCA winter
#   observations
# - Only locations on land

# Get background country data:

conus <- getData('GADM', country='USA', level=1) %>%
  subset(!(NAME_1 %in% c('Alaska', 'Hawaii'))) %>%
  gUnaryUnion

mexico <- getData('GADM', country='Mexico', level=0) %>%
  gUnaryUnion

cuba <- getData('GADM', country='Cuba', level=0) %>%
  gUnaryUnion

northAmerica <- do.call(bind, list(conus, mexico, cuba))

# Get extent:

winterExtent <- read_csv('grcaMigration-Ryder.csv') %>%
  filter(month(timestamp) %in% c(1, 2, 11, 12)) %>%
  select(`location-long`, `location-lat`) %>%
  SpatialPoints(proj4string = CRS(proj4string(northAmerica))) %>%
  extent %>%
  extend(.5) %>%
  as('SpatialPolygons')

# Define projection:

projection(winterExtent) <- proj4string(northAmerica)

# Crop land to winter extent and generate random points:

landCrop <- gIntersection(northAmerica,winterExtent, byid = T)

randomPts <- spsample(landCrop, 1000, type = 'random')

# Want to see what they look like?

ggplot(landCrop, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = '#00cc44', color = 'black') +
  geom_point(data = randomPts@coords %>%
               tbl_df %>%
               rename(long = x, lat = y) %>%
               mutate(group = 1),
             color = 'blue'
  ) +
  theme_bw()

# Add id fields and random dates:

randomPtFrame <- randomPts@coords %>%
  tbl_df %>%
  transmute(
    tagId = 0,
    birdId = 0,
    date = sample(
      seq(as.Date('2016-11-01'), as.Date('2017-02-01'), by = 'day'),
      size = 1000,
      replace = TRUE),
    long = x,
    lat = y
  )

# Write to file to upload into movebank

# write_csv(randomPtFrame, 'randomPtFrame.csv')