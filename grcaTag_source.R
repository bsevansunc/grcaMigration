# set up ------------------------------------------------------------------

options(stringsAsFactors = FALSE)

# Important! This script requires the most up-to-date (currently development)
# version of tidyverse, available using the following:

# devtools::install_github("hadley/tidyverse")

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
# DO NOT RUN!!!!


# This section generates dummy data to load onto movebank to extract a
# background of environmental variables.

# Dummy data are 1000 random samples. Samples are drawn from:
# - Winter dates (month November through February)
# - A bounding box within the latitude and longitude of all GRCA winter
#   observations
# - Only locations on land

# library(rgeos)
# 

# # Get background country data:
# 
# conus <- getData('GADM', country='USA', level=1) %>%
#   subset(!(NAME_1 %in% c('Alaska', 'Hawaii'))) %>%
#   gUnaryUnion
# 
# mexico <- getData('GADM', country='Mexico', level=0) %>%
#   gUnaryUnion
# 
# cuba <- getData('GADM', country='Cuba', level=0) %>%
#   gUnaryUnion
# 
# northAmerica <- do.call(bind, list(conus, mexico, cuba))
# 
# # Get extent:
# 
# winterExtent <- read_csv('grcaMigration-Ryder.csv') %>%
#   filter(month(timestamp) %in% c(1, 2, 11, 12)) %>%
#   select(`location-long`, `location-lat`) %>%
#   SpatialPoints(proj4string = CRS(proj4string(northAmerica))) %>%
#   extent %>%
#   extend(.5) %>%
#   as('SpatialPolygons')
# 
# # Define projection:
# 
# projection(winterExtent) <- proj4string(northAmerica)
# 
# # Crop land to winter extent and generate random points:
# 
# landCrop <- gIntersection(northAmerica,winterExtent, byid = T)
# 
# randomPts <- spsample(landCrop, 1000, type = 'random')
# 
# # Want to see what they look like?
# 
# ggplot(landCrop, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill = '#00cc44', color = 'black') +
#   geom_point(data = randomPts@coords %>%
#                tbl_df %>%
#                rename(long = x, lat = y) %>%
#                mutate(group = 1),
#              color = 'blue'
#   ) +
#   theme_bw()
# 
# # Add id fields and random dates:
# 
# randomPtFrame <- randomPts@coords %>%
#   tbl_df %>%
#   transmute(
#     tagId = 0,
#     birdId = 0,
#     date = sample(
#       seq(as.Date('2016-11-01'), as.Date('2017-02-01'), by = 'day'),
#       size = 1000,
#       replace = TRUE),
#     long = x,
#     lat = y
#   )
# 
# # Write to file to upload into movebank
# 
# write_csv(randomPtFrame, 'randomPtFrame.csv')

# read  and tidy data -----------------------------------------------------

# This code will tidy the data files used by the remainder of the script. These
# files include:
#
# gpsTagData: Tag location and time data. This was created by uploading the gps 
# file into movebank and downloading the data from there. Dummy data was 
# uploaded as described above.
#
# grcaTagBanding: Banding data associated with the tagged birds, downloaded from
# the Nestwatch database.
#
# env: Climatic and land cover variables downloaded from movebank. Data are 
# associated with the movebank tag data.
#
# lcLegend: Legend for the globCover land cover data.

# Function to read in the raw data:

readRaw <- function(fileName){
  readLoc <- 'data_raw'
  f <- paste0(fileName, '.csv')
  read_csv(
    paste(readLoc, f, sep = '/')
  )
}

# GRCA tag data, filtered by hdop, eRes, and sFix criteria:

gpsTagData <- readRaw('grcaMigration-Ryder') %>% 
  bind_rows(
    readRaw('grcaMigration-Ryder-3217451753114930912') %>%
      mutate(`individual-local-identifier` = '0000-00000')
  ) %>%
  transmute(
    eventId = `event-id`,
    tagId = `tag-local-identifier`,
    bandNumber = `individual-local-identifier`,
    date = as.Date(timestamp),
    long = `location-long`,
    lat = `location-lat`,
    hdop = `gps:hdop`,
    sats = comments
  ) %>%
  separate(sats, into = c('sFix', 'sAvail'), sep = '/') %>%
  select(-sAvail) %>%
  filter(is.na(hdop)|hdop <= 10,
         is.na(sFix)|sFix > 3)

# Get and format banding data:

grcaTagBanding <- readRaw('grcaTagBanding') %>%
  mutate(region = case_when(
    str_detect(str_sub(siteID, -3), 'MA') ~ 'MA',
    str_detect(str_sub(siteID, -3), 'CO') ~ 'CO',
    str_detect(str_sub(siteID, -3), 'GA') ~ 'GA',
    TRUE ~ 'DC'
  )) %>%
  mutate(region = ifelse(bandNumber == '2671-02025', 'GA', region)) %>%
  select(region, siteID:obs, bandNumber:fat)


# Get and format environmental data:

env <- readRaw('grcaMigration-Ryder-566073171027321591') %>%
  select(1, 14:18) %>%
  bind_rows(
    readRaw('grcaMigration-Ryder-3217451753114930912') %>%
      select(1, 14:17)
  ) %>%
  left_join(
    readRaw('grcaMigration-Ryder-6244935445491619139') %>% 
      select(1, 14:27) %>%
      bind_rows(
        readRaw('grcaMigration-Ryder-707658057072842817') %>% 
          select(1, 14:23)
      ), by = 'event-id') %>%
  transmute(
    eventId = `event-id`,
    globCoverLc = `GlobCover 2009 2009 Land-Cover Classification`,
    evi = `MODIS Land Terra Vegetation Indices 250m 16d Enhanced Vegetation Index`,
    ndvi = `MODIS Land Terra Vegetation Indices 250m 16d NDVI`,
    surfaceVeg = `NCEP NARR SFC Vegetation at Surface`,
    precip = `NCEP NARR SFC Total Precipitation at Surface (Accumulated)`,
    dewPt = `NCEP NARR FLX Dew Point Temperature at 2 m above Ground`,
    evaporation = `NCEP NARR SFC Evaporation at Surface (Accumulated)`,
    nightTemp = `MODIS Land Terra Surface Temp & Emissivity 1km 8d Land Surface Temperature Nighttime`,
    dayTemp = `MODIS Land Terra Surface Temp & Emissivity 1km 8d Land Surface Temperature Daytime`,
    pressure = `NCEP NARR SFC Pressure at Surface`,
    vWind10m = `NCEP NARR FLX V Wind at 10 m above Ground`,
    uWind10m = `NCEP NARR FLX U Wind at 10 m above Ground`,
    vWind30m = `NCEP NARR FLX V Wind at 30 m above Ground`,
    uWind30m = `NCEP NARR FLX U Wind at 30 m above Ground`
  )


# Definition data for globCover

lcLegend <- readRaw('globCover_legend') %>%
  transmute(
    globCoverLc = class,
    lc = str_trim(definition)
  ) %>%
  filter(globCoverLc %in% env$globCoverLc)

rm(readRaw, smartInstallAndLoad)

# Before you go: The philosophy here is to avoid creating a one-size-fits-all
# monster data frame. Instead we will treat most of the data in the global
# environment like a database -- joining and querying only when needed, but
# keeping the files separate and thus as tidy as can reasonably be expected.
