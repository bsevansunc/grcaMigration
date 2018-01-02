#=================================================================================*
# ---- SET-UP ----
#=================================================================================*

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

#=================================================================================*
# ---- MAKE DUMMY WINTERING DATA FOR MOVEBANK ----
#=================================================================================*
# DO NOT RUN!!!!

# This section generates dummy data to load onto movebank to extract a background
# of environmental variables.

# Dummy data are 1000 random samples. Samples are drawn from:
# - Winter dates (month November through February) 
# - A bounding box within the latitude and longitude of all GRCA winter observations
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

#=================================================================================*
# ---- GET AND FORMAT DATA ----
#=================================================================================*
# Log in to movebank. Username is evansbr. Password is 2358Shazam!
# Go to Tracking Data Map -> Studies
# For tag data just use the Download dropdown
# For environmental data use Env-Data -> Show My Requests -> download

# They will download as zip files, so extract the files into whatever working
# directory you plan to use.

# This code will create the data files used by the remainder of the script. These
# files include:
#
# gpsTagData: Tag location and time data. This was created by uploading the gps
#    file into movebank and downloading the data from there. Dummy data was
#    uploaded as described above.
#
# grcaTagBanding: Banding data associated with the tagged birds, downloaded from
#    the Nestwatch database.
#
# env: Climatic and land cover variables downloaded from movebank. Data are 
#    associated with the movebank tag data.
#
# lcLegend: Legend for the globCover land cover data.

# Make sure your working directory is set to the location of your files!

# GRCA tag data, filtered by hdop, eRes, and sFix criteria:

gpsTagData <- read_csv('grcaMigration-Ryder.csv') %>% 
  bind_rows(
    read_csv('grcaMigration-Ryder-3217451753114930912.csv') %>%
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

grcaTagBanding <- read_csv('grcaTagBanding.csv') %>%
  mutate(region = case_when(
    str_detect(str_sub(siteID, -3), 'MA') ~ 'MA',
    str_detect(str_sub(siteID, -3), 'CO') ~ 'CO',
    str_detect(str_sub(siteID, -3), 'GA') ~ 'GA',
    TRUE ~ 'DC'
  )) %>%
  mutate(region = ifelse(bandNumber == '2671-02025', 'GA', region)) %>%
  select(region, siteID:obs, bandNumber:fat)


# Get and format environmental data:

env <- read_csv('grcaMigration-Ryder-566073171027321591.csv') %>%
  select(1, 14:18) %>%
  bind_rows(
    read_csv('grcaMigration-Ryder-3217451753114930912.csv') %>%
      select(1, 14:17)
  ) %>%
  left_join(
    read_csv('grcaMigration-Ryder-6244935445491619139.csv') %>% 
      select(1, 14:27) %>%
      bind_rows(
        read_csv('grcaMigration-Ryder-707658057072842817.csv') %>% 
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

lcLegend <- read_csv('globCover_legend.csv') %>%
  transmute(
    globCoverLc = class,
    lc = str_trim(definition)
  ) %>%
  filter(globCoverLc %in% env$globCoverLc)

# Before you go: The philosophy here is to avoid creating a one-size-fits-all
# monster data frame. Instead we will treat most of the data in the global
# environment like a database -- joining and querying only when needed, but 
# keeping the files separate and thus as tidy as can reasonably be expected.

#=================================================================================*
# ---- DISTANCE TO BREEDING GROUNDS AND MIGRATION ----
#=================================================================================*
# Here, we will categorize points as:
#   - B: Breeding grounds -- Movements within 1 km of the capture location
#   - M: Migrating -- Points not classified as B or W
#   - W: Wintering -- Movements within 95% of total distance from the breeding
#        location
# The output of this section is a data frame named grcaMigration.

# Unique vector of tagId's

tagIds <- unique(gpsTagData$tagId[gpsTagData$tagId != 0])

# Calculate the distance from the first gps observation:

grcaList <- vector('list', length = length(tagIds))

for(i in 1:length(tagIds)){
  # Filter to a single tagId:
  idSubset <- gpsTagData %>%
    filter(tagId == tagIds[i])
  # Spatial point of capture location:
  ptOrigin <- idSubset %>%
    filter(date == min(date)) %>%
    select(long, lat) %>%
    distinct %>%
    SpatialPoints(proj4string = CRS('+proj=longlat'))
  # Spatial points of gps fixes:
  idSubsetSpatial <- idSubset %>%
    select(long, lat) %>%
    SpatialPoints(proj4string = CRS('+proj=longlat'))
  # Add column of distances:
  idSubset$distBreedingKm <- distGeo(ptOrigin, idSubsetSpatial)/1000
  # Output distances:
  grcaList[[i]] <- idSubset
}

# Bind list and code migratory stage:

grcaMigration <- bind_rows(grcaList) %>%
  arrange(tagId, date)  %>%
  select(eventId:lat, distBreedingKm) %>%
  group_by(tagId) %>%
  mutate(
    mStage = case_when(
      date < min(date[distBreedingKm >= 10]) ~ 'B',
      date >= min(date[distBreedingKm/max(distBreedingKm) > 0.95]) ~ 'W',
      TRUE ~ 'M'
    )
  ) %>%
  ungroup

#=================================================================================*
# ---- GET PATH DISTANCES ALONG MIGRATION ROUTE ----
#=================================================================================*
# This section of the script will calculate the euclidean and path distance from
# breeding to wintering points. The output is a dataframe named grcaPathDistance

grcaMigrationList <- vector('list', length = length(tagIds))

for(i in 1:length(tagIds)){
  # Subset frame to a single tagId:
  grcaIdSubset <- grcaMigration %>%
    filter(tagId == tagIds[i]) %>%
    filter(
      mStage == 'M'|
        date == max(date[mStage == 'B'])|
        date == min(date[mStage == 'W'])
    ) %>%
    arrange(date)
  # Get origin and end points:
  originPt <- grcaIdSubset %>%
    slice(1) %>%
    select(long, lat) %>%
    SpatialPoints(proj4string = CRS('+proj=longlat'))
  endPt <- grcaIdSubset %>%
    slice(nrow(grcaIdSubset)) %>%
    select(long, lat) %>%
    SpatialPoints(proj4string = CRS('+proj=longlat'))
  # Get Euclidean distance between origin and end points (in km):
  grcaIdSubset$tDist <- distGeo(originPt, endPt)/1000
  # Get path distance between origin and end points (in km):
  grcaIdSubset$pointDist <- 0
  for(j in 2:nrow(grcaIdSubset)){
    originPt <- grcaIdSubset %>%
      slice(j - 1) %>%
      select(long, lat) %>%
      SpatialPoints(proj4string = CRS('+proj=longlat'))
    targetPt <- grcaIdSubset %>%
      slice(j) %>%
      select(long, lat) %>%
      SpatialPoints(proj4string = CRS('+proj=longlat'))
    grcaIdSubset$pointDist[j] <- distGeo(originPt, targetPt)/1000
  }
  grcaMigrationList[[i]] <- grcaIdSubset
}

# Bind list and provide as summary frame:

grcaPathDistance <- bind_rows(grcaMigrationList) %>%
  group_by(tagId) %>%
  summarize(
    tDistance = unique(tDist),
    pathDistance = sum(pointDist),
    pathSinuosity = pathDistance/tDistance,
    nPoints = n()
  )

#=================================================================================*
# ---- SUMMARIZING MIGRATION DATA BY TAG ID ----
#=================================================================================*
# This section creates a summary frame of GRCA data.

grcaSummary <- grcaMigration %>%
  # Migration data:
  group_by(tagId) %>%
  mutate(
    estDeparture = max(date[mStage == 'B']) + 1,
    estArrival = min(date[distBreedingKm/max(distBreedingKm) > 0.95]) - 1,
    nMigrationDays = difftime(estArrival, estDeparture,
                              units = 'days') %>% as.numeric,
    breedingLat = unique(lat[date == min(date)]),
    winterLat = mean(lat[mStage == 'W'])
  ) %>%
  # Add banding region:
  left_join(grcaTagBanding %>%
              select(bandNumber, region), by = 'bandNumber') %>%
  # Path data:
  left_join(grcaPathDistance, by = 'tagId') %>%
  select(
    tagId, region, bandNumber, estDeparture, estArrival,
    nMigrationDays, winterLat,breedingLat, tDistance,
    pathDistance, pathSinuosity, nPoints
  ) %>%
  distinct

#=================================================================================*
# ---- MAP BY INDIVIDUAL ----
#=================================================================================*

# Set colors for birds:

colorPalette <- colorFactor(
  palette = rainbow(length(tagIds)),
  domain = gpsTagData$tagId
)

# Lines for each bird:

makeLines <- function(dataframeIn, colorMatch){
  # Unique IDs:
  birdIDs <- unique(dataframeIn$tagId)
  # List of data frames, one per ID:
  tracksList <- vector('list', length = length(birdIDs))
  for(i in 1:length(tracksList)){
    tracksList[[i]] <- dataframeIn %>%
      filter(tagId == birdIDs[i])
  }
  # List of lines objects (one list item per tagId):
  birdLines <- vector('list', length = length(birdIDs))
  for(i in 1:length(tracksList)){
    coordMat <- tracksList[[i]] %>%
      select(long, lat) %>%
      as.matrix
    birdLines[[i]] <- Lines(Line(coordMat),
                            ID = tracksList[[i]] %>%
                              select(tagId) %>%
                              unique)
  }
  # Combine lines list to spatial lines data frame:
  SpatialLinesDataFrame(SpatialLines(birdLines),
                        data = dataframeIn %>%
                          select(tagId) %>%
                          distinct,
                        match.ID = FALSE
  )
}

# Make basemap:

grcaBasemap <- leaflet(data = gpsTagData %>% filter(tagId != 0),
                       options = leafletOptions(zoomControl = FALSE)) %>%
  # Background of lines, for visibility:
  addPolylines(
    data = makeLines(gpsTagData %>% filter(tagId != 0)),
    weight = 5,
    opacity = .3,
    color = 'black'
  )

# Add colored lines and points:

grcaPointLine <- grcaBasemap %>%
  # Lines, colored by tagId:
  addPolylines(
    data = makeLines(gpsTagData %>% filter(tagId != 0)),
    weight = 3, dashArray = "5, 5",
    opacity = 1,
    color = ~colorPalette(tagId)
  ) %>%
  # Circle markers, colored by tagId:
  addCircleMarkers(~long, ~lat,
                   radius = ~4,
                   color = 'white',
                   fillColor = ~colorPalette(tagId),
                   stroke = TRUE,
                   popup = ~paste(
                     sep = "<br/>",
                     paste0('<b style="color:#0000FF">', tagId, '</b>'),
                     paste0("<b> Band number: </b>", bandNumber),
                     paste0("<b> Date: </b>", date),
                     paste0("<b> Longitude: </b>", round(long, 4)),
                     paste0("<b> Latitude: </b>", round(lat, 4))
                   ),
                   fillOpacity = 0.9
  )

# Show map, National Geographic background:

grcaPointLine %>%
  addProviderTiles('Esri.NatGeoWorldMap')

# Show map, aerial photo background:

grcaPointLine %>%
  addProviderTiles("Esri.WorldImagery")

#=================================================================================*
# ---- MAP BY INDIVIDUAL ----
#=================================================================================*

# Add region to tagging data:

gTaggingWithRegion <- left_join(
  grcaMigration,
  grcaTagBanding %>%
    select(bandNumber, region),
  by = 'bandNumber'
)

# Regional color palette:

colorPaletteRegion <- colorFactor(
  palette = c('#8A2BE2', 'blue', 'yellow', 'red'),
  domain = gTaggingWithRegion$region
)

grcaRegionalBasemap <- leaflet(data = gTaggingWithRegion,
                               options = leafletOptions(zoomControl = FALSE)) %>%
  # Background of lines, for visibility:
  addPolylines(
    data = makeLines(gTaggingWithRegion),
    weight = 5,
    opacity = .3,
    color = 'black'
  ) %>%
  addPolylines(
    data = makeLines(
      gTaggingWithRegion %>%
        filter(region == 'CO')
    ),
    weight = 3, dashArray = "5, 5", 
    opacity = 1,
    color = '#8A2BE2'
  ) %>%
  addPolylines(
    data = makeLines(
      gTaggingWithRegion %>%
        filter(region == 'DC')
    ),
    weight = 3, dashArray = "5, 5",
    opacity = 1,
    color = 'blue'
  )  %>%
  addPolylines(
    data = makeLines(
      gTaggingWithRegion %>%
        filter(region == 'MA')
    ),
    weight = 3, dashArray = "5, 5",
    opacity = 1,
    color = 'red'
  ) %>%
  addPolylines(
    data = makeLines(
      gTaggingWithRegion %>%
        filter(region == 'GA')
    ),
    weight = 3, dashArray = "5, 5",
    opacity = 1,
    color = 'yellow'
  ) %>%
  addCircleMarkers(~long, ~lat,
                   radius = ~3,
                   color = 'white',
                   fillColor = ~colorPaletteRegion(region),
                   stroke = TRUE,
                   popup = ~paste(
                     sep = "<br/>",
                     paste0('<b style="color:#0000FF">', tagId, '</b>'),
                     paste0("<b> Band number: </b>", bandNumber),
                     paste0("<b> Date: </b>", date),
                     paste0("<b> Longitude: </b>", round(long, 4)),
                     paste0("<b> Latitude: </b>", round(lat, 4))
                   ),
                   fillOpacity = 1
  )

# Show map, National Geographic background:

grcaRegionalBasemap %>%
  addProviderTiles('Esri.NatGeoWorldMap')

# Show map, aerial photo background:

grcaRegionalBasemap %>%
  addProviderTiles("Esri.WorldImagery")  

#=================================================================================*
# ---- VISUALIZING MIGRATION DISTANCE BY DATE ----
#=================================================================================*

# Distance from first gps location, scaled:

grcaMigration %>%
  group_by(tagId) %>%
  mutate(
    dBreeding = distBreedingKm/max(distBreedingKm)
  ) %>%
  ggplot(aes(x = date, y = dBreeding, group = tagId, col = tagId)) +
  geom_line(size = 1) +
  geom_point(size = 4, color = 'black') +
  geom_point(size = 3) +
  theme_bw() +
  scale_color_gradientn(colors = rainbow(4))

#=================================================================================*
# ---- VISUALIZING SUMMARY MIGRATION DATA ----
#=================================================================================*
# Just some basic early looks. Note that I removed crazy point 18, as this limits
# our ability to visualize anything.

# Length of migration, spatial by temporal:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = nMigrationDays, y = tDistance, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()

# How does the date of first migration vary with its temporal distance?

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = estDeparture, y = nMigrationDays, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()

# How does the date of first migration vary with its spatial distance?

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = estDeparture, y = tDistance, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()


# Boxplot (blech) summarizing how departure date varies by region:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = region, y = estDeparture)) +
  geom_boxplot(fill = 'gray90') +
  theme_classic()

# Boxplot (blech) summarizing how temporal distance varies by region:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = region, y = nMigrationDays)) +
  geom_boxplot(fill = 'gray90') +
  theme_classic()

# Boxplot (blech) summarizing how spatial distance varies by region:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = region, y = tDistance)) +
  geom_boxplot(fill = 'gray90') +
  theme_classic()

# Boxplot (blech) summarizing how winter latitude varies by region:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  ggplot(aes(x = region, y = winterLat)) +
  geom_boxplot(fill = 'gray90') +
  theme_classic()

#=================================================================================*
# ---- VISUALIZING SUMMARY MIGRATION DATA WITH MEASUREMENTS----
#=================================================================================*
# Some questionable bird measurement by migration stats stuff!

# Migration distance by wing:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  left_join(
    grcaTagBanding %>%
      select(-region),
    by = 'bandNumber') %>%
  filter(wing < 99999) %>%
  ggplot(aes(x = wing, y = tDistance, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()


# Migration distance by mass:

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  left_join(
    grcaTagBanding %>%
      select(-region),
    by = 'bandNumber') %>%
  filter(wing < 99999) %>%
  ggplot(aes(x = mass, y = tDistance, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()

# Migration distance by mass wing ratio

grcaSummary %>%
  filter(nMigrationDays < 75) %>%
  left_join(
    grcaTagBanding %>%
      select(-region),
    by = 'bandNumber') %>%
  filter(wing < 99999) %>%
  ggplot(aes(x = mass/wing, y = tDistance, color = region)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size = 5, color = 'black') +
  geom_point(size = 3) +
  theme_classic()

#=================================================================================*
# ---- VISUALIZING MIGRATION DATA WITH ENVIRONMENT ----
#=================================================================================*
# To use the environmental data, you will have to link it to the observation data.

# Doing so for the raw tag data is easy:

gpsTagData %>%
  left_join(env, by = 'eventId')

# To do so with the more informative migration data, it is the same:

grcaMigration %>%
  left_join(env, by = 'eventId')

# Both are easy to use:

grcaMigration %>%
  left_join(env, by = 'eventId') %>%
  filter(tagId %in% c(1383, 1385, 1391)) %>%
  filter(mStage == 'B'|mStage == 'M') %>%
  ggplot(aes(x = date, y = nightTemp, group = factor(tagId), color = factor(tagId))) +
  geom_point(size = 3) +
  geom_line() +
  theme_classic()

# You can bring in some of the summary data pretty simply as well. For example,
# if you wanted to plot by region:

grcaMigration %>%
  left_join(
    grcaSummary %>%
      select(tagId:region),
    by = 'tagId'
  ) %>%
  left_join(env, by = 'eventId') %>%
  filter(mStage == 'B'|mStage == 'M') %>%
  filter(region %in% c('DC', 'MA')) %>%
  ggplot(aes(x = date, y = nightTemp, group = factor(tagId), color = factor(region))) +
  geom_point(size = 3) +
  geom_line() +
  theme_classic()
  

# To combine all of the environment and summary data you would do this:

grcaMigration %>%
  left_join(grcaSummary %>%
              select(-bandNumber), by = 'tagId') %>%
  left_join(env, by = 'eventId')

# What might be more challenging is using these data. 

# For example, perhaps we want to plot the wind associated with the last 
# breeding date:

grcaMigration %>%
  left_join(grcaSummary %>%
              select(-bandNumber), by = 'tagId') %>%
  left_join(env, by = 'eventId') %>%
  group_by(tagId) %>%
  filter(mStage == 'B') %>%
  filter(date == max(date) & date > '2016-08-01') %>%
  ggplot(aes(x = uWind10m, y = vWind10m, color = factor(region))) +
  geom_point(size = 3) +
  geom_vline(xintercept=0, linetype = 2) +
  geom_hline(yintercept=0, linetype = 2) +
  theme_classic()

# Or maybe we might want to look at departure dates by land cover class:
# Note: You've got to zoom and stretch this pretty far to see the labels.

grcaMigration %>%
  left_join(grcaSummary %>%
              select(-bandNumber), by = 'tagId') %>%
  left_join(env, by = 'eventId') %>%
  left_join(lcLegend, by = 'globCoverLc') %>%
  group_by(tagId) %>%
  filter(mStage == 'B') %>%
  filter(date == max(date) & date > '2016-08-01') %>%
  ggplot(aes(x = factor(lc), y = date)) +
  geom_boxplot(fill = 'gray70') +
  coord_flip() +
  theme_bw()
