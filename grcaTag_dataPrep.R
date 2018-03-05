# source ------------------------------------------------------------------

source('grcaTag_source.R')

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