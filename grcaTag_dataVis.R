source('grcaTag_dataPrep.R')

#=========================================================================*
# ---- MAPPING ----
#=========================================================================*

# map by individual -------------------------------------------------------

# Set colors for birds:

colorPalette <-
  colorFactor(palette = rainbow(length(tagIds)),
              domain = gpsTagData$tagId)

# Lines for each bird:

makeLines <-
  function(dataframeIn, colorMatch) {
    # Unique IDs:
    birdIDs <- unique(dataframeIn$tagId)
    # List of data frames, one per ID:
    tracksList <- vector('list', length = length(birdIDs))
    for (i in 1:length(tracksList)) {
      tracksList[[i]] <- dataframeIn %>%
        filter(tagId == birdIDs[i])
    }
    # List of lines objects (one list item per tagId):
    birdLines <- vector('list', length = length(birdIDs))
    for (i in 1:length(tracksList)) {
      coordMat <- tracksList[[i]] %>%
        select(long, lat) %>%
        as.matrix
      birdLines[[i]] <- Lines(Line(coordMat),
                              ID = tracksList[[i]] %>%
                                select(tagId) %>%
                                unique)
    }
    # Combine lines list to spatial lines data frame:
    SpatialLinesDataFrame(
      SpatialLines(birdLines),
      data = dataframeIn %>%
        select(tagId) %>%
        distinct,
      match.ID = FALSE
    )
  }

# Make basemap:

grcaBasemap <-
  leaflet(data = gpsTagData %>% filter(tagId != 0),
          options = leafletOptions(zoomControl = FALSE)) %>%
  # Background of lines, for visibility:
  addPolylines(
    data = makeLines(gpsTagData %>% filter(tagId != 0)),
    weight = 5,
    opacity = .3,
    color = 'black'
  )

# Add colored lines and points:

grcaPointLine <-
  grcaBasemap %>%
  # Lines, colored by tagId:
  addPolylines(
    data = makeLines(gpsTagData %>% filter(tagId != 0)),
    weight = 3,
    dashArray = "5, 5",
    opacity = 1,
    color = ~ colorPalette(tagId)
  ) %>%
  # Circle markers, colored by tagId:
  addCircleMarkers(
    ~ long,
    ~ lat,
    radius = ~ 4,
    color = 'white',
    fillColor = ~ colorPalette(tagId),
    stroke = TRUE,
    popup = ~ paste(
      sep = "<br/>",
      paste0('<b style="color:#0000FF">', tagId, '</b>'),
      paste0("<b> Band number: </b>", bandNumber),
      paste0("<b> Date: </b>", date),
      paste0("<b> Longitude: </b>", round(long, 4)),
      paste0("<b> Latitude: </b>", round(lat, 4))
    ),
    fillOpacity = 0.9
  )

# Show map, open streetmap:

grcaPointLine %>%
  addTiles()

# Show map, National Geographic background:

grcaPointLine %>%
  addProviderTiles('Esri.NatGeoWorldMap')

# Show map, aerial photo background:

grcaPointLine %>%
  addProviderTiles("Esri.WorldImagery")

# map by region -----------------------------------------------------------

# Add region to tagging data:

gTaggingWithRegion <-
  left_join(grcaMigration,
            grcaTagBanding %>%
              select(bandNumber, region),
            by = 'bandNumber')

# Function to add lines by region:

addLines_region <-
  function(leafMap, regionValue, lineColor) {
    dataRegion <- gTaggingWithRegion %>%
      filter(region == regionValue)
    leafMap %>%
      addPolylines(
        data = makeLines(dataRegion),
        weight = 5,
        opacity = .3,
        color = 'black'
      ) %>%
      addPolylines(
        data = makeLines(dataRegion),
        weight = 3,
        dashArray = "5, 5",
        opacity = 1,
        color = lineColor
      )
  }

# Regional color palette:

colorPaletteRegion <-
  colorFactor(palette = c('#8A2BE2', 'blue', 'yellow', 'red'),
              domain = gTaggingWithRegion$region)

grcaRegionalBasemap <-
  leaflet(data = gTaggingWithRegion,
          options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles() %>%
  addLines_region('CO', '#8A2BE2') %>%
  addLines_region('DC', 'blue') %>%
  addLines_region('MA', 'red') %>%
  addLines_region('GA', 'yellow') %>%
  addCircleMarkers(
    ~ long,
    ~ lat,
    radius = ~ 3,
    color = 'white',
    fillColor = ~ colorPaletteRegion(region),
    stroke = TRUE,
    popup = ~ paste(
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
