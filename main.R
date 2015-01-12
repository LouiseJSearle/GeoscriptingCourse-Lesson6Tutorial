### Louise Searle
### January 12 2015

## Lesson 6 Tutorial: Raster Vector integration in R

## 7.0 Example
## A simple land cover classification of Wageningen from Landsat 8 data
library(raster)
library(downloader)

# Download, unzip and load the data
dir.create('data/', showWarnings=F)
download('https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/landsat8.zip', 'data/landsat8.zip', quiet=T, mode='wb')
unzip('data/landsat8.zip', exdir='data/landsat8/')

# Identify the right file
landsatPath <- list.files('data/landsat8/', pattern = glob2rx('LC8*.grd'), full.names = TRUE)
wagLandsat <- brick(landsatPath)

# # plotRGB does not support negative values, so that they need to be removed.
wagLandsat[wagLandsat < 0] <- NA
plotRGB(wagLandsat, 5, 4, 3)

# Download municipality boundaries
nlCity <- getData('GADM',country='NLD', level=3)
class(nlCity)

# Investigate the structure of the object
head(nlCity@data)

# Subset the SpatialPolygonsDataFrame to the city of Wageningen alone.
wagContour <- nlCity[nlCity$NAME_2 == 'Wageningen',]

# Question: Would you rather reproject a raster or a vector layer? Give two reasons.
# Vector: it is quicker to reproject.
# Vector: no distortion of pixel values.

# Load rgdal library (needed to reproject data)
library(rgdal)

wagContourUTM <- spTransform(wagContour, CRS(proj4string(wagLandsat)))

wagLandsatCrop <- crop(wagLandsat, wagContourUTM)
wagLandsatSub <- mask(wagLandsat, wagContourUTM)

# Set graphical parameters (one row and two columns)
opar <- par(mfrow=c(1,2))
plotRGB(wagLandsatCrop, 5, 4, 3, main = 'Crop()')
plotRGB(wagLandsatSub, 5, 4, 3, main = 'Mask()')
plot(wagContourUTM, add = TRUE)

# Reset graphical parameters
par(opar)

# Water mask of Wageningen in vector format.
download('https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/wageningenWater.zip', 'data/wageningenWater.zip', mode='auto', quiet=T)
unzip('data/wageningenWater.zip', exdir='data/wageningenWater/')

# Check the names of the layers for input in readOGR()
ogrListLayers('data/wageningenWater/Water.shp')
water <- readOGR('data/wageningenWater/Water.shp', layer = 'Water')
waterUTM <- spTransform(water, CRS(proj4string(wagLandsat)))

# Mask the pixels the intersect with the features of the vector object.
wagLandsatSubW <- mask(wagLandsatSub, mask = waterUTM, inverse = TRUE)
plotRGB(wagLandsatSubW, 5, 4, 3)
plot(waterUTM, col = 'blue', add = TRUE)

## 7.1 Build a calibration dataset in Google Earth

# Change to the correct file path and layer name
samples <- readOGR('data/CalibrationData.kml', layer = 'CalibrationData')

# Re-project SpatialPointsDataFrame
samplesUTM <- spTransform(samples, CRS(proj4string(wagLandsatCrop)))

# The extract function does not understand why the object would have 3 coord columns, so we need to edit this field
samplesUTM@coords <- coordinates(samplesUTM)[,-3]

# Extract the surface reflectance 
calib <- extract(wagLandsatCrop, samplesUTM, df=TRUE)

# Combine the newly created dataframe to the description column of the calibration dataset
calib2 <- cbind(samplesUTM$Description, calib)

# Change the name of the first column, for convienience
colnames(calib2)[1] <- 'lc'

# Inspect the structure of the dataframe
str(calib2)

#  calibrate a random forest model using the extracted data frame.
install.packages('randomForest')
library(randomForest)

# Calibrate model
model <- randomForest(lc ~ band1 + band2 + band3 + band4 + band5 + band6 + band7, data = calib2)

# Use the model to predict land cover
lcMap <- predict(wagLandsatCrop, model = model)

# The function levelplot() from the rasterVis package is a convenient function to plot categorical raster data.
library(rasterVis)

levelplot(lcMap, col.regions = c('lightgreen', 'mediumseagreen', 'lightgoldenrod', 'lightyellow4', 'lightskyblue1'))

## 7.2 Extract raster values along a transect

# Download data
bel <- getData('alt', country='BEL', mask=TRUE)

# Display metadata
bel
plot(bel)

# We want to look at a transect, which we can draw by hand by selecting two points by clicking
line <- drawLine()

#Then the elevation values can simply be extracted using extract().
alt <- extract(bel, line, along = TRUE)
plot(alt[[1]], type = 'l')

# calculate the distance between the two extremities of the transect,
install.packages('geosphere')
library(geosphere)

# Calculate great circle distance between the two ends of the line
dist <- distHaversine(coordinates(line)[[1]][[1]][1,], coordinates(line)[[1]][[1]][2,])

# Format a vector for use as x axis index
distanceVector <- seq(0, dist, along.with = alt[[1]])

# Visualize the output
# Set graphical parameters (grid with 2 rows and 1 column)
opar <- par(mfrow = c(2,1))
plot(bel, main = 'Altitude (m)')
plot(line, add = TRUE)
plot(distanceVector/1000, alt[[1]], type = 'l',
     main = 'Altitude transect Belgium',
     xlab = 'Distance (Km)',
     ylab = 'Altitude (m)',
     las = 1)
# Reset graphical parameters to default
par(opar)
