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

