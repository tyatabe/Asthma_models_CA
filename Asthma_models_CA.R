# Set wd in laptop's developer's folder
setwd('/home/tyatabe/Onedrive/Dev folder/Asthma_models_CA/data/')

# read in and prepare asthma attacks data
asthma <- read.csv("Asthma_Emergency_Department_Visit_Rates_by_ZIP_Code.csv")

# clean ZIP code data (separate zip from coordinates)
library(zipcode)
asthma$zip <- as.factor(clean.zipcodes(substr(asthma$ZIP.code, 1,5 )))


# Get total emergency visits by zipcode
cases <- asthma[asthma$Age.Group=="All Ages",]
cases <- cases[,-2]
cases$Age.adjusted.rate <- log(cases$Age.adjusted.rate)
# zipcode coordinates

data(zipcode)
zipcode$zip <- as.factor(zipcode$zip)
cases <- merge(cases, zipcode, by="zip")

# Read in and prepare air quality data

so2 <- rbind(read.csv('SO2_2009.csv'), read.csv('SO2_2012.csv'), 
             read.csv('SO2_2013.csv'), read.csv('SO2_2014.csv'), 
             read.csv('SO2_2015.csv'))
so2$Year <- ifelse(grepl("2009",so2$Date, fixed=T), 2009, 
                   ifelse(grepl("2012",so2$Date, fixed=T), 2012, 
                   ifelse(grepl("2013",so2$Date, fixed=T), 2013, 
                   ifelse(grepl("2014",so2$Date, fixed=T), 2014,2015))))

no2 <- rbind(read.csv('NO2_2009.csv'), read.csv('NO2_2012.csv'), 
             read.csv('NO2_2013.csv'), read.csv('NO2_2014.csv'), 
             read.csv('NO2_2015.csv'))
no2$Year <- ifelse(grepl("2009",no2$Date, fixed=T), 2009, 
                   ifelse(grepl("2012",no2$Date, fixed=T), 2012, 
                          ifelse(grepl("2013",no2$Date, fixed=T), 2013, 
                                 ifelse(grepl("2014",no2$Date, fixed=T), 2014,2015))))

co <- rbind(read.csv('CO_2009.csv'), read.csv('CO_2012.csv'), 
             read.csv('CO_2013.csv'), read.csv('CO_2014.csv'), 
             read.csv('CO_2015.csv'))
co$Year <- ifelse(grepl("2009",co$Date, fixed=T), 2009, 
                   ifelse(grepl("2012",co$Date, fixed=T), 2012, 
                          ifelse(grepl("2013",co$Date, fixed=T), 2013, 
                                 ifelse(grepl("2014",co$Date, fixed=T), 2014,2015))))

o3 <- rbind(read.csv('O3_2009.csv'), read.csv('O3_2012.csv'), 
            read.csv('O3_2013.csv'), read.csv('O3_2014.csv'), 
            read.csv('O3_2015.csv'))
o3$Year <- ifelse(grepl("2009",o3$Date, fixed=T), 2009, 
                  ifelse(grepl("2012",o3$Date, fixed=T), 2012, 
                         ifelse(grepl("2013",o3$Date, fixed=T), 2013, 
                                ifelse(grepl("2014",o3$Date, fixed=T), 2014,2015))))

pm10 <- rbind(read.csv('PM10_2009.csv'), read.csv('PM10_2012.csv'), 
            read.csv('PM10_2013.csv'), read.csv('PM10_2014.csv'), 
            read.csv('PM10_2015.csv'))
pm10$Year <- ifelse(grepl("2009",pm10$Date, fixed=T), 2009, 
                  ifelse(grepl("2012",pm10$Date, fixed=T), 2012, 
                         ifelse(grepl("2013",pm10$Date, fixed=T), 2013, 
                                ifelse(grepl("2014",pm10$Date, fixed=T), 2014,2015))))

pm25 <- rbind(read.csv('PM25_2009.csv'), read.csv('PM25_2012.csv'), 
              read.csv('PM25_2013.csv'), read.csv('PM25_2014.csv'), 
              read.csv('PM25_2015.csv'))
pm25$Year <- ifelse(grepl("2009",pm25$Date, fixed=T), 2009, 
                    ifelse(grepl("2012",pm25$Date, fixed=T), 2012, 
                           ifelse(grepl("2013",pm25$Date, fixed=T), 2013, 
                                  ifelse(grepl("2014",pm25$Date, fixed=T), 2014,2015))))

# Extract the median for each measuring station

library(doBy)
so2 <- summaryBy(Daily.Max.1.hour.SO2.Concentration 
                 ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                 FUN=median, data = so2)

no2 <- summaryBy(Daily.Max.1.hour.NO2.Concentration 
                 ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                 FUN=median, data = no2)

co <- summaryBy(Daily.Max.8.hour.CO.Concentration 
                 ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                 FUN=median, data = co)

o3 <- summaryBy(Daily.Max.8.hour.Ozone.Concentration 
                ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                FUN=median, data = o3)

pm10 <- summaryBy(Daily.Mean.PM10.Concentration 
                ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                FUN=median, data = pm10)

pm25 <- summaryBy(Daily.Mean.PM2.5.Concentration 
                  ~ Year + AQS_SITE_ID + SITE_LATITUDE + SITE_LONGITUDE, 
                  FUN=median, data = pm25)

# Checking the distribution of the concentration of pollutants

par(mfrow=c(2,3))
plot(density(so2$Daily.Max.1.hour.SO2.Concentration))
plot(density(no2$Daily.Max.1.hour.NO2.Concentration))
plot(density(co$Daily.Max.8.hour.CO.Concentration))
plot(density(o3$Daily.Max.8.hour.Ozone.Concentration))
plot(density(pm10$Daily.Mean.PM10.Concentration))
plot(density(pm25$Daily.Mean.PM2.5.Concentration))

# Looks very right skewed for so2, and pm10. Also negative values

# Do min-max normalization to remove neg values

so2$value <- as.vector(scale(so2$Daily.Max.1.hour.SO2.Concentration, 
                   center = min(so2$Daily.Max.1.hour.SO2.Concentration),
                   scale=max(so2$Daily.Max.1.hour.SO2.Concentration) - 
                     min(so2$Daily.Max.1.hour.SO2.Concentration)))

no2$value <- as.vector(scale(no2$Daily.Max.1.hour.NO2.Concentration,
                   center = min(no2$Daily.Max.1.hour.NO2.Concentration),
                   scale=max(no2$Daily.Max.1.hour.NO2.Concentration) -
                     min(no2$Daily.Max.1.hour.NO2.Concentration)))

co$value <- as.vector(scale(co$Daily.Max.8.hour.CO.Concentration.median,
                   center = min(co$Daily.Max.8.hour.CO.Concentration.median),
                   scale=max(co$Daily.Max.8.hour.CO.Concentration.median) -
                     min(co$Daily.Max.8.hour.CO.Concentration.median)))

o3$value <- as.vector(scale(o3$Daily.Max.8.hour.Ozone.Concentration.median,
                      center = min(o3$Daily.Max.8.hour.Ozone.Concentration.median),
                      scale=max(o3$Daily.Max.8.hour.Ozone.Concentration.median) -
                        min(o3$Daily.Max.8.hour.Ozone.Concentration.median)))

pm10$value <- as.vector(scale(pm10$Daily.Mean.PM10.Concentration.median,
                        center = min(pm10$Daily.Mean.PM10.Concentration.median),
                        scale=max(pm10$Daily.Mean.PM10.Concentration.median) -
                          min(pm10$Daily.Mean.PM10.Concentration.median)))

pm25$value <- as.vector(scale(pm25$Daily.Mean.PM2.5.Concentration.median,
                        center = min(pm25$Daily.Mean.PM2.5.Concentration.median),
                         scale=max(pm25$Daily.Mean.PM2.5.Concentration.median) -
                          min(pm25$Daily.Mean.PM2.5.Concentration.median)))


# See how log transform worksPlot and see how it looks
par(mfrow=c(2,3))
plot(density(log(so2$value + 0.001)))
plot(density(no2$value))
plot(density(co$value))
plot(density(o3$value))
plot(density(log(pm10$value+0.001)))
plot(density(pm25$value))

# it's better only for so2 and pm10
so2$value <- log(so2$value + 0.001)
pm10$value <- log(pm10$value+0.001)

# Getting maps to work with spatial data
library(raster)
library(rgdal)
# US and CA shapefiles
usa <- getData('GADM', country='USA', level=1)
ca_simple <- usa[ usa$NAME_1 %in% c('California'), ]
# California shapefile (county level)
caco <- shapefile("counties_2000_TA.shp")
ca_simple <- spTransform(ca_simple, CRS(projection(caco)))

# Create spatial points data frame for all contaminants
so2.pts <- SpatialPoints(data.frame(so2$SITE_LONGITUDE, so2$SITE_LATITUDE))
so2.pts <- SpatialPointsDataFrame(so2.pts, so2)
projection(so2.pts) <- "+proj=longlat +datum=NAD83"
no2.pts <- SpatialPoints(data.frame(no2$SITE_LONGITUDE, no2$SITE_LATITUDE))
no2.pts <- SpatialPointsDataFrame(no2.pts, no2)
projection(no2.pts) <- "+proj=longlat +datum=NAD83"
co.pts <- SpatialPoints(data.frame(co$SITE_LONGITUDE, co$SITE_LATITUDE))
co.pts <- SpatialPointsDataFrame(co.pts, co)
projection(co.pts) <- "+proj=longlat +datum=NAD83"
o3.pts <- SpatialPoints(data.frame(o3$SITE_LONGITUDE, o3$SITE_LATITUDE))
o3.pts <- SpatialPointsDataFrame(o3.pts, o3)
projection(o3.pts) <- "+proj=longlat +datum=NAD83"
pm10.pts <- SpatialPoints(data.frame(pm10$SITE_LONGITUDE, pm10$SITE_LATITUDE))
pm10.pts <- SpatialPointsDataFrame(pm10.pts, pm10)
projection(pm10.pts) <- "+proj=longlat +datum=NAD83"
pm25.pts <- SpatialPoints(data.frame(pm25$SITE_LONGITUDE, pm25$SITE_LATITUDE))
pm25.pts <- SpatialPointsDataFrame(pm25.pts, pm25)
projection(pm25.pts) <- "+proj=longlat +datum=NAD83"

## Transform the data to Teale Albers (note that I am using km here!)
TA <- CRS(" +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0")
so2.pts <- spTransform(so2.pts, TA)
no2.pts <- spTransform(no2.pts, TA)
co.pts <- spTransform(co.pts, TA)
o3.pts <- spTransform(o3.pts, TA)
pm10.pts <- spTransform(pm10.pts, TA)
pm25.pts <- spTransform(pm25.pts, TA)

# CA and USA to TA
usa <- spTransform(usa, TA)
ca_simple <- spTransform(ca_simple, TA)

## Get elevation data:
elv <- getData('worldclim', res=2.5, var='alt')
r <- raster(usa, res=5)
celv <- projectRaster(elv, r)
celv <- mask(celv, usa)


