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

# CA and USA to TA...NOT WORKING
usa <- spTransform(usa, TA)
ca_simple <- spTransform(ca_simple, TA)

## Get environmental data

# Elevation data for CA
elv1 <- getData('worldclim', res=0.5, lon = -121, lat=40 , var='alt')
elv2 <- getData('worldclim', res=0.5, lon = -120, lat=40 , var='alt')
elv <- mosaic(elv1, elv2, fun=mean)
r <- raster(ca_simple, res=5)
celv <- projectRaster(elv, r)
celv <- mask(celv, ca_simple)

# Average wind speed data for CA. Need to stack and average myself
wind <- list()
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
for (i in 1:12){
wind[i] <- raster(paste("wc2.0_30s_wind_", month[i], ".tif", sep=""))
}
# Stacking, projecting and masking
wind.s <- stack(wind)
cwind.s <- projectRaster(wind.s, r)
cwind.s <- mask(cwind.s, ca_simple)

# Mean wind speed in CA
cwind <- mean(cwind.s)

# Annual precipitation...need better resolution...downloading now
prec <- raster("wc2.0_bio_2.5m_12.tif")
cprec <- projectRaster(prec, r)
cprec <- mask(cprec, ca_simple)

# Average temp...need better resolution...downloading now
temp <- raster("wc2.0_bio_2.5m_01.tif")
ctemp <- projectRaster(temp, r)
ctemp <- mask(ctemp, ca_simple)

# Radiation...this is just april and low resolution. Need higher res and whole year to get average
rad <- raster("wc2.0_2.5m_srad_04.tif")
crad <- projectRaster(rad, r)
crad <- mask(crad, ca_simple)

# Make raster stack of air pollutants
covar <- stack(celv, cwind, cprec, ctemp, crad)

## Convert cases to spatialpoints data frame
cases.pts <- SpatialPoints(data.frame(cases$longitude, cases$latitude))
cases.pts <- SpatialPointsDataFrame(cases.pts, cases)
projection(cases.pts) <- "+proj=longlat +datum=NAD83"
#Project
cases.pts <- spTransform(cases.pts, TA)

## Interpolation of air pollutants
# NEED TO DO IT FOR EVERY YEAR!!!

# Transform raster stack to spatial pixels data frame
covar <- as(covar, "SpatialPixelsDataFrame")

# SO2. It did not converged
library(GSIF)
library(plotKML)

rf.so2.09 <- fit.gstatModel(so2.pts[so2.pts@data$Year==2009,], 
                            value~ layer.1 + layer.2 + wc2.0_bio_2.5m_12
                         + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                         method="randomForest", fit.method=7)
# Checking the model
print(rf.so2.09)
plot(rf.so2.09)
plot(rf.so2@regModel)
rf.so2@vgmModel

# Predict...It doesn't work for SO2...to little obs
# Perhaps with data for the whole US
so2.rk <- predict(rf.so2.09, covar, predict.method = "RK")
plot(so2.rk)

# NO2. it converged for 2009, 2013, 2014, and 2015
# It did not converged for 2012
rf.no2.09 <- fit.gstatModel(no2.pts[no2.pts@data$Year==2009,], 
                         value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                         + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                         method="randomForest", fit.method=7)

rf.no2.12 <- fit.gstatModel(no2.pts[no2.pts@data$Year==2012,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.no2.13 <- fit.gstatModel(no2.pts[no2.pts@data$Year==2013,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.no2.14 <- fit.gstatModel(no2.pts[no2.pts@data$Year==2014,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.no2.15 <- fit.gstatModel(no2.pts[no2.pts@data$Year==2015,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)


print(rf.no2.13)
plot(rf.no2.09)

## plot the estimated error for number of bootstrapped trees:
plot(rf.no2.09@regModel)
rf.no2.09@vgmModel

# Predict...predictions still good
no2.rk.09 <- predict(rf.no2.09, covar, predict.method = "RK")
no2.rk.12 <- predict(rf.no2.12, covar, predict.method = "RK")
no2.rk.13 <- predict(rf.no2.13, covar, predict.method = "RK")
no2.rk.14 <- predict(rf.no2.14, covar, predict.method = "RK")
no2.rk.15 <- predict(rf.no2.15, covar, predict.method = "RK")

plot(no2.rk.15)# it looks good

# Making a raster from predictions
pred.no2.09 <- no2.rk.09@predicted
no2.rk.raster.09 <- raster(pred.no2.09[4])

pred.no2.12 <- no2.rk.12@predicted
no2.rk.raster.12 <- raster(pred.no2.12[4])

pred.no2.13 <- no2.rk.13@predicted
no2.rk.raster.13 <- raster(pred.no2.13[4])

pred.no2.14 <- no2.rk.14@predicted
no2.rk.raster.14 <- raster(pred.no2.14[4])

pred.no2.15 <- no2.rk.15@predicted
no2.rk.raster.15 <- raster(pred.no2.15[4])


plot(no2.rk.raster.15); plot(no2.pts[no2.pts@data$Year==2015,], add=T, pch=1, cex=(no2.pts[no2.pts@data$Year==2015,]$value-min(no2.pts[no2.pts@data$Year==2015,]$value))
                          /max(no2.pts[no2.pts@data$Year==2015,]$value-min(no2.pts[no2.pts@data$Year==2015,]$value)))


# CO it converged for 2013(neg r2)
# It did not converged for 2009, 2012(neg r2), 2014 (neg r2), 2015
rf.co.09 <- fit.gstatModel(co.pts[co.pts@data$Year==2009,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.co.12 <- fit.gstatModel(co.pts[co.pts@data$Year==2012,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.co.13 <- fit.gstatModel(co.pts[co.pts@data$Year==2013,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.co.14 <- fit.gstatModel(co.pts[co.pts@data$Year==2014,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)

rf.co.15 <- fit.gstatModel(co.pts[co.pts@data$Year==2015,], 
                            value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                            + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                            method="randomForest", fit.method=7)


print(rf.co.15)
plot(rf.co.09)

## plot the estimated error for number of bootstrapped trees:
plot(rf.co.09@regModel)
rf.co.09@vgmModel

# Predict...predictions still good
co.rk.09 <- predict(rf.co.09, covar, predict.method = "RK")
co.rk.12 <- predict(rf.co.12, covar, predict.method = "RK")
co.rk.13 <- predict(rf.co.13, covar, predict.method = "RK")
co.rk.14 <- predict(rf.co.14, covar, predict.method = "RK")
co.rk.15 <- predict(rf.co.15, covar, predict.method = "RK")

plot(co.rk.15)# it looks ok for some

# Making a raster from predictions
pred.co.09 <- co.rk.09@predicted
co.rk.raster.09 <- raster(pred.co.09[4])

pred.co.12 <- co.rk.12@predicted
co.rk.raster.12 <- raster(pred.co.12[4])

pred.co.13 <- co.rk.13@predicted
co.rk.raster.13 <- raster(pred.co.13[4])

pred.co.14 <- co.rk.14@predicted
co.rk.raster.14 <- raster(pred.co.14[2])

pred.co.15 <- co.rk.15@predicted
co.rk.raster.15 <- raster(pred.co.15[4])

# It looks surprisingly ok (not great, just OK)
plot(co.rk.raster.15); plot(co.pts[co.pts@data$Year==2015,], add=T, pch=1, cex=(co.pts[co.pts@data$Year==2015,]$value-min(co.pts[co.pts@data$Year==2015,]$value))
                             /max(co.pts[co.pts@data$Year==2015,]$value-min(co.pts[co.pts@data$Year==2015,]$value)))


# O3 it converged for 2009, 2013, 2014
# It did not converged for 2012, 2015
rf.o3.09 <- fit.gstatModel(o3.pts[o3.pts@data$Year==2009,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.o3.12 <- fit.gstatModel(o3.pts[o3.pts@data$Year==2012,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.o3.13 <- fit.gstatModel(o3.pts[o3.pts@data$Year==2013,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.o3.14 <- fit.gstatModel(o3.pts[o3.pts@data$Year==2014,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.o3.15 <- fit.gstatModel(o3.pts[o3.pts@data$Year==2015,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)


print(rf.o3.15)
plot(rf.o3.09)

## plot the estimated error for number of bootstrapped trees:
plot(rf.o3.09@regModel)
rf.o3.09@vgmModel

# Predict...predictions still good
o3.rk.09 <- predict(rf.o3.09, covar, predict.method = "RK")
o3.rk.12 <- predict(rf.o3.12, covar, predict.method = "RK")
o3.rk.13 <- predict(rf.o3.13, covar, predict.method = "RK")
o3.rk.14 <- predict(rf.o3.14, covar, predict.method = "RK")
o3.rk.15 <- predict(rf.o3.15, covar, predict.method = "RK")

plot(o3.rk.15)# it looks good

# Making a raster from predictions
pred.o3.09 <- o3.rk.09@predicted
o3.rk.raster.09 <- raster(pred.o3.09[4])

pred.o3.12 <- o3.rk.12@predicted
o3.rk.raster.12 <- raster(pred.o3.12[4])

pred.o3.13 <- o3.rk.13@predicted
o3.rk.raster.13 <- raster(pred.o3.13[4])

pred.o3.14 <- o3.rk.14@predicted
o3.rk.raster.14 <- raster(pred.o3.14[4])

pred.o3.15 <- o3.rk.15@predicted
o3.rk.raster.15 <- raster(pred.o3.15[4])

# It looks good
plot(o3.rk.raster.15); plot(o3.pts[o3.pts@data$Year==2015,], add=T, pch=1, cex=(o3.pts[o3.pts@data$Year==2015,]$value-min(o3.pts[o3.pts@data$Year==2015,]$value))
                            /max(o3.pts[o3.pts@data$Year==2015,]$value-min(o3.pts[o3.pts@data$Year==2015,]$value)))


# PM10 it converged for 2009, 2015
# It did not converged for 2012, 2013, 2014
rf.pm10.09 <- fit.gstatModel(pm10.pts[pm10.pts@data$Year==2009,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.pm10.12 <- fit.gstatModel(pm10.pts[pm10.pts@data$Year==2012,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.pm10.13 <- fit.gstatModel(pm10.pts[pm10.pts@data$Year==2013,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.pm10.14 <- fit.gstatModel(pm10.pts[pm10.pts@data$Year==2014,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)

rf.pm10.15 <- fit.gstatModel(pm10.pts[pm10.pts@data$Year==2015,], 
                           value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                           + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                           method="randomForest", fit.method=7)


print(rf.pm10.15)
plot(rf.pm10.09)

## plot the estimated error for number of bootstrapped trees:
plot(rf.pm10.09@regModel)
rf.pm10.09@vgmModel

# Predict...predictions still good
pm10.rk.09 <- predict(rf.pm10.09, covar, predict.method = "RK")
pm10.rk.12 <- predict(rf.pm10.12, covar, predict.method = "RK")
pm10.rk.13 <- predict(rf.pm10.13, covar, predict.method = "RK")
pm10.rk.14 <- predict(rf.pm10.14, covar, predict.method = "RK")
pm10.rk.15 <- predict(rf.pm10.15, covar, predict.method = "RK")

plot(pm10.rk.15)# it looks good

# Making a raster from predictions
pred.pm10.09 <- pm10.rk.09@predicted
pm10.rk.raster.09 <- raster(pred.pm10.09[4])

pred.pm10.12 <- pm10.rk.12@predicted
pm10.rk.raster.12 <- raster(pred.pm10.12[4])

pred.pm10.13 <- pm10.rk.13@predicted
pm10.rk.raster.13 <- raster(pred.pm10.13[4])

pred.pm10.14 <- pm10.rk.14@predicted
pm10.rk.raster.14 <- raster(pred.pm10.14[4])

pred.pm10.15 <- pm10.rk.15@predicted
pm10.rk.raster.15 <- raster(pred.pm10.15[4])

# It looks good
plot(pm10.rk.raster.15); plot(pm10.pts[pm10.pts@data$Year==2015,], add=T, pch=1, cex=(pm10.pts[pm10.pts@data$Year==2015,]$value-min(pm10.pts[pm10.pts@data$Year==2015,]$value))
                            /max(pm10.pts[pm10.pts@data$Year==2015,]$value-min(pm10.pts[pm10.pts@data$Year==2015,]$value)))


# PM2.5 it converged for 2009
# It did not converged for 2012, 2013, 2014, 2015
rf.pm25.09 <- fit.gstatModel(pm25.pts[pm25.pts@data$Year==2009,], 
                             value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                             + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                             method="randomForest", fit.method=7)

rf.pm25.12 <- fit.gstatModel(pm25.pts[pm25.pts@data$Year==2012,], 
                             value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                             + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                             method="randomForest", fit.method=7)

rf.pm25.13 <- fit.gstatModel(pm25.pts[pm25.pts@data$Year==2013,], 
                             value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                             + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                             method="randomForest", fit.method=7)

rf.pm25.14 <- fit.gstatModel(pm25.pts[pm25.pts@data$Year==2014,], 
                             value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                             + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                             method="randomForest", fit.method=7)

rf.pm25.15 <- fit.gstatModel(pm25.pts[pm25.pts@data$Year==2015,], 
                             value~layer.1 + layer.2 + wc2.0_bio_2.5m_12
                             + wc2.0_bio_2.5m_01 + wc2.0_2.5m_srad_04, covar,
                             method="randomForest", fit.method=7)


print(rf.pm25.15)
plot(rf.pm25.09)

## plot the estimated error for number of bootstrapped trees:
plot(rf.pm25.09@regModel)
rf.pm25.09@vgmModel

# Predict...predictions still good
pm25.rk.09 <- predict(rf.pm25.09, covar, predict.method = "RK")
pm25.rk.12 <- predict(rf.pm25.12, covar, predict.method = "RK")
pm25.rk.13 <- predict(rf.pm25.13, covar, predict.method = "RK")
pm25.rk.14 <- predict(rf.pm25.14, covar, predict.method = "RK")
pm25.rk.15 <- predict(rf.pm25.15, covar, predict.method = "RK")

plot(pm25.rk.15)# it looks good

# Making a raster from predictions
pred.pm25.09 <- pm25.rk.09@predicted
pm25.rk.raster.09 <- raster(pred.pm25.09[4])

pred.pm25.12 <- pm25.rk.12@predicted
pm25.rk.raster.12 <- raster(pred.pm25.12[4])

pred.pm25.13 <- pm25.rk.13@predicted
pm25.rk.raster.13 <- raster(pred.pm25.13[4])

pred.pm25.14 <- pm25.rk.14@predicted
pm25.rk.raster.14 <- raster(pred.pm25.14[2])

pred.pm25.15 <- pm25.rk.15@predicted
pm25.rk.raster.15 <- raster(pred.pm25.15[4])

# It looks good
plot(pm25.rk.raster.15); plot(pm25.pts[pm25.pts@data$Year==2015,], add=T, pch=1, cex=(pm25.pts[pm25.pts@data$Year==2015,]$value-min(pm25.pts[pm25.pts@data$Year==2015,]$value))
                              /max(pm25.pts[pm25.pts@data$Year==2015,]$value-min(pm25.pts[pm25.pts@data$Year==2015,]$value)))

# Stack the rasters of interpolated concentration for each pollutants
# Except so2 b/c too little data
no2.stack <- stack(no2.rk.raster.09, no2.rk.raster.12, no2.rk.raster.13, 
                   no2.rk.raster.14, no2.rk.raster.15)

co.stack <- stack(co.rk.raster.09, co.rk.raster.12, co.rk.raster.13, 
                   co.rk.raster.14, co.rk.raster.15)

o3.stack <- stack(o3.rk.raster.09, o3.rk.raster.12, o3.rk.raster.13, 
                   o3.rk.raster.14, o3.rk.raster.15)

pm10.stack <- stack(pm10.rk.raster.09, pm10.rk.raster.12, pm10.rk.raster.13, 
                   pm10.rk.raster.14, pm10.rk.raster.15)

pm25.stack <- stack(pm25.rk.raster.09, pm25.rk.raster.12, pm25.rk.raster.13, 
                   pm25.rk.raster.14, pm25.rk.raster.15)

# Make stack of all pollutants for 2015
all.stack <- stack(no2.rk.raster.15, co.rk.raster.15, o3.rk.raster.15, 
                   pm10.rk.raster.15, pm25.rk.raster.15)


# Extract values for each zip code
# Extract no2, co, o3, pm10, amd pm25

kk.pts <- cases.pts
kk.pts@data$no2 <- extract(no2.stack, kk.pts)
kk.pts@data$co <- extract(co.stack, kk.pts)
kk.pts@data$o3 <- extract(o3.stack, kk.pts)
kk.pts@data$pm10 <- extract(pm10.stack, kk.pts)
kk.pts@data$pm25 <- extract(pm25.stack, kk.pts)

dat <- kk.pts@data

d <- dat [,c("zip", "Year", "Number.of.Visits", "Age.adjusted.rate",
                "latitude", "longitude")]
d$rate <- exp(d$Age.adjusted.rate)
d$no2 <- ifelse(dat$Year==2009, dat$no2[,1],ifelse(dat$Year==2012, dat$no2[,2],
                ifelse(dat$Year==2013, dat$no2[,3], 
                ifelse(dat$Year==2014, dat$no2[,4], 
                ifelse(dat$Year==2015, dat$no2[,5],-999)))))

d$co <- ifelse(dat$Year==2009, dat$co[,1],ifelse(dat$Year==2012, dat$co[,2],
                                                   ifelse(dat$Year==2013, dat$co[,3], 
                                                          ifelse(dat$Year==2014, dat$co[,4], 
                                                                 ifelse(dat$Year==2015, dat$co[,5],-999)))))

d$o3 <- ifelse(dat$Year==2009, dat$o3[,1],ifelse(dat$Year==2012, dat$o3[,2],
                                                   ifelse(dat$Year==2013, dat$o3[,3], 
                                                          ifelse(dat$Year==2014, dat$o3[,4], 
                                                                 ifelse(dat$Year==2015, dat$o3[,5],-999)))))

d$pm10 <- ifelse(dat$Year==2009, dat$pm10[,1],ifelse(dat$Year==2012, dat$pm10[,2],
                                                   ifelse(dat$Year==2013, dat$pm10[,3], 
                                                          ifelse(dat$Year==2014, dat$pm10[,4], 
                                                                 ifelse(dat$Year==2015, dat$pm10[,5],-999)))))

d$pm25 <- ifelse(dat$Year==2009, dat$pm25[,1],ifelse(dat$Year==2012, dat$pm25[,2],
                                                   ifelse(dat$Year==2013, dat$pm25[,3], 
                                                          ifelse(dat$Year==2014, dat$pm25[,4], 
                                                                 ifelse(dat$Year==2015, dat$pm25[,5],-999)))))

# Remove NA's (not so sure why they don't have pollutants, perhaps out of state boundary)
d <- d[!is.na(d$o3),]


# Split data in training and testing
training <- d[d$Year<2015,c(3:4, 8:12)]
testing <- d[d$Year>2014,c(3:4, 8:12)]
all <- d

# Run the models: random forest
library(randomForest)
rfbf = randomForest(Age.adjusted.rate ~ ., data=training[,-1], importance=T)
rfbf2 = randomForest(Number.of.Visits ~ ., data=training[,-2], importance=T)

rfbf # ok 30.3% var explained
rfbf2 # 18.3% cases do worse

#Most important variables
varImpPlot(rfbf)
varImpPlot(rfbf2)

# Predicting with the testing data
pred <- predict(rfbf, testing[,-1], type="response",
                norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

#Model looks bad
plot(exp(pred), exp(testing$Age.adjusted.rate), ylim=c(0,150), xlim=c(0,150),
     xlab="Pred 2015 asthma attacks rates", ylab="Obs 2015 asthma attacks rates")
abline(lm(exp(pred)~exp(testing$Age.adjusted.rate)), lty=2)
abline(1,1, col="red")

cor(pred, testing$Age.adjusted.rate)^2 #terrible fit 0.176

#MSE random forest VS just using the overal mean...15% better
mean((pred - testing$Age.adjusted.rate)^2)
mean((mean(training$Age.adjusted.rate) - testing$Age.adjusted.rate)^2)

# Learning curve of random forest
library(caret)
learning.c <- learing_curve_dat(training[,-1], outcome = "Age.adjusted.rate", 
                                proportion = (1:10)/10,
                                test_prop = 0, method = "rf", verbose = TRUE)

# Plot...Need more data
ggplot(learning.c, aes(x = Training_Size, y = RMSE, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()

# Trying boosting
library(gbm)
# Rate
boost.gaussian <- gbm(Age.adjusted.rate ~ ., data=training[,-1], distribution = "gaussian", 
                      n.trees=5000, interaction.depth = 4, shrinkage = 0.1)
# Count...does worse
boost.poisson <- gbm(Number.of.Visits ~ ., data=training[-2], distribution = "poisson", 
                     n.trees=5000, interaction.depth = 4, shrinkage = 0.1)

#Predicting
pred.boost.gaus <- predict(boost.gaussian, testing[,-1], n.trees=5000)
pred.boost.pois <- predict(boost.poisson, testing[,-1], n.trees=5000)

#MSE boost gaussian...very similar to RF
mean (( pred.boost.gaus - testing$Age.adjusted.rate ) ^2)
mean (( pred.boost.pois - testing$Number.of.Visits ) ^2)
cor(pred.boost.gaus, testing$Age.adjusted.rate)^2
cor(pred.boost.pois, testing$Number.of.Visits)^2
plot(exp(pred.boost.gaus), exp(testing$Age.adjusted.rate), ylim=c(0,150), xlim=c(0,150),
     xlab="Pred 2015 asthma attacks rates", ylab="Obs 2015 asthma attacks rates")
abline(1,1, col="red")
abline(lm(exp(pred.boost.gaus)~exp(testing$Age.adjusted.rate)), lty=2)

# Poisson looks awful
plot(pred.boost.pois, testing$Number.of.Visits, ylim=c(0,300), xlim=c(0,300),
     xlab="Pred 2015 asthma attacks rates", ylab="Obs 2015 asthma attacks rates")
abline(1,1, col="red")
abline(lm(pred.boost.pois~testing$Age.adjusted.rate), lty=2)


# HAVE TO TRY SOMETHING ELSE
# If fitting GLM, try interactions, cuadratic, cubic and to the 4th degree polynomials

# Multi-level Poisson model with Stan. It has to be neg binomial, as mean
# is 50.7 and var is 1185

# Creatig a pairwise difference in time matrix (time distance b/w obs)
dist.y <- dist(unique(d$Year), method = "maximum", diag = T, upper = T, p = 2)
dist.m.y <- as.matrix(dist.y)# It looks funny b/c years are not ordered

# Creatig a pairwise spatial distance matrix (spatial distance b/w obs)
coords <- cbind(d$zip, d$longitude, d$latitude)
coords <- coords[unique(d$zip),2:3]
dist.zips <- pointDistance(coords, lonlat = F)

# Creating list of data for Stan model
# Note that rates are X 10, to make ir integer. 
# Now it represents rates per 100,000
d.list <- list(N= length(d$rate), N_zip=length(unique(d$zip)), 
               N_year=length(unique(d$Year)), dist_year=dist.m.y, 
               Dmat=dist.zips,
               zip=as.integer(d$zip), year=d$Year, rate=as.integer(d$rate*10), 
               no2=as.vector(scale(d$no2, center=mean(d$no2), scale=sd(d$no2)*2)),
               co=as.vector(scale(d$co, center=mean(d$co), scale=sd(d$co)*2)),
               o3=as.vector(scale(d$o3, center=mean(d$o3), scale=sd(d$o3)*2)),
               pm10=as.vector(scale(d$pm10, center=mean(d$pm10), scale=sd(d$pm10)*2)),
               pm25=as.vector(scale(d$pm25, center=mean(d$pm25), scale=sd(d$pm25)*2)))
# Cuadratic terms and two way interactions
d.list$no2_sq <- d.list$no2^2
d.list$co_sq <- d.list$co^2
d.list$o3_sq <- d.list$o3^2
d.list$pm10_sq <- d.list$pm10^2
d.list$pm25_sq <- d.list$pm25^2
d.list$no2_co <- d.list$no2 * d.list$co
d.list$no2_o3 <- d.list$no2 * d.list$o3
d.list$no2_pm10 <- d.list$no2 * d.list$pm10
d.list$no2_pm25 <- d.list$no2 * d.list$pm25
d.list$co_o3 <- d.list$co * d.list$o3
d.list$co_pm10 <- d.list$co * d.list$pm10
d.list$co_pm25 <- d.list$co * d.list$pm25
d.list$o3_pm10 <- d.list$o3 * d.list$pm10
d.list$o3_pm25 <- d.list$o3 * d.list$pm25
d.list$pm10_pm25 <- d.list$pm10 * d.list$pm25

# Save the data in case it crashes

saveRDS(d.list, "data_list.rds")

# Cuadratic terms and two way interactions for data frame for RF
d$no2_sq <- d$no2^2
d$co_sq <- d$co^2
d$o3_sq <- d$o3^2
d$pm10_sq <- d$pm10^2
d$pm25_sq <- d$pm25^2
d$no2_co <- d$no2 * d$co
d$no2_o3 <- d$no2 * d$o3
d$no2_pm10 <- d$no2 * d$pm10
d$no2_pm25 <- d$no2 * d$pm25
d$co_o3 <- d$co * d$o3
d$co_pm10 <- d$co * d$pm10
d$co_pm25 <- d$co * d$pm25
d$o3_pm10 <- d$o3 * d$pm10
d$o3_pm25 <- d$o3 * d$pm25
d$pm10_pm25 <- d$pm10 * d$pm25


# Saving the data frame for later use

saveRDS(d, "data_ready.rds")


# Load saved list
d.list <- readRDS("data_list.rds")



# Getting .r file for upload to stan forum
rstan::stan_rdump(names(d.list), file = "my.data.R", envir = as.environment(d.list))

# Init values
n_chains <- 1
start <- list(a=0, a_zip_raw=rep(0, d.list$N_zip), bno2=0, bco=0, bo3=0, bpm10=0, bpm25=0,
              bno2_sq=0, bco_sq=0, bo3_sq=0, bpm10_sq=0, bpm25_sq=0,
              bno2_co=0, bno2_o3=0, bno2_pm10=0, bno2_pm25=0, bco_o3=0,
              bco_pm10=0, bco_pm25=0, bo3_pm10=0, bo3_pm25=0, bpm10_pm25=0)
              
init <- list()
for ( i in 1:n_chains ) init[[i]] <- start

# Model fitting. I need more than 16 gb of ram to run this model
# I'm screwed!
library(rstan)
m1.1 <- stan(model_code = stancode1, iter=4000, chains=1, cores=1, init=init,
             warmup=1000, control = list(adapt_delta = 0.8),
             data=d.list)

# Model checking

## Creating data set for trying Asthma rate as a function of previous year's
## rate plus pollutants

# Loading previously created file
data2 <- readRDS("data_ready.rds")
data2$rate <- data2$Age.adjusted.rate

# One data frame for each year
data.2009 <- data2[data2$Year==2009,c("zip", "rate")]
data.2012 <- data2[data2$Year==2012,c("zip", "rate")]
data.2013 <- data2[data2$Year==2013,c("zip", "rate")]
data.2014 <- data2[data2$Year==2014,c("zip", "rate")]
data.2015 <- data2[data2$Year==2015,]

# Merging the data frames
data3 <- merge(data.2009, data.2012, by="zip")
data3 <- merge(data3, data.2013, by="zip")
data3 <- merge(data3, data.2014, by="zip")
data3 <- merge(data3, data.2015, by="zip")

colnames(data3)[c(2:5, 11)] <- c("rate09", "rate12", "rate13", "rate14", "rate15")


# Remove unused vars
data.all <- data3[,-c(1, 6:10)]


# Split data in training and testing
r.s <- sample(nrow(data.all), nrow(data.all)*.8)
training <- data.all[r.s,]
testing <- data.all[-r.s,]


# Doing random forest with previous years + interpolated air pollutants
library(randomForest)
rfbf = randomForest(rate15 ~ ., data=training, importance=T)

# Compare with just using previous years...Just previous years is as good
rfbf_2 = randomForest(rate15 ~ ., data=training[,1:5], importance=T)

# Predicting with the testing data
pred <- predict(rfbf, testing, type="response",
                norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

pred_2 <- predict(rfbf_2, testing, type="response",
                norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE)


#Model looks good...but we know why
plot(exp(pred), exp(testing$rate15), ylim=c(0,150), xlim=c(0,150),
     xlab="Pred 2015 asthma attacks rates", ylab="Obs 2015 asthma attacks rates")
abline(lm(exp(pred)~exp(testing$rate15)), lty=2)
abline(1,1, col="red")

cor(pred, testing$rate15)^2 #good fit 0.85

#MSE random forest VS just using just previous years' rate
mse1 <- mean((pred - testing$rate15)^2)
mse2 <- mean((pred_2 - testing$rate15)^2)
mse3 <- mean((testing$rate14 - testing$rate15)^2)

mse1/mse2# about 9% better than just using previous years
mse1/mse3# about 18% better than just using previous years

#Most important variables
varImpPlot(rfbf)

# Creating object for the app

# Fit model with full dataset
rfbf_all = randomForest(rate15 ~ ., importance=T, data=data.all)
pred.all <- predict(rfbf_all, data.all, type="response",
                norm.votes=F, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

# Backtransforming from log scale to rate per 10,000
pred.all <- exp(pred.all)

# Creating data frame for app to work with
pred.all <- data.frame(pred.all, data3$zip)
colnames(pred.all) <- c("pred", "zip")

# Transforming zip code from factor to text
pred.all$zip.text <- as.character(pred.all$zip)  

# Save as RDS (saves object to be used later)
saveRDS(pred.all, "/home/tyatabe/Onedrive/Dev folder/AsthMap/data/pred.rds")

# load just to check
jjj <- readRDS("/home/tyatabe/Onedrive/Dev folder/AsthMap/data/pred.rds")
