###17.10.2022
#Script to read .nc files, filter data, extract data and generate graphs
packages<-c("ncdf4","raster","rgdal","ggplot2","tidyverse","reshape2")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

setwd("F:/Brian/Maestria_KFU")

points<-read_csv("ru.csv") %>% select(city,lat,lng) #upload our file to R and select 3 columns
cities<-points[1:12,] #here we take the first 12 rows
#cities <- cities[-c(3), ] 

#summarise(cities, group_by(city))
nc_data <- nc_open('air.2m.gauss.2020_daily.nc') #our netCDF file

lon <- ncvar_get(nc_data, "lon") #extract lon
lat <- ncvar_get(nc_data, "lat", verbose = F) #extract lat
t <- ncvar_get(nc_data, "time") #extract time

names(nc_data$var)  #variables in our file


air_2020 <- ncvar_get(nc_data, "air") # store the data in a 3-dimensional array
dim(air_2020) #144  73  17 366

fillvalue <- ncatt_get(nc_data, "air", "_FillValue") #create second data set for fill values
fillvalue
nc_close(nc_data) #close .nc file

###

air_2020[air_2020 == fillvalue$value] <- NA
air_2020<-air_2020-273 #convert kelvin to Celsius

air_slice <- air_2020[, , 366]  #we choose one date/level for mapping
#map<-air_2020[, , 1,366] #lets map one day (last day of data) in case of 4 dimension file
dim(air_slice)

r <- raster(t(air_slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r)

points(cities$lng,cities$lat, pch=19, cex = 0.5, col="Red" ) #add points ot our map
r@crs #verify our coordinate system
centroid_spdf <- SpatialPointsDataFrame(cities[,2:3], proj4string=r@crs, cities) #convert points to spatialdatafram
#writeRaster(r, "name_raster.tif", "GTiff", overwrite=TRUE)  #use this line if you want to save this raster

##Stack  rasters in one
r_brick <- brick(air_2020, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_brick<-t(r_brick)

stations_data <- raster::extract(r_brick,         # raster layer
                                 centroid_spdf,   # SPDF with centroids for buffer
                                 #buffer = 0,     # buffer size, units depend on CRS
                                 #fun=mean,         # what to value to extract
                                 df=TRUE)         # return a dataframe? 

View(stations_data)
# Transform our data into dataframe 
toolik_df <- data.frame(Day= seq(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"),  "day"), Temp=t(stations_data[,-1]))
#add manually code stations/name of your points location
colnames(toolik_df)<-c("Day", "Moscow","Saint Petersburg","Novosibirsk","Yekaterinburg","Nizhniy Novgorod","Kazan","Chelyabinsk","Omsk","Samara","Rostov","Ufa","Krasnoyarsk")

data_long <- melt(toolik_df, id = "Day") #Transform data in 3 columns.
#code for plotting daily data.
ggplot(data_long,            
       aes(x = Day,
           y = value,
           color = factor(variable))) +  geom_line(aes(group = variable))+
  geom_line(size=0.8)+
  labs(x = "Days" , y = "Temperature °C" , color = "Cities",title = "2020 mean Daily Air temperature at 2 m") #change titles

write.csv(data_long,"data_cities_daily_temperature_2m.csv") ##Export data in csv to your directory
