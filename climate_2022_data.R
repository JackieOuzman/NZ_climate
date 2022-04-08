
# remove.packages("sf", sf)
# remove.packages("sp", sp)
# remove.packages("raster", raster)
# 

install.packages("sf")
#install.packages("sp")
#install.packages("raster")


library(raster)
library(sp)
library(sf)

library(ggplot2)
getwd()



sf::sf_extSoftVersion()


#lets load a raster
DOH_2013 <-raster("V:/Marlborough regional/climate/wetransfer_AS/Climate_tiff/Dateof200g20122013cor_SB_1.csv_B_gissmall_Ad.tif")

DOH_2013
slot(DOH_2013, "crs")

class(DOH_2013)
nlayers(DOH_2013)
plot(DOH_2013)
DOH_2013

#Bring in the bounadry using grid that Rob created
study_vineyards <- raster("V:/Marlborough regional/Boundary_files/marlb1.tif")
study_vineyards
crsInfo <- slot(study_vineyards, "crs")


st_crs(study_vineyards)
st_bbox(study_vineyards)
study_vineyards
plot(study_vineyards)


#for rasters:  To retrieve or describe the CRS:  projection(x)
# projection(x) <- CRS(“+init=epsg:28992”)
# To transform from one CRS to another:  newData <- projectRaster(x, proj4string(OtherData))

DOH_2013_proj <- projectRaster(DOH_2013, proj4string(study_vineyards))

DOH_2013_proj


mapCRS <- CRS("+init=epsg:2193")     # 2193 = New Zealand Transverse Mercator 2000 - New Zealand Transverse Mercator (NZTM)
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

#FROM - https://epsg.io/?q=New%20Zealand%20kind%3APROJCRS
#NZGD2000 / New Zealand Transverse Mercator 2000 - New Zealand Transverse Mercator (NZTM)
#EPSG:2193 with transformation: 1565


ggplot()+
  geom_sf(data = eden_valley, size = 1 , colour = "black")+
  coord_sf()
CRS("+init=epsg:4326")
CRS("+init=epsg:28354")


mapCRS <- CRS("+init=epsg:28354")     # 28355 = GDA_1994_MGA_Zone_54
wgs84CRS <- CRS("+init=epsg:4326")   # 4326 WGS 84 - assumed for input lats and longs

some_pts <- read.csv(file = "graph1_data.csv")
str(some_pts)
plot_some_pts <- st_as_sf(some_pts, coords = c("POINT_X", "POINT_Y"), 
                          crs = mapCRS)



ggplot() +
  geom_sf(data = eden_valley, size = 1 , colour = "black")+
  geom_sf(data = plot_some_pts) +
  coord_sf()+
  ggtitle("Map")
