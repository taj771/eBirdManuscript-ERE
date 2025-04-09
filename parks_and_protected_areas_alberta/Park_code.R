rm(list = ls())
library(rgdal)
library(ggplot2)
library(raster)
library(sf)
library(data.table)
library(maptools)
library(maps)
library(rgeos)
library(tidyverse)
library(GISTools)
library(sp)
library(rgeos)
library(polyclip)
library(spatialEco)
library(dplyr)

#read shape file
shp <- readOGR(dsn = ".","Parks_Protected_Areas_Alberta" )
shp <-  spTransform(shp,CRS("+proj=longlat +datum=WGS84 +no_defs"))
st_crs(shp)
parks_df <- data.frame(shp)
write.csv(parks_df, "./parks_alberta.csv")

#read data table
hot_spot = fread("ab-ebd-hotspot-locations.csv")
hot_spot = as.data.table(hot_spot)
Points_Shape = SpatialPoints(hot_spot[,c("longitude","latitude")],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
Points_Shape = SpatialPointsDataFrame(Points_Shape,hot_spot)
plot(Points_Shape)

plot(shp)
points(Points_Shape, pch = 20, col = "orange")

pts.poly <- point.in.poly(Points_Shape, shp)
head(pts.poly@data)
write.csv(pts.poly, "./pts_poly.csv")
parks <- read.csv("./pts_poly.csv")

library(dplyr)
df = subset(parks, select = c(locality_id, TYPE))


df = df%>%
  mutate(TYPE = case_when(TYPE == "NP"~ "nat", #national parks
                           TYPE == "WA"~"prov", #wilderness area
                           TYPE == "ER"~"prov", #ecological reserve
                           TYPE == "WP"~"prov", #willmore Wilderness park
                           TYPE == "WPP"~"prov", #wildland park
                           TYPE == "PP"~"prov", #provincial park
                           TYPE == "HR"~"prov", #Heritage Rangeland 
                           TYPE == "NA"~"prov", #natural area
                           TYPE == "PRA"~"prov" )) #Provincial recreation area

df[is.na(df)] = "out"
write.csv(df, "./type_of_hotspt.csv")
