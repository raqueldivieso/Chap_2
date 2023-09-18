
## Code to create the bivariate map: Figures 5B and 5C.

library(sf)
library(dplyr)
library(raster)
library(ggplot2)
library(raster)
library(ncdf4)
library(terra)

# Read the shapefile - this shapefile is one withh all mmammals species ranges combined
shapefile <- st_read("shapes/data_0.shp")

# Remove all uncertanty and extinct and possible extinct areas from shapefiles
leg<-data.frame(table(shapefile$LEGEND))
names_to_remove <- c("Extinct",
                     "Extinct & Origin Uncertain",
                     "Presence Uncertain",
                     "Probably Extant (resident)",
                     "Possibly Extant (passage)",
                     "Possibly Extant (resident)", 
                     "Presence Uncertain & Origin Uncertain") 

# Filter/remove polygons with the specified names
filtered_shapefile <- shapefile[!(shapefile$LEGEND %in% names_to_remove), ]

table(filtered_shapefile$LEGEND)

shapef<-filtered_shapefile
shapefile=NULL
filtered_shapefile=NULL
index<-read.csv("index_vulnerab-all_species.csv")
index<-dplyr::select(index, binomial, index_all)
hist(index$index_all, breaks=30)
index$index_all<-index$index_all+abs(min(index$index_all))
hist(index$index_all, breaks=30)

nspp<-unique(shapef$SCI_NAME)
# all_names <- all(nspp %in% index$binomial)
# table(all_names) #just one species 

##### Include the index as an atribute in the polygons
shapef$vul_index <- index$index_all[match(shapef$SCI_NAME, index$binomial)]
hist(shapef$vul_index)

# Include the IUCN category as an atribute in the polygons
index<-read.csv("index_vulnerab-all_species.csv")
cat<-dplyr::select(index, binomial, category)
shapef$category <- index$category[match(shapef$SCI_NAME, index$binomial)]

# Building a diversity map(raster 0.5x0.5 with spp number in each pixel)
shapef$diversity <- "1"
shapef$diversity<- as.numeric(shapef$diversity)
spec<- as.character(unique(index$binomial))

# r1<-brick("LUHa_u2t1.v1_image.v1.1_gsecd.nc4")
# r1<-r1[[69]]
# Get extent and resolution of the raster
# raster_extent <- extent(r1)
# raster_resolution <- res(r1)
# Create an empty raster
# empty_raster <- raster()
# Assign extent and resolution to the empty raster
# extent(empty_raster) <- raster_extent
# res(empty_raster) <- raster_resolution

ras <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)
rast <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)

for (i in 1:length(spec)) {
  tryCatch({
    try({x <- shapef[shapef$SCI_NAME == spec[i], ]})
    try({x$diversity<-as.numeric((x$diversity))})
    try({x <- rasterize(x, ras, x$diversity, touches=T)})
    try({rast<- sum(raster(x), rast, na.rm = T)})
    try({print(paste("spp:", i))})
  })
}
writeRaster(rast, filename = "Diversity_10x10.tif", format = "GTiff", overwrite=TRUE)

plot(rast)

# Raster with the vulnerability index sum for each cell
ras <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)
# Assign extent and resolution to the empty raster
# extent(ras) <- raster_extent
# res(ras) <- raster_resolution
# crs(ras) <- crs(shapef)

spec<- as.character(unique(index$binomial))
ras <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)
rast <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)

for (i in 1:length(spec)) {
  tryCatch({
    try({x <- shapef[shapef$SCI_NAME == spec[i], ]})
    try({x$vul_index<-as.numeric((x$vul_index))})
    try({x <- rasterize(x, ras, x$vul_index, touches=T)})
    try({rast<- sum(raster(x), rast, na.rm = T)})
    try({print(paste("spp:", i))})
  })
}
writeRaster(rast, filename = "Sum_Vul-index_10x10.tif", format = "GTiff", overwrite=TRUE)

# Median
spec<- as.character(unique(index$binomial))
ras <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)
rast <- rast(nrows=1800, ncols=3600, xmin=-180, xmax=180)

for (i in 1:100) {
  tryCatch({
    try({x <- shapef[shapef$SCI_NAME == spec[2], ]})
    try({x$vul_index<-as.numeric((x$vul_index))})
    try({x <- rasterize(x, ras, x$vul_index, touches=T)})
    r_sub <- stack(x, rast)
    try({rast<- median(raster(x), rast, na.rm = T)})
    try({print(paste("spp:", i))})
  })
}
# 
# s <- stack(m, raster(x))
# calc <- calc(s, median, na.rm = TRUE)
writeRaster(rast, filename = "Median_Vul-index_10x10.tif", format = "GTiff", overwrite=TRUE)

# To calculate the mean, we need to open both rasters and dived the sum for the species number.
rm(list=ls())
nspp<-raster("Diversity_10x10.tif")
sum<-raster("Sum_Vul-index_10x10.tif")

plot(nspp)
plot(sum)
mean<-sum/nspp
plot(mean)

writeRaster(mean, filename = "Vulnerab-index_mean_10x10.tif", format = "GTiff", overwrite=TRUE)

# Calculate the sum-index raster for a 50km resolution
shapefile <- st_read("C:/Users/raque/OneDrive - ufpr.br/shapefiles/All_mammals_shapes/data_0.shp")


r1<-brick("LUHa_u2t1.v1_image.v1.1_gsecd.nc4")
r1<-r1[[69]]
# Get extent and resolution of the raster
raster_extent <- extent(r1)
raster_resolution <- res(r1)
# Create an empty raster
rast <- raster()
# Assign extent and resolution to the empty raster
extent(rast) <- raster_extent
res(rast) <- raster_resolution
crs(rast)<-crs(shapefile)
# # ras<-rast(ras)
# rast<-ras


spec<- as.character(unique(index$binomial))

for (i in 1:length(spec)) {
  tryCatch({
    try({x <- shapef[shapef$SCI_NAME == spec[i], ]})
    try({x$vul_index<-as.numeric((x$vul_index))})
    try({x <- rasterize(x, ras, x$vul_index, touches=T)})
    try({rast<- sum(raster(x), rast, na.rm = T)})
    try({print(paste("spp:", i))})
  })
}
writeRaster(rast, filename = "Sum_Vul-index_50x50.tif", format = "GTiff", overwrite=TRUE)

plot(rast)



## Create the bivariate map- diversity x Potential vulnerability mean

rm(list=ls())
setwd("C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final")

#https://bluegreenlabs.org/post/map-building-3/
## Bivariate map
# install.packages("cartography")
# install.packages("paletteer")
# install.packages("rnaturalearth")
# install.packages("patchwork")
# install.packages("showtext")
# install.packages("rnaturalearthdata")
library(paletteer)
library(cartography)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)

u <-raster("Vulnerab-index_mean_10x10.tif")
plot(u)
u[u == 0] <- NA
valu<-as.data.frame(u, cells=TRUE)
hist(valu$Vulnerab.index_mean_10x10) 
max(valu,na.rm = TRUE) #1
min(valu,na.rm = TRUE) #0.08691236

min_u <- min(valu,na.rm = TRUE)
max_u <- max(valu,na.rm = TRUE)
new_min <- 0
new_max <- 100

u <- ((u - min_u) / (max_u - min_u)) * (new_max - new_min) + new_min
valu<-as.data.frame(u, cells=TRUE)
min(valu,na.rm = TRUE)
hist(valu$Vulnerab.index_mean_10x10)
plot(u)

t <-raster("Diversity_10x10.tif")
t[t == 0] <- NA
valt<-as.data.frame(t, cells=TRUE)
hist(valt$Diversity_10x10)
max(valt,na.rm = TRUE) #334.3169
min(valt,na.rm = TRUE) #0.2668412
# We need to transform both raster values for the same scale (0-100)

plot(t)

# Rescale the vulnerability raster
# Define the original range (1 to 350)
original_min <- min(valt,na.rm = TRUE)
original_max <- max(valt,na.rm = TRUE)

# Define the new range (1 to 100)
new_min <- 0
new_max <- 100

# Remap the values to the new range
t <- ((t - original_min) / (original_max - original_min)) * (new_max - new_min) + new_min
valt<-as.data.frame(t, cells=TRUE)
plot(t)
hist(valu$Vulnerab.index_mean_10x10)
hist(valt$Diversity_10x10)

# u <- rotate(u)
# t <- rotate(t)

plot(t)
plot(u)

robinson <- CRS("+proj=robin +over")
# download countries 
countries <- ne_countries(scale = 50, returnclass = c("sf"))

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)


# convert gridded raster dato dataframe
u_df <- u %>%
  projectRaster(., res=50000, crs = robinson) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "vuln"))

t_df <- t %>%
  projectRaster(., res=50000, crs = robinson) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "diver"))


quantile(u_df$vuln, probs = c(0.25, 0.5, 0.75)) # 18.20976   20.48825    23.25477    

quantile(t_df$diver, probs = c(0.25, 0.5, 0.75))  #10.17634  17.60641  28.15309  
# reclassify data using threshold values
# so we get 3 classes for each layer
u_df <- u_df %>%
  mutate(
    val = ifelse(vuln <= 18.20976, "1",
                 ifelse(vuln <= 20.48825, "2", (
                   ifelse(vuln >= 23.25477, "4", "3")))))
# hist(subset(u_df, val==1)$vuln)

t_df <- t_df %>%
  mutate(
    val = ifelse(diver <= 10.17634, "1",
                 ifelse(diver <= 17.60641, "2", (
                   ifelse(diver >= 28.15309, "4", "3")))))

# hist(subset(t_df, val==1)$diver)

# bind data by location and group
# by the value (index) created above
df <- left_join(t_df, u_df, by = c("x","y")) %>%
  mutate(
    group = paste(val.y, val.x, sep = " - ")
  ) %>%
  dplyr::select(-c(val.x, val.y))


# create a bivariate legend with indices
# matching those created above
legend_3 <- tibble(
  "4 - 4" = "#8b551d", 
  "4 - 3" = "#917930",
  "4 - 2" = "#C3A044",
  "4 - 1" = "#FE720B",
  "3 - 4" = "#978726", 
  "3 - 3" = "#979d43",
  "3 - 2" = "#D2BD6B",
  "3 - 1" = "#FCB14B",
  "2 - 4" = "#6e9f2c",
  "2 - 3" = "#8aa85a", 
  "2 - 2" = "#E0DA92",
  "2 - 1" = "#FCE47E",
  "1 - 4" = "#186D2E",  
  "1 - 3" = "#7CB271",
  "1 - 2" = "#B5D7A6",  
  "1 - 1" = "#f0f7c9",
) %>%
  gather("group", "fill")

# match the group constructed
# above with the colour scheme
# with 3 categories this is the
# plotting dataframe
df <- left_join(df, legend_3)


# create the main map
p1 <- ggplot()+
  geom_raster(
    data = df,
    aes(
      x=x,
      y=y,
      fill=fill
    ),
    interpolate = TRUE
  ) +
  scale_fill_identity() +
  # geom_point(data = b, aes(x=longitude, y=latitude), size = 0.2, col="brown")+
  geom_sf(data=countries_robinson,
          colour='grey25',
          linetype='solid',
          fill= NA,
          size=0.1) +
  geom_sf(data=bb_robinson,
          colour='white',
          linetype='solid',
          fill = NA,
          size=0.4)+
  theme_void() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(
          color = "grey30",
          size = 40,
          hjust = 0.1),
        plot.title = element_text(
          family = "Prata",
          color = "grey30",
          size = 70,
          hjust = 0.1),
        plot.caption = element_text(
          color = "grey30",
          size = 25,
          lineheight = 0.3),
        plot.margin = margin(r = 10)
  )

p1
# create the legend
p2 <- legend_3 %>%
  separate(group,
           into = c("land", "vuln"),
           sep = " - ") %>%
  mutate(vuln = as.integer(vuln),
         land = as.integer(land)) %>%
  ggplot() +
  geom_tile(mapping = aes(
    x = land,
    y = vuln,
    fill = fill)) +
  scale_fill_identity() +
  labs(x = "",
       y = "") +
  theme_void() +
  theme(
    axis.title = element_text(
      size = 23,
    ),
    axis.title.y = element_text(angle = 90)) +
  coord_fixed()
p2

tiff('C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final/Mapas_qgis/Map_FIG4_A.tiff',  units="in", width=8, height=6.4, res=1000, compression = 'lzw')
p1
dev.off()
tiff('C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final/Mapas_qgis/Leg_FIG4_A.tiff',  units="in", width=3, height=3, res=1000, compression = 'lzw')
p2
dev.off()

# Ploting a histogram with the frequency of the species in the first quantile for the most vulnerables

# points<-read.csv("last_quantile_point.csv")
# points<-na.omit(points)
# points_sf <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
# hist(points$latitude)
# p <-ggplot(points, aes(x=latitude)) + geom_histogram(fill="gray30")+
# geom_vline(aes(xintercept=0),
#                 color="white", linewidth=1)+
# theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# p + coord_flip()

rob_cont<-as(countries_robinson, 'Spatial')
class(rob_cont[1])
writeOGR(rob_cont[1], "C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final/Mapas_qgis", "contries_Robinson2", 
         driver = "ESRI Shapefile") 

## Second bivariate map - sum of vulnerability and future changes in land use

rm(list=ls())
library(paletteer)
library(cartography)

#https://bluegreenlabs.org/post/map-building-3/
# install.packages("rnaturalearth")
# install.packages("patchwork")
# install.packages("showtext")
library(tidyverse)
library(raster)
library(rnaturalearth)
library(sf)

u <-raster("Sum_Vul-index_50x50.tif")

u[u == 0] <- NA
valu<-as.data.frame(u, cells=TRUE)
hist(valu$Sum_Vul.index_50x50) 
max(valu,na.rm = TRUE) #349.0706
min(valu,na.rm = TRUE) #0.08691236

min_u <- min(valu,na.rm = TRUE)
max_u <- max(valu,na.rm = TRUE)
new_min <- 0
new_max <- 100

u <- ((u - min_u) / (max_u - min_u)) * (new_max - new_min) + new_min
valu<-as.data.frame(u, cells=TRUE)
min(valu,na.rm = TRUE)
hist(valu$Sum_Vul.index_50x50)
plot(u)

countries <- ne_countries(scale = 50, returnclass = c("sf"))
plot(countries$geometry, add=T)
t <-raster("PROJECTED_LAND_COVER_50Y.tif") 
plot(t)
t<-mask(t,countries)
plot(t)
valu<-as.data.frame(t$PROJECTED_LAND_COVER_50Y, cells=TRUE)
hist(valu$PROJECTED_LAND_COVER_50Y)

t[t == 0] <- 0.000001
valt<-as.data.frame(t, cells=TRUE)
hist(valt$PROJECTED_LAND_COVER_50Y)
max(valt,na.rm = TRUE) #1
min(valt,na.rm = TRUE) #0e-06
plot(t)

# We need to transform both raster values for the same scale (0-100)
# Rescale the land cover raster
t<-t$PROJECTED_LAND_COVER_50Y
t<- t*100
plot(t)
# hist(t$PROJECTED_LAND_COVER_50Y)

plot(t)
plot(u)

robinson <- CRS("+proj=robin +over")
# download countries 
countries <- ne_countries(scale = 50, returnclass = c("sf"))

# create a bounding box for the robinson projection
# we'll use this as "trim" to remove jagged edges at
# end of the map (due to the curved nature of the
# robinson projection)
bb <- sf::st_union(sf::st_make_grid(
  st_bbox(c(xmin = -180,
            xmax = 180,
            ymax = 90,
            ymin = -90), crs = st_crs(4326)),
  n = 100))
bb_robinson <- st_transform(bb, as.character(robinson))

# transform the coastline to robinson
countries_robinson <- st_transform(countries, robinson)


# convert gridded raster dato dataframe
u_df <- u %>%
  projectRaster(., res=50000, crs = robinson) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "vuln"))

t_df <- t %>%
  projectRaster(., res=50000, crs = robinson) %>%
  rasterToPoints %>%
  as.data.frame() %>%
  `colnames<-`(c("x", "y", "land"))

quantile(u_df$vuln, probs = c(0.25, 0.5, 0.75)) # 7.794349   12.907998     24.221492 

quantile(t_df$land, probs = c(0.25, 0.5, 0.75))  #7.244358   21.906373   49.697064  
# reclassify data using threshold values
# so we get 3 classes for each layer
u_df <- u_df %>%
  mutate(
    val = ifelse(vuln <= 7.794349 , "1",
                 ifelse(vuln <= 12.907998 , "2", (
                   ifelse(vuln >= 24.221492 , "4", "3")))))
# hist(subset(u_df, val==1)$vuln)

t_df <- t_df %>%
  mutate(
    val = ifelse(land <= 7.244358, "1",
                 ifelse(land <= 21.906373, "2", (
                   ifelse(land >= 49.697064, "4", "3")))))
# hist(subset(t_df, val==1)$diver)

# bind data by location and group
# by the value (index) created above
df <- left_join(t_df, u_df, by = c("x","y")) %>%
  mutate(
    group = paste(val.y, val.x, sep = " - ")
  ) %>%
  dplyr::select(-c(val.x, val.y))



# create a bivariate legend with indices
# matching those created above
legend_3 <- tibble(
  "4 - 4" = "#3F2949", 
  "3 - 4" = "#435786",
  "2 - 4" = "#466EA4",
  "1 - 4" = "#4885C1",
  "4 - 3" = "#69304B", 
  "3 - 3" = "#806A8A",
  "2 - 3" = "#7681A9",
  "1 - 3" = "#89A1C8",
  "4 - 2" = "#93364D",
  "3 - 2" = "#9E738D", 
  "2 - 2" = "#A594AD",
  "1 - 2" = "#AAB0CC",
  "4 - 1" = "#d02b48",  
  "3 - 1" = "#CA647C",
  "2 - 1" = "#C39DB0",  
  "1 - 1" = "#CABED0",
) %>%
  gather("group", "fill")

# match the group constructed
# above with the colour scheme
# with 3 categories this is the
# plotting dataframe
df <- left_join(df, legend_3)


# create the main map
p1 <- ggplot()+
  geom_raster(
    data = df,
    aes(
      x=x,
      y=y,
      fill=fill
    ),
    interpolate = TRUE
  ) +
  scale_fill_identity() +
  # geom_point(data = b, aes(x=longitude, y=latitude), size = 0.2, col="brown")+
  geom_sf(data=countries_robinson,
          colour='grey25',
          linetype='solid',
          fill= NA,
          size=0.1) +
  geom_sf(data=bb_robinson,
          colour='white',
          linetype='solid',
          fill = NA,
          size=0.4)+
  theme_void() +
  theme(legend.position = 'none',
        plot.subtitle = element_text(
          color = "grey30",
          size = 40,
          hjust = 0.1),
        plot.title = element_text(
          family = "Prata",
          color = "grey30",
          size = 70,
          hjust = 0.1),
        plot.caption = element_text(
          color = "grey30",
          size = 25,
          lineheight = 0.3),
        plot.margin = margin(r = 10)
  )


p1
# create the legend
p2 <- legend_3 %>%
  separate(group,
           into = c("land", "vuln"),
           sep = " - ") %>%
  mutate(vuln = as.integer(vuln),
         land = as.integer(land)) %>%
  ggplot() +
  geom_tile(mapping = aes(
    x = land,
    y = vuln,
    fill = fill)) +
  scale_fill_identity() +
  labs(x = "",
       y = "") +
  theme_void() +
  theme(
    axis.title = element_text(
      size = 23,
    ),
    axis.title.y = element_text(angle = 90)) +
  coord_fixed()
p2

tiff('C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final/Mapas_qgis/Map_FIG4_A.tiff',  units="in", width=8, height=6.4, res=1000, compression = 'lzw')
p1
dev.off()
tiff('C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final/Mapas_qgis/Leg_FIG4_B.tiff',  units="in", width=3, height=3, res=1000, compression = 'lzw')
p2
dev.off()

# The combination of the three maps and the respective labels were arranged in an image editor.