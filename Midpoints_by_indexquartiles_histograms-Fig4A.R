
## Estimating the midpoint of each species in each of the four quantile of the index with the respective IUCN vulnerability classification.

library(raster)
library(rgeos)
library(terra)
library(dplyr)
library(geosphere)
library(stringr)
library(sf)
library(ggplot2)

# Read the shapefile with geographical distribution for all mammals species from IUCN data
shapefile <- st_read("All_mammals_shapes/data_0.shp")

#Remove all uncertain, extinct and possible extinct areas from polygons:
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

# table(filtered_shapefile$LEGEND)

shapef<-filtered_shapefile
shapefile=NULL
filtered_shapefile=NULL

# Include the index and IUCN category as an atribute in each polygons:
index<-read.csv("index_vulnerab-all_species.csv")
shapef$vul_index <- index$potvuln_index[match(shapef$SCI_NAME, index$binomial)]
shapef$category <- index$category[match(shapef$SCI_NAME, index$binomial)]

q<-quantile(index$potvuln_index, probs = c(0.25, 0.5, 0.75))

index<- index %>%
  mutate(
    val = ifelse(potvuln_index <= -1.1121707, "1",
                 ifelse(potvuln_index <= -0.5561464, "2", (
                   ifelse(potvuln_index >= 0.2298773, "4", "3")))))

prim<-subset(index, val=="1")
sec<-subset(index, val=="2")
ter<-subset(index, val=="3")
quar<-subset(index, val=="4")
spp<-quar$binomial
quar_quantile_point<-data.frame(matrix(nrow=length(spp), ncol=4))
colnames(quar_quantile_point)<-c("binomial", "lat", "long", "category")

for (i in 1:length(spp)){
  tryCatch({
    x <- shapef[shapef$SCI_NAME == spp[i], ]
    try ({x$area <- st_area(x)})
    try ({f<-x[which.max(x$area), ]})
    try ({a<-as.character(st_centroid(f$geometry))})
    try ({a <- gsub('c','',a)})
    try ({b<-gsub("[()]", "", a)})
    try ({b<-str_split_fixed(b,", ", 2)})
    try ({quar_quantile_point$binomial[i]<-spp[i]})
    try ({quar_quantile_point$lat[i]<-b[,1]})
    try ({quar_quantile_point$long[i]<-b[,2]})
    try ({quar_quantile_point$category[i]<-x$category})
    try ({write.csv(quar_quantile_point, "quar_quantile_points.csv")})})}

# + + : North and East
# + - : North and West
# - - : South and West
# - + : South and East


# Figure 4A
# Histograms of each quartile of the "potential vulnerability index" by latitude;
# Eastern and Western hemispheres
# Combining the 4 tables
one<-read.csv("first_quantile_points.csv")
one<-rename(one, longitude=lat, latitude=long)
one$qua<-"1"
one<- one %>%
  mutate(
    side = ifelse(longitude <= 0, "east", #East (+)
                  "west")) # or West(-) 
two<-read.csv("sec_quantile_points.csv")
two<-rename(two, longitude=lat, latitude=long)
two$qua<-"2"
two<- two %>%
  mutate(
    side = ifelse(longitude <= 0, "east", #East (+)
                  "west")) # or West(-) 
tri<-read.csv("ter_quantile_points.csv")
tri<-rename(tri, longitude=lat, latitude=long)
tri$qua<-"3"
tri<- tri %>%
  mutate(
    side = ifelse(longitude <= 0, "east", #East (+)
                  "west")) # or West(-) 
four<-read.csv("quar_quantile_points.csv")
four<-rename(four, longitude=lat, latitude=long)
four$qua<-"4"
four<- four %>%
  mutate(
    side = ifelse(longitude <= 0, "east", #East (+)
                  "west")) # or West(-) 

lat<-c(one$latitude, two$latitude, tri$latitude, four$latitude)
max<-max(lat, na.rm=T)
min<-min(lat, na.rm=T)

one_east<-subset(one, side=="east")
one_east$fill<- "A"
two_east<-subset(two, side=="east")
two_east$fill<- "A"
tri_east<-subset(tri, side=="east")
tri_east$fill<- "B"
four_east<-subset(four, side=="east")
four_east$fill<- "B"

one_west<-subset(one, side=="west")
one_west$fill<- "A"
two_west<-subset(two, side=="west")
two_west$fill<- "A"
tri_west<-subset(tri, side=="west")
tri_west$fill<- "B"
four_west<-subset(four, side=="west")
four_west$fill<- "B"

east<-rbind(one_east,two_east, tri_east, four_east)
west<-rbind(one_west,two_west, tri_west, four_west)

east$lat_g <- as.numeric(cut(east$latitude, 20))
hist(subset(east, lat_g=="1")$latitude)

#in this categorization of latitudes, the following numbers correspond to each on of the 20 intervals:
conf<-cbind(east$lat_g, east$latitude)
conf[which.min(conf[,2]),] #The lowest lat is -53.96 and corresponds to the interval 1º.
conf[which.max(conf[,2]),] #The lowest lat is 82.71 and corresponds to the interval 20º
filtered_df1 <- east[east$latitude > -1 & east$latitude  < 1, ] #and the zero latitude corresponds to the 8º interval.
#The same for west hemispher
west$lat_g <- as.numeric(cut(west$latitude, 20))
# hist(subset(west, lat_g=="1")$latitude)
conf2<-cbind(west$lat_g, west$latitude)
conf2[which.min(conf2[,2]),] #The lowest lat is -42.02 and corresponds to the interval 1/20.
conf2[which.max(conf2[,2]),] #The lowest lat is 82.88 and corresponds to the interval 20/20.
filtered_df2 <- west[west$latitude > -1 & west$latitude  < 1, ] #and the zero latitude corresponds to the 7º interval.

#This is important to precisely align the graphs with the map!

d<-rbind(east,west)
d$lat_gg <- as.numeric(cut(d$latitude, 20))

w<-ggplot(west) + 
  geom_bar(aes(x = lat_g, fill = factor(qua)))+
  scale_fill_manual(values=c('#bededa', '#62c8c8','#39a4a4', '#1e6e79'))+
  theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_vline(xintercept=7, col="white", linewidth=0.001)+
  coord_flip()

ggsave("legenda_mapa_lat.png", w, dpi = 600,  width = 40.8, height = 36.1, bg = "transparent")


e<-ggplot(east) + 
  geom_bar(aes(x = lat_g, fill = factor(qua)))+
  scale_fill_manual(values=c('#bededa', '#62c8c8','#39a4a4', '#1e6e79'))+theme(panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_vline(xintercept=8, col="white", linewidth=0.001)+
  coord_flip()+ theme(legend.position = "none")

ggsave("east-hist_pisc_ok.png", e, dpi = 600,  width = 40.8, height = 36.1, bg = "transparent")


