# Code to calculate the range size of each terrestrial mammal species using the geographical distribution data available on IUCN (https://www.iucnredlist.org/; version 2021-2). The distribution polygons for each species are contained within the shapefile of each order (1-26).

library(dplyr)
library(tidyverse)
library(rgdal)
library(rgeos)
library(geosphere)


# First, let's following information from the 26 order shapefiles stored in IUCN_range_data folder:

for (i in 1:26){
sf<-readOGR(paste(dir()[1], sep="/",(paste(dir("IUCN_range_data")[i], sep="/", "data_0.shp"))))
n<-paste(dir("IUCN_range_data")[i], sep="_", "data_shape2.csv")
write.csv(sf@data, paste("IUCN_range_data_info", sep="/", n))}


# # 7 Dermoptera 2
# # 11 Hyracoidea 5
# # 13 Litopterna 3 - there is no shape for any spp
# # Macroscelidea   19 
# # 15 Microbiotheria 1
# # 16 Monotremata 7
# # 17 Notoryctemorphia 2 - there is no shape for any spp
# # 18 Notoungulata 3 - there is no shape for any spp
# # 19 Paucituberculata 7
# # Peramelemorphia 25 
# # Perissodactyla 29 
# # 22 Pholidota 9
# # Proboscidea 18
# # 28 Sirenia 5 - there is no shape for any spp
# # 29 Tubulidentata 1

 
#Combining all info data
folder<- "C:/Users/raque/OneDrive - ufpr.br/Doutorado/3ºCap/IUCN_range_data_info"

## csv's must all have identical column names.
filenames <- list.files(folder)
setwd(folder)
all_files <- Reduce(rbind, lapply(filenames, read.csv))

#This are all range type classifications from IUCN:
leg<-as.data.frame(table(all_files$LEGEND))

length(all_files$BINOMIAL)
length(unique(all_files$BINOMIAL))
#we have 5328 species and 11938 features of range.

# We will count the classifications with more than 50 features among all the species ranges to optimize the code. Afterward, we can calculate the specific species ranges for these particular classifications.
leg<- leg  %>%  filter (Freq>50)
leg<-data.frame(as.character(leg$Var1))
leg<-rbind("Binomial", leg)
colnames(leg)<-"range_IUCN"

# To account for the size of each feature with different classifications, we will create a table with the name of each species from the order in the rows and the classifications of the range in the columns, which will be filled with the size of each feature.
# Once the shapefiles are by order, our function has to calculate ranges by all species in each order.
# Since the unit of measurement returned by the gArea function is in square meters, to facilitate data manipulation, we divide the values by 10,000.

setwd("IUCN_range_data") # Directory where the shapefiles of each order are in folders with the name of the respective order.


for (j in 26) {

sha<-readOGR(paste(dir()[j], "data_0.shp", sep="/", collapse=NULL))
my_proj <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") #Albers Equal Area projection
sha <- spTransform(sha, my_proj)

o<-dir()[j]
o<-gsub("_2021","",as.character(o))

spec<-unique(sha$BINOMIAL)

rangeSize<-data.frame(matrix(nrow=length(spec), ncol=10))
leg$range_IUCN<- as.character(gsub(' ', '_', leg$range_IUCN))
colnames(rangeSize) <- leg$range_IUCN

for(i in 1:length(spec)){
  x <- sha[sha$BINOMIAL == spec[i], ]
  rangeSize$Binomial[i]<-spec[i] 
  try({rangeSize$'Extant_(resident)'[i]<-round(gArea(x[x$LEGEND== "Extant (resident)", ])/10000)})
  try({rangeSize$'Possibly_Extinct'[i]<-round(gArea(x[x$LEGEND== "Possibly Extinct", ])/10000)})
  try({rangeSize$'Extinct'[i]<-round(gArea(x[x$LEGEND== "Extinct", ])/10000)})
  try({rangeSize$'Presence_Uncertain'[i]<-round(gArea(x[x$LEGEND== "Presence Uncertain", ])/10000)})
  try({rangeSize$'Extant_&_Introduced_(resident)'[i]<-round(gArea(x[x$LEGEND== "Extant & Introduced (resident)", ])/10000)})
  try({rangeSize$'Extant_&_Origin_Uncertain_(resident)'[i]<-round(gArea(x[x$LEGEND== "Extant & Origin Uncertain (resident)", ])/10000)})
  try({rangeSize$'Extant_&_Reintroduced_(resident)'[i]<-round(gArea(x[x$LEGEND== "Extant & Reintroduced (resident)", ])/10000)})
  try({rangeSize$'Possibly_Extant_(resident)'[i]<-round(gArea(x[x$LEGEND== "Possibly Extant (resident)", ])/10000)})
  try({rangeSize$'Probably_Extant_(resident)'[i]<-round(gArea(x[x$LEGEND== "Probably Extant (resident)", ])/10000)})
  v<-paste(o, ".csv", sep="")
  p<-paste("IUCN_range_tables", v, sep="/")
  write.csv(rangeSize,p)} 
}

#The range size tables by orders are saved in the folder 'IUCN_range_tables'.
