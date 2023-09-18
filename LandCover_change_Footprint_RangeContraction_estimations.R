## Accounting for changes in land use for each species;
# Vegetation changes rasters were obtained in Song et al., 2018 (https://doi.org/10.1038/s41586-018-0411-9)
# Range polygons with the distribution of each species were obtained from IUCN data (https://www.iucnredlist.org/).

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)
library(dplyr)


ras<-raster("Short_vegetation_change_1982-2016.tif")
ras2<-raster("Tree_canopy_change_1982-2016.tif")

fold<- list.files("IUCN_range_data")
orders<-unlist(str_split(fold, "_"))
orders<- split(orders, 1:2)$`1`

mar<-read.csv("Marine_spp.csv")

for (j in 1:26) {
  
setwd("IUCN_range_data")
sha<-readOGR(paste(paste(o,"_2021", sep=""), "data_0.shp", sep="/", collapse=NULL))
proj4string(sha)<-ras$Short_vegetation_change_1982.2016@crs

x<-data.frame(unique(sha$BINOMIAL)) #BINOMIAL
x<-rename(xis, binomial=unique.sha.BINOMIAL.) #
marinhas<-merge(x, mar, by="binomial", all.x=T)
marinhas[is.na(marinhas)] <- 0
spp<- marinhas %>% filter(habitat=="0")
spp<-spp$binomial 

change<-data.frame(matrix(nrow=length(spp), ncol=5))
colnames(change)<-c("binomial", "Short_vegetation_change", "Tree_Canopy_change", "n_pixels", "change_perc")


for(i in 1:length(spp)){
try({
x <- sha[sha$BINOMIAL == spp[i], ] #BINOMIAL
ras_crop<-crop(ras,x)               
Env_shape1<-mask(ras_crop, x)

col_vec <- data.frame(matrix(c(as.matrix(Env_shape1)), ncol = 1))
sum(col_vec$matrix.c.as.matrix.Env_shape1....ncol...1., na.rm = T)
ras_crop2<-crop(ras2,x)               
Env_shape2<-mask(ras_crop2, x)

# s<-sum(as.matrix(Env_shape1), na.rm = T)
col_vec <- data.frame(matrix(c(as.matrix(Env_shape1)), ncol = 1))
# sum(col_vec$matrix.c.as.matrix.Env_shape1....ncol...1., na.rm = T)
col_vec2 <- data.frame(matrix(c(as.matrix(Env_shape2)), ncol = 1))
# sum(col_vec2$matrix.c.as.matrix.Env_shape2....ncol...1., na.rm = T)

data<-as.data.frame(cbind(col_vec[,1], col_vec2[,1]))

# If there was an increase in short vegetation this can be the result of decrease in tree canopy, consider only a real increase of short vegetation when the tree canopy did not experience any decrease. 

data<-na.omit(data)
colnames(data)<-c("Short_vegetation", "Tree_canopy")

data$ab<-ifelse(data$Short_vegetation>=0 & data$Tree_canopy>=0, 0, "FALSE")
data$cd<-ifelse(data$Short_vegetation<0  & data$Tree_canopy<0, ifelse(data$Short_vegetation<data$Tree_canopy,data$Short_vegetation, data$Tree_canopy), "FALSE")
data$ef<-ifelse(data$Short_vegetation>=0  & data$Tree_canopy<0, data$Tree_canopy, "FALSE")
data$g<-ifelse(data$Short_vegetation<0  & data$Tree_canopy>=0 & abs(data$Short_vegetation)>data$Tree_canopy,data$Short_vegetation+data$Tree_canopy , "FALSE")
data$h<-ifelse(data$Short_vegetation<0  & data$Tree_canopy>=0 & abs(data$Short_vegetation)<data$Tree_canopy, 0, "FALSE")
dat<-data
dat[dat=="FALSE"]<-NA
dat <- dat %>% mutate_at(c("ab", "cd", "ef", "g", "h"), as.numeric)
dat$veg_loss<-rowSums(dat[,3:7], na.rm = TRUE)

sum<-sum(dat$veg_loss)

change$Short_vegetation_change[i]<-sum(data$Short_vegetation)
change$Tree_Canopy_change[i]<-sum(data$Tree_canopy)
nc<-ncell(Env_shape1) - freq(Env_shape1, value=NA)
change$binomial[i]<- spp[i]
change$n_pixels[i]<-nc
change$change_perc[i]<-sum*(100/nc)/100})
  
n<-paste(orders[j],"vegetation_loss7.csv", sep="_")
write.csv(change, paste("Land_cover_change_tables", n, sep="/"))} 
}



## Calculating the footprint for each species:

# Footprint rasters for the year 2018 were obtained from Mu et al., 2022 (https://doi.org/10.1038/s41597-022-01284-8).

ras<-raster("footprint_2018/hfp2018.tif")
fold<- list.files("IUCN_range_data")
orders<-unlist(str_split(fold, "_"))
orders<- split(orders, 1:2)$`1`
mar<-read.csv("Marine_spp.csv")

for (j in 1:26) {
o<- orders[j]

setwd("IUCN_range_data")
sha<-readOGR(paste(paste(o,"_2021", sep=""), "data_0.shp", sep="/", collapse=NULL))
sha <- spTransform(sha, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
xis<-data.frame(unique(sha$BINOMIAL)) #BINOMIAL
xis<-rename(xis, binomial=unique.sha.BINOMIAL.) #
marin<-merge(xis, mar, by="binomial", all.x=T)
marin[is.na(marin)] <- 0
spp<- marin %>% filter(habitat=="0")
spp<-spp$binomial 

footp<-data.frame(matrix(nrow=length(spp), ncol=2))
colnames(footp)<-c("binomial", "footprint2018")

for(i in 1:length(spp)){
  try({
    x <- sha[sha$BINOMIAL == spp[i], ] #BINOMIAL
    ras_crop<-crop(ras,x)               
    Env_shape1<-mask(ras_crop, x)
    
    col_vec <- data.frame(matrix(c(as.matrix(Env_shape1)), ncol = 1))
    footp$footprint2018[i]<-mean(col_vec$matrix.c.as.matrix.Env_shape1....ncol...1., na.rm = T)
    footp$binomial[i]<- spp[i]})
  
  n<-paste(orders[j],"footprint2.csv", sep="_")
  write.csv(footp, paste("footprint_2018", n, sep="/"))}
}


## Calculating the % of contraction in range pixels from expected and observed ranges 
# Change in cells PHYLACINE

# Spatial_metadata.csv is provided by PHYLACINE 1.2 (Faurby, S. et al. PHYLACINE 1.2: The Phylogenetic Atlas of Mammal Macroecology. Ecology 99, 2626 (2018).)

cel_ex<-read.csv("data/Spatial_metadata.csv")
head(cel_ex)

cel_ex$percent<-cel_ex$Change.In.Cells*100/cel_ex$Number.Cells.Present.Natural.Range
cel_ex$neg<-ifelse(cel_ex$Number.Cells.Current.Range>cel_ex$Number.Cells.Present.Natural.Range, TRUE, FALSE)
cel_ex$percent <- cel_ex$percent %>% replace(is.na(.), 0)
table(cel_ex$neg)
cel_ex$percentagem<-ifelse(cel_ex$neg==TRUE, 0, cel_ex$percent)
table(is.na(cel_ex$percentagem))
lost_area<-select(cel_ex, Binomial.1.2, percentagem )
colnames(lost_area)<-c("binomial", "area_loss")
write.csv(lost_area, "data/losted_area.csv")



