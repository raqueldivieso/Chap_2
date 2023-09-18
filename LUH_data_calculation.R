## Land use future transitions to build the Figure 4C.

library(terra)

states<- terra::rast("multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp119-2-1-f_gn_2015-2100.nc")
summary(states)
# primf= (primary land) forest
# prin= (primary land) non-forest
# plot(states$primf_9) #the 9º layer corresponds to the data for 2023.
# plot(states$primf_59) #the 59º layer corresponds to the data for 2073.
# plot(states$primn_9)
# plot(states$primn_59)
l_9 <- mosaic(states$primf_9, states$primn_9, fun = "sum") #combining forest and non-forest layers to account for all primary land use. 
# plot(l_9)
l_59 <- mosaic(states$primf_59, states$primn_59, fun = "sum") #the same for 2073
# plot(l_59)
writeRaster(l_9-l_59, "PROJECTED_LAND_COVER_50Y.tiff") #subtracting the layers to get the difference (the loss of primary land) between 50 years 

