# mapping SST in Med and Cyprus with cool color scale

library(ncdf4)
library(raster)
library(rasterVis) #to plot the map with levelplot
library(RColorBrewer)#to play with colour scales
library(dplyr)
library(ggplot2)

###### data download ######
## To run this script download first SST data from copernicus as netcdf and safe it to hard disk!

# This link leads to monthly SST for the global ocean:
# https://data.marine.copernicus.eu/product/GLOBAL_ANALYSISFORECAST_PHY_001_024/download?dataset=cmems_mod_glo_phy-thetao_anfc_0.083deg_P1M-m
# For SST there are hourly values (ends with PT6H), daily (D) and monthly (M)
# I defined coordinates as N=48, W=-25, E=42, S=-5 and time range from 16.01.2021 to 16.11.2022
# good to crop the layers already on the website to safe storage
# safe the data in your working directory and rename it 
#to get the files as raster layers load them as a stack 

#########################################
###### mapping the Levantine Basin ######
#########################################

##To plot the map:
sst_Lev <- stack("output_data/cmems_mod_glo_phy-thetao_anfc_0.083deg_P1M-m_1768641060298.nc") #Levantine Basin

# first rename layers in stack by date
dates <- format(seq(as.Date('2022/06/01'), as.Date('2022/12/01'), by='month'), '%Y%m')
dates[1:7]#check 
names(sst_Lev) <- paste0("", dates)

### AUGUST ###
##get raster layer for august 2021
sst_Lev_aug <- sst_Lev$X202208
x11()
plot(sst_Lev_aug)

# use geom raster in ggplot
# for this you have to first create a dataframe of your raster layer
sst_Lev_aug_df <-raster::as.data.frame(sst_Lev_aug, xy = TRUE) %>%
  na.omit() 
#change the variable name
colnames(sst_Lev_aug_df)[3]<-"sst"

#here you can adjust the colours you want
mycol <- c("darkblue", "cyan4", "chartreuse3", "yellow", "red", "red4")

#plot and safe as pdf
ggplot(sst_Lev_aug_df,aes(x = x, y = y)) +
  geom_raster(aes(fill = sst),interpolate=FALSE) +
  scale_fill_gradientn(colours=mycol,guide="colourbar") +
  labs(title="SST August 2022", x="Longitude (°)",y="Latitude (°)") +
  coord_fixed(ratio = 1,expand = FALSE, clip = "on")+#to fix coordinate ratio in plot
  theme(text=element_text(size=11),
        panel.background = element_rect(fill="darkgrey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
