#plot_strandings

rm(list=ls())
library(maps)          # creating geographical maps
# library(mapdata)       # go with maps - contains topo and geologic data
# library(mapproj)       # for creating projected maps
library(sp)            # classes and methods for spatial data
# library(maptools)      # tools for reading and handling spatial objects
# library(gpclib)        # can't install because needs to be compiled... 5/18/2016
# library(sfsmisc)       # utilities from Seminar fuer Statistik ETH Zurich
# library(raster)        # tools to deal with raster images
# library(rgeos)         # interface to geometry engine
library(rgdal)         # bindings for the geospatial data abstraction library
# library(scales)        # making transparency
#
library(viridis)
library(ggmap)
# library(ggplot2)
# library(ggsn)         # for adding scale bars
# library(directlabels) # for adding contour line labels
library(broom)        # instead of fortify in ggplot2 (recommended)

save.fig <- F
# if there is no internet, use the stored file - this may not work
# as Google apparently allow the use of downloaded images for only
# 30 days.
internet <- T

# create the base map
if (internet){
  West.coast <- get_map(location = c(lon = -138.0,
                                     lat = 43.0),
                        zoom = 4,
                        maptype = "terrain",
                        source = 'google')
  saveRDS(West.coast, file = 'RData/CC_stranding_westcoast.RData')

  So.Cal <- get_map(location = c(lon = -119.0,
                                 lat = 33),
                    zoom = 7,
                    maptype = "satellite",
                    color = 'color',
                    source = 'google')
  saveRDS(So.Cal, file = 'RData/CC_stranding_SoCal.RData')
} else {
  West.coast <- readRDS(file = 'RData/CC_stranding_westcoast.RData')
  SoCal <- readRDS(file = 'RData/CC_stranding_SoCal.RData')
  print('read from RData')
}

map.west.coast <- ggmap(West.coast)
map.So.Cal <- ggmap(So.Cal)

# critical habitat polygons can be found here:
# http://www.nmfs.noaa.gov/pr/species/criticalhabitat.htm
# Dc_CH <- spTransform(readOGR(dsn = "data",
#                              layer = "Final_LeatherbackCH",
#                              verbose = F),
#                      CRS("+proj=longlat +datum=WGS84"))
#
# Dc_CH.df <- tidy(Dc_CH)

# p1 <- map.west.coast +
#   geom_polygon(data = Dc_CH.df,
#                aes(x = long, y = lat, group = group),
#                fill = 'darkseagreen1', colour = 'gray26',
#                alpha = 0.4)


# read the stranding data:
infile <- 'data/Stranding_Query_Loggerheads_March2017.txt'

dat1 <- read.table(infile, sep = ",", header = TRUE)
dat1$yr.fac <- as.factor(dat1$Year_Initially_Observed)

dat1.locs <- na.omit(dat1[, c('yr.fac', 'Latitude', 'Longitude')])
colnames(dat1.locs) <- c('Year', 'Latitude', 'Longitude')

p1 <- map.west.coast +
  geom_point(data = dat1.locs,
             aes(x = Longitude, y = Latitude,
                 color = Year),
             size = 4) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Longitude") +
  ylab("Latitude")

dat.locs.So.Cal <- subset(dat1.locs, Longitude > -122)

p2 <- map.So.Cal +
  geom_point(data = dat.locs.So.Cal,
             aes(x = Longitude,
                 y = Latitude,
                 color = Year),
             size = 3) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Longitude") +
  ylab("Latitude")


if (save.fig){
  ggsave(filename = paste0('figures/Cc_strandings_WestCoast_',
                           Sys.Date(), '.png'),
         width = 8,
         height = 7,
         plot = p1,
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_SoCal_',
                           Sys.Date(), '.png'),
         width = 8,
         height = 7,
         plot = p2,
         dpi = 1200)

}

