#Dc_stranding

# tomo eguchi
# 7 January 2015
# 2022-10-12 Modified for presentation at West Coast sea turtle symposium

rm(list=ls())

# from https://stackoverflow.com/questions/19200841/consecutive-rolling-sums-in-a-vector-in-r
moving.cumsum <- function(x, n = 2){
  # computes cumulative sum over a span
  y <- rowSums(outer(1:(length(x)-n+1),
                     1:n,
                     FUN=function(i,j){x[(j - 1) + i]}))
  #y <- c(rep(NA, n-1), y)
  return(y)
}

library(tidyverse)
library(reshape)
# library(maps)
# library(rgdal)
library(ggmap)
library(broom)
library(viridis)
library(scales)

save.fig <- T

#SE <- function(x) sd(x, na.rm = T)/sqrt(length(na.omit(x)))

# the following file was created using queryStranding.R - with ODBC connection

#infile <- 'data/Turtle_Stranding_all_2017-10-30.rds'
infile <- "data/WC_Strandings_Caretta_2022-10-05.csv"

# scales library has col_factor, which conflicts with readr col_factor when
# defining columns. 
cols.def <- cols(Year_Initially_Observed = col_integer(),
                 Month_Initially_Observed = col_integer(),
                 Day_Initially_Observed = col_integer(),
                 Stranded = readr::col_factor(levels = c("TRUE", "FALSE")),
                 Released = col_character(),
                 Died = col_character(),
                 Latitude = col_double(),
                 Longitude = col_double(),
                 State = readr::col_factor(),
                 Curved_Carapace_Length = col_double(),
                 Weight = col_double(),
                 Fishery_Interaction = readr::col_factor(),
                 Human_Interaction = col_character(),
                 Alive_Released = readr::col_factor(),
                 Genus = readr::col_factor(),
                 Species = readr::col_factor(),
                 Date = col_date(format = "%Y-%m-%d"))

dat1 <- read_csv(infile, col_types = cols.def)

dat1.Cc <- filter(dat1, Genus == "Caretta") %>%
  select(Genus, Species,
         Year_Initially_Observed,
         Month_Initially_Observed,
         Stranded, Released, Died, #Necropsied,
         #Age, Condition, 
         Latitude,
         Longitude, State,
         #Sex, 
         Weight,
         #Weight_Accuracy,
         Curved_Carapace_Length) %>%
         #Straight_Carapace_Length,
         #Fishery_Interaction,
         #Alive_Transferred) %>%
  dplyr::rename(Year = Year_Initially_Observed,
         Month = Month_Initially_Observed) %>%
  mutate(frac.Year = Year + Month/12) %>%
  mutate(fYear = as.factor(Year)) %>%
  dplyr::rename(., CCL = Curved_Carapace_Length)

dat1.Cc.CA <- filter(dat1.Cc, State == 'CALIFORNIA')
dat1.Cc.SofPtC <- filter(dat1.Cc, Latitude <= 34.45)

# missing CCL
sum(is.na(dat1.Cc$CCL))

dat1.Cc$with.latitude <- 1
dat1.Cc$with.latitude[is.na(dat1.Cc$Latitude)] <- 0

#dat1.Cc[is.na(dat1.Cc$Released), "Released"] <- 0
# enter latitudes for those without latitudes - use the state information
mean.lat.CA <- filter(dat1.Cc, State == "CALIFORNIA") %>%
  select(Latitude) %>% 
  summarise(mean = mean(Latitude, na.rm = T))

dat1.Cc[dat1.Cc$with.latitude == 0, 'Latitude'] <- mean.lat.CA

dat1.Cc[is.na(dat1.Cc$CCL), 'CCL'] <- mean(dat1.Cc$CCL, na.rm = T)

#dat1.dead <- dat1[-grep("TRUE", dat1$Alive_Released),]
#dat1.dead.CA <- dat1.dead[dat1.dead$STATE == 'CA',]
#dat1.human <- dat1.dead[dat1.dead$Human_Interaction != 'NO',]
# dat1.fishery <- dat1.human[dat1.human$HUMAN_INTERACTION_CAUSE != 'BOAT COLLISION' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'OTHER' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'POWER PLANT ENTRAINMENT' &
#                              dat1.human$HUMAN_INTERACTION_CAUSE != 'SHOT', ]

#dat1.fishery <- dat1.human[dat1.human$Fishery_Interaction == T,]

#summary(dat1.human)
# change the colors by state.
# 
# # Get ONI Oceanic Nino Index data:
# # https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_change.shtml
# # look at the link at the bottom to see the most up to date data in a flat ascii file.
# oceans.and.maps.dir <- paste0(Sys.getenv("HOME"), "/Oceans and Maps/")
# ONI.values <- read_fwf(file = paste0(oceans.and.maps.dir, "/ONI/ONI_20221012.txt"), 
#                        col_positions = fwf_widths(c(4,5,7,11,5),
#                                                   col_names = c("Year", "Month", "Total", "ClimAdj", "ONI")),
#                        skip = 1,
#                        col_types = cols(col_integer(),
#                                         col_integer(),
#                                         col_double(),
#                                         col_double(),
#                                         col_double()))
# 
# ONI.values %>% mutate(time = Year + Month/12 - 1/12) %>%
#   mutate(time.end = time + 1/12) %>%
#   mutate(Nino = ifelse(ONI.values$ONI > 0, 'TRUE', 'FALSE')) %>%
#   mutate(cumuONI = cumsum(ONI)) %>%
#   mutate(cumu6moONI = c(rep(NA, 5), moving.cumsum(ONI, 6))) %>%
#   mutate(cumu4moONI = c(rep(NA, 3), moving.cumsum(ONI, 4))) %>%
#   mutate(cumu2moONI = c(rep(NA, 1), moving.cumsum(ONI, 2))) %>%
#   mutate(lag6moONI = c(rep(NA, 6), ONI[1:(nrow(ONI.values) - 6)])) %>%
#   mutate(cumulag6moONI = c(rep(NA, 5), moving.cumsum(lag6moONI, 6))) %>%
#   filter(Year <= max(dat1.Cc$Year)+1 & Year >= min(dat1.Cc$Year)) -> ONI.values
# 
# # Get PDO data:
# # https://www.ncei.noaa.gov/access/monitoring/pdo/
# # Select "View Data" and "Access Data" An ascii flat file will appear on the browser
# 
# dat.PDO <- read_fwf(file = paste0(oceans.and.maps.dir, "/PDO/PDO_20221012.txt"), 
#                     col_positions = fwf_widths(c(4, rep(6, times = 12)),
#                                                col_names = c("Year", "Jan", "Feb", "Mar", "Apr",
#                                                              "May", "Jun", "Jul", "Aug", "Sep",
#                                                              "Oct", "Nov", "Dec")),
#                     skip = 1,
#                     col_types = cols(col_integer(),
#                                      col_double(), col_double(),
#                                      col_double(), col_double(),
#                                      col_double(), col_double(),
#                                      col_double(), col_double(),
#                                      col_double(), col_double(),
#                                      col_double(), col_double()))
# 
# dat.PDO %>%
#   pivot_longer(!Year, names_to = "MMM", values_to = "PDO") -> PDO.values
# 
# PDO.values %>%
#   mutate(Month = match(MMM, month.abb),
#          dt = (Month - 1)/12,
#          time = Year + dt,
#          Pos = ifelse(PDO > 0, "TRUE", "FALSE")) -> PDO.values
# 
# PDO.values %>% mutate(cumuPDO = cumsum(PDO)) %>%
#   mutate(cumu6moPDO = c(rep(NA, 5), moving.cumsum(PDO, 6))) %>%
#   mutate(cumu4moPDO = c(rep(NA, 3), moving.cumsum(PDO, 4))) %>%
#   mutate(cumu2moPDO = c(rep(NA, 1), moving.cumsum(PDO, 2))) %>%
#   mutate(lag6moPDO = c(rep(NA, 6), PDO[1:(nrow(PDO.values) - 6)])) %>%
#   mutate(cumulag6moPDO = c(rep(NA, 5), moving.cumsum(lag6moPDO, 6))) %>%
#   filter(Year <= max(dat1.Cc$Year)+1 & Year >= min(dat1.Cc$Year)) -> PDO.values

dat1.state <- dat1.Cc[dat1$State != '', ]
p.1 <- ggplot(data = dat1.state) +
  geom_bar(aes(x = fYear, fill = State)) +
  scale_y_continuous(breaks = seq(0, 7, 1)) +
  ylab('') + xlab('') +
  scale_color_viridis(discrete = TRUE, name = "State",
                      option = "turbo") +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 12,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "top") #c(0.5, 0.8))


# create the base map
# Help document from register_google(). Now Google charges for using their map
# data... 

#"As of mid-2018, the Google Maps Platform requires a registered API key. 
#While this alleviates previous burdens (e.g. query limits), it creates some 
#challenges as well. The most immediate challenge for most R users is that ggmap 
#functions that use Google's services no longer function out of the box, 
#since the user has to setup an account with Google, enable the relevant APIs, 
#and then tell R about the user's setup."
West.coast <- readRDS(file = 'RData/CC_stranding_westcoast.RData')
So.Cal <- readRDS(file = 'RData/CC_stranding_SoCal.RData')

# There are some incorrect lat/lon information so I remove them for
# plotting on maps. I looked at plots and found a few that were not 
# good... these were:

dat1.Cc %>% filter(!(Year == 1998 & Latitude == 33.991)) %>%
  filter(!(Year == 1981 & Latitude == 33)) %>%
  filter(!(Year == 2016 & Latitude == 32)) -> dat2.Cc
  
map.west.coast <- ggmap(West.coast)
map.So.Cal <- ggmap(So.Cal)
p.2 <- map.west.coast +
  geom_point(data = dat2.Cc,
             aes(x = Longitude, y = Latitude,
                 color = fYear),
             size = 4) +
  scale_color_viridis(discrete = TRUE, name = "Year") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = c(0.15, 0.5),
        legend.background = element_rect(fill=alpha("white", 0.5)))

dat.locs.So.Cal <- subset(dat2.Cc, Longitude > -122)

p.3 <- map.So.Cal +
  geom_point(data = dat.locs.So.Cal,
             aes(x = Longitude,
                 y = Latitude,
                 color = fYear),
             size = 3) +
  scale_color_viridis(discrete = TRUE, 
                      name = "Year",
                      option = "turbo") +
  xlab("Longitude") +
  ylab("Latitude")+
  theme(legend.position = c(0.2, 0.4),
        legend.background = element_rect(fill=alpha("white", 0.6)))


if (save.fig){
  ggsave(filename = paste0('figures/Cc_strandings_histogram_',
                           Sys.Date(), '.png'),
         plot = p.1,
         width = 8,
         height = 7,
         dpi = 1200)
  
  ggsave(filename = paste0('figures/Cc_strandings_WestCoast_',
                           Sys.Date(), '.png'),
         width = 8,
         height = 7,
         plot = p.2,
         dpi = 1200)
  
  ggsave(filename = paste0('figures/Cc_strandings_SoCal_',
                           Sys.Date(), '.png'),
         width = 8,
         height = 7,
         plot = p.3,
         dpi = 1200)
  
}
