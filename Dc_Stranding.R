#Dc_stranding

# tomo eguchi
# 7 January 2015

library(ggplot2)
sysInfo <- Sys.info()
ifelse(sysInfo[1] == "Windows", source('~/R/TomosFunctions.R'),
       source('~/Documents/R/TomosFunctions.R'))

#source('~/R/R_Work/TomosFunctions.R')
D <- dirSelector()
runDate <- Sys.Date()

wkDir <- paste0(D$Dtomo, 'SWFSC/GroundFishBiOp/')

infile <- paste0(wkDir, 'WC_DC_Strandings.csv')

dat1 <- read.table(infile, sep = ",", header = TRUE)
dat1$yr.fac <- as.factor(dat1$YEAR)

dat1.dead <- dat1[-grep("ALIVE", dat1$FINAL_DISPOSITION),]
dat1.dead.CA <- dat1.dead[dat1.dead$STATE == 'CA',]
dat1.human <- dat1.dead[dat1.dead$HUMAN_INTERACTION != 'NO',]
dat1.fishery <- dat1.human[dat1.human$HUMAN_INTERACTION_CAUSE != 'BOAT COLLISION' & 
                             dat1.human$HUMAN_INTERACTION_CAUSE != 'OTHER' &
                             dat1.human$HUMAN_INTERACTION_CAUSE != 'POWER PLANT ENTRAINMENT' &
                             dat1.human$HUMAN_INTERACTION_CAUSE != 'SHOT', ]

summary(dat1.fishery)
# change the colors by state. 

qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) + 
  scale_y_continuous(breaks = seq(0, 10, 1)) +
  ylab('Counts') + xlab('Year') + 
  ggtitle('Stranded leatherback turtles with human interaction (dead or unknown disposition)') +
  theme(axis.text.x = element_text(angle = 90, size = 15, vjust = 0.5))

qplot(yr.fac, data = dat1.dead, geom = "bar", fill = STATE) + 
  scale_y_continuous(breaks = seq(0, 12, 1)) +
  ylab('Counts') + xlab('Year') + 
  ggtitle('Stranded leatherback turtles (dead or unknown final disposition)') +
  theme(axis.text.x = element_text(angle = 90, size = 15, vjust = 0.5))


