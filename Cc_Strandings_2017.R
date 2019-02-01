#Dc_stranding

# tomo eguchi
# 7 January 2015


rm(list=ls())
ifelse(Sys.info()[1] == 'Linux',
       source('~/Documents/R/tools/TomosFunctions.R'),
       source('~/R/tools/TomosFunctions.R'))

library(tidyverse)
library(reshape)
runDate <- Sys.Date()
save.fig <- F

#SE <- function(x) sd(x, na.rm = T)/sqrt(length(na.omit(x)))

# the following file was created using queryStranding.R - with ODBC connection
# the rundate is in the filename.
infile <- 'data/Turtle_Stranding_all_2017-10-30.rds'

dat1 <- read_rds(path = infile)

dat1.Cc <- filter(dat1, Genus == "Caretta") %>%
  select(., Genus, Species,
         Year_Initially_Observed,
         Month_Initially_Observed,
         Stranded, Released, Died, Necropsied,
         Age, Condition, Latitude,
         Longitude, State,
         Sex, Weight,
         Weight_Accuracy,
         Curved_Carapace_Length,
         Straight_Carapace_Length,
         Fishery_Interaction,
         Alive_Transferred) %>%
  dplyr::rename(., Year = Year_Initially_Observed,
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

dat1.Cc[is.na(dat1.Cc$Released), "Released"] <- 0
# enter latitudes for those without latitudes - use the state information
mean.lat.CA <- filter(dat1.Cc, State == "CALIFORNIA") %>%
  select(., Latitude) %>% summarise(., mean = mean(Latitude, na.rm = T))

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

# Get ONI data:
ONI.values <- read.csv('~/R/SCB_AerialSurvey/Data/ONI_20180319_nosource.csv')
#ONI.values <- reshape::melt(ONI.dat.raw, id.vars = 'Year', value.name = 'ONI')

colnames(ONI.values) <- c('Year', 'Month', 'Total', 'ClimAdj', 'ONI')

ONI.values %>% mutate(time = Year + Month/12 - 1/12) %>%
  mutate(time.end = time + 1/12) %>%
  mutate(Nino = ifelse(ONI.values$ONI > 0, 'TRUE', 'FALSE')) %>%
  mutate(cumuONI = cumsum(ONI)) %>%
  mutate(cumu6moONI = c(rep(NA, 5), moving.cumsum(ONI, 6))) %>%
  mutate(cumu4moONI = c(rep(NA, 3), moving.cumsum(ONI, 4))) %>%
  mutate(cumu2moONI = c(rep(NA, 1), moving.cumsum(ONI, 2))) %>%
  mutate(lag6moONI = c(rep(NA, 6), ONI[1:(nrow(ONI.values) - 6)])) %>%
  mutate(cumulag6moONI = c(rep(NA, 5), moving.cumsum(lag6moONI, 6))) %>%
  filter(Year <= max(dat1.Cc$Year)+1 & Year >= min(dat1.Cc$Year)) -> ONI.values

# Get PDO data:
dat.PDO <- read.delim('~/R/SCB_AerialSurvey/Data/PDO_20171128.txt',
                      sep = "", header = T)
PDO.values <- melt(dat.PDO, id.vars = 'YEAR')
colnames(PDO.values) <- c('Year', 'MMM', 'PDO')

dt <- seq(from = 0, to = 1.0 - 1/12, by = 1/12)
uniq.period <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
num.month <- 1:12

PDO.values$dt <- NA
PDO.values$Month <- NA
for (k in 1:length(uniq.period)){
  PDO.values[PDO.values$MMM == uniq.period[k], 'dt'] <- dt[k]
  PDO.values[PDO.values$MMM == uniq.period[k], 'Month'] <- num.month[k]
}

PDO.values$time <- PDO.values$Year + PDO.values$dt
PDO.values$Pos <- ifelse(PDO.values$PDO > 0, 'TRUE', 'FALSE')
PDO.values %>% mutate(cumuPDO = cumsum(PDO)) %>%
  mutate(cumu6moPDO = c(rep(NA, 5), moving.cumsum(PDO, 6))) %>%
  mutate(cumu4moPDO = c(rep(NA, 3), moving.cumsum(PDO, 4))) %>%
  mutate(cumu2moPDO = c(rep(NA, 1), moving.cumsum(PDO, 2))) %>%
  mutate(lag6moPDO = c(rep(NA, 6), PDO[1:(nrow(PDO.values) - 6)])) %>%
  mutate(cumulag6moPDO = c(rep(NA, 5), moving.cumsum(lag6moPDO, 6))) %>%
  filter(Year <= max(dat1.Cc$Year)+1 & Year >= min(dat1.Cc$Year)) -> PDO.values
# correlation between the number of strandings and ONI
dat1.Cc %>% group_by(Year, Month) %>% summarise(n_Cc = n()) -> dat1.Cc.byYrMo
# ONI.values %>% group_by(Year) %>% summarise(avgONI = mean(ONI),
#                                             sumONI = sum(ONI)) -> ONI.values.byYr

dat1.Cc.byYrMo %>%
  right_join(., ONI.values, by = c("Year", "Month")) %>%
  right_join(., PDO.values, by = c("Year", "Month")) -> dat1.Cc.byYrMo.ONI.PDO

dat1.Cc.byYrMo.ONI.PDO[is.na(dat1.Cc.byYrMo.ONI.PDO)] <- 0

# try glm to see if ONI is useful:
# should look at zero-inflated... see Zurr et al. 2009, p.278- (11.4.2)
library(pscl)
fit1 <- zeroinfl(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI + PDO + cumu6moPDO + lag6moPDO + cumulag6moPDO -1 | ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI + PDO + cumu6moPDO + lag6moPDO + cumulag6moPDO - 1,
            dist = 'poisson', link = 'logit',
            data = dat1.Cc.byYrMo.ONI.PDO)

summary(fit1)
# not really... ONI is negatively affecting counts... isn't that the opposite of what
# we expected? What gives?  Check for overdispersion by looking at the negative binomial
# model.

fit1.nb <- zeroinfl(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI -1 | ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI- 1,
                   dist = 'negbin', link = 'logit',
                   data = dat1.Cc.byYrMo.ONI.PDO)

summary(fit1.nb)
library(lmtest)

# Run the likelihood ratio to test if the variance structure for the Poisson model
# is the same as the negative binomial model.
lrtest(fit1, fit1.nb)
# well... it appears that they are not the same... so we should use the negative
# binomial model.

# several models to be compared: First change binomial zero-inflated part.
model2 <- formula(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI -1 | ONI + Month + cumu6moONI + lag6moONI - 1)

model3 <- formula(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI -1 | ONI + Month + cumu6moONI - 1)

model4 <- formula(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI -1 | ONI + Month - 1)

model5 <- formula(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI + cumulag6moONI -1 | ONI - 1)

fit2.nb <- zeroinfl(formula = model2, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit3.nb <- zeroinfl(formula = model3, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit4.nb <- zeroinfl(formula = model4, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit5.nb <- zeroinfl(formula = model5, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)

AIC(fit1.nb, fit2.nb, fit3.nb, fit4.nb, fit5.nb)
summary(fit3.nb)

# Next, change the count part:
model3.2 <- formula(n_Cc ~ ONI + Month + cumu6moONI + lag6moONI -1 | ONI+Month - 1)
model3.3 <- formula(n_Cc ~ ONI + Month + cumu6moONI -1 | ONI+Month - 1)
model3.4 <- formula(n_Cc ~ ONI + Month -1 | ONI+Month - 1)
model3.5 <- formula(n_Cc ~ ONI -1 | ONI+Month - 1)

fit3.2.nb <- zeroinfl(formula = model3.2, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit3.3.nb <- zeroinfl(formula = model3.3, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit3.4.nb <- zeroinfl(formula = model3.4, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)
fit3.5.nb <- zeroinfl(formula = model3.5, dist = 'negbin', link = 'logit',
                    data = dat1.Cc.byYrMo.ONI.PDO)

AIC(fit3.nb, fit3.2.nb, fit3.3.nb, fit3.4.nb, fit3.5.nb)
# So, the original fit3.nb is the best... Really?

summary(fit3.nb)

dat1.state <- dat1.Cc[dat1$State != '', ]
p1 <- ggplot(data = dat1.state) +
  geom_bar(aes(x = yr.fac, fill = State)) +
  #qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) +
  scale_y_continuous(breaks = seq(0, 17, 1)) +
  ylab('') + xlab('') +
  #ggtitle('Stranded loggerhead turtles') +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 12,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = c(0.7, 0.8))

# dat1.size <- dat1[, c('State', 'yr.fac', 'Species_Code',
#                       'Latitude', 'Longitude',
#                       'Weight', 'Weight_Accuracy',
#                       'Curved_Carapace_Length',
#                       'Straight_Carapace_Length',
#                       'Fishery_Interaction')]

p2 <- ggplot() +
  geom_rect(data = ONI.values,
            aes(ymin = -Inf, ymax = Inf,
                xmin = time, xmax = time.end,
                fill = ONI), alpha = 0.4) +
  scale_fill_gradient(low = 'white', high = 'red') +
  geom_point(data = dat1.Cc,
             aes(x = frac.Year,
                 y = Latitude,
                 color = as.factor(with.latitude),
                 shape = as.factor(Released),
                 size = CCL)) +
  scale_color_manual(name = 'Latitude',
                     values = c("0" = 'darkgreen',
                                "1" = 'darkred'),
                     labels = c("Average", "With data")) +
  scale_shape_manual(name = 'Dead/Released',
                     labels = c("Dead", "Released"),
                     values = c(16,17))+
  scale_size_continuous(name = "CCL (cm)") +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5))) +
  geom_hline(yintercept = 34.45, size = 1.5, alpha = 0.6)+
  xlab('') +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.7, 0.8),
        legend.box = 'horizontal')

dat1.Cc %>% filter(Latitude <=34.45) -> dat1.Cc.S
p2.S <- ggplot() +
  geom_point(data = dat1.Cc.S,
             aes(x = frac.Year,
                 y = Latitude,
                 color = as.factor(with.latitude),
                 shape = as.factor(Released),
                 size = CCL)) +
  scale_color_manual(name = 'Latitude',
                     values = c("0" = 'darkgreen',
                                "1" = 'darkred'),
                     labels = c("Average", "With data")) +
  scale_shape_manual(name = 'Dead/Released',
                     labels = c("Dead", "Released"),
                     values = c(16,17))+
  scale_size_continuous(name = "CCL (cm)") +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5))) +
  #geom_hline(yintercept = 34.45, size = 1.5, alpha = 0.6)+
  xlab('') +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.1, 0.3),
        legend.box = 'vertical')

dat1.Cc %>% filter(Latitude > 34.45) %>%
  select(Latitude, Released, CCL, frac.Year) -> dat1.Cc.N
p2.N <- ggplot() +
  geom_point(data = dat1.Cc.N,
             aes(x = frac.Year,
                 y = Latitude,
                 size = CCL),
             color = 'darkred') +
  scale_size_continuous(name = "CCL (cm)") +
  guides(color = guide_legend(override.aes = list(size = 5)),
         shape = guide_legend(override.aes = list(size = 5))) +
  #geom_hline(yintercept = 34.45, size = 1.5, alpha = 0.6)+
  xlab('') +
  ylab(expression(paste("Latitude (", degree, "N)"))) +
  theme(axis.text.x = element_text(angle = 0,
                                   size = 12,
                                   vjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = c(0.85, 0.7),
        legend.box = 'horizontal')


if (save.fig){
  ggsave(filename = paste0('figures/Cc_strandings_histogram_',
                           Sys.Date(), '.png'),
         plot = p1,
         width = 8,
         height = 7,
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_latitude_',
                           Sys.Date(), '.png'),
         plot = p2,
         width = 7,
         height = 5,
         units = 'in',
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_N_latitude_',
                           Sys.Date(), '.png'),
         plot = p2.N,
         #width = 7,
         height = 3.44,
         units = 'in',
         dpi = 1200)

  ggsave(filename = paste0('figures/Cc_strandings_S_latitude_',
                           Sys.Date(), '.png'),
         plot = p2.S,
         #width = 7,
         height = 3.44,
         units = 'in',
         dpi = 1200)

}

