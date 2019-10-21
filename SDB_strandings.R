
rm(list = ls())
library(tidyverse)
library(ggplot2)

run.date <- "2019-10-21"
data.all <- read.csv(file = paste0("data/WC_Strandings_Chelonia_", 
                        run.date, ".csv"))

data.all %>% filter(Latitude < 32.73) %>%
  mutate(year = lubridate::year(Date)) -> data.SDB

data.SDB %>%  group_by(year) %>%
  summarise(n = n()) -> counts


p1 <- ggplot(data = counts) + 
  geom_path(aes(x = year, y = n),
            size = 2) +
  labs(x = "", y = "Number stranded (< 32.73 N)")


ggsave(plot = p1,
       filename = "figures/Strandings_SDB.png",
       device = "png",
       dpi = 600)
