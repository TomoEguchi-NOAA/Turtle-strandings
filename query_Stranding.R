
rm(list=ls())
library(RODBC)
library(dplyr)
library(readr)

SWFSC <- T
# load a couple databases through ODBC
# this works only at SWFSC - or VPN also?

# define the genus name for species to filter
#sp2filter <- data.frame(Genus = as.factor(c("Chelonia", "Caretta", "Lepidochelys"))) #"Dermochelys"

sp2filter <- data.frame(Genus = as.factor(c("Chelonia"))) #"Dermochelys"

if (SWFSC){
  Turtle <- odbcConnect(dsn = 'Turtle', uid = '', pwd = '')
  #Turtle.tbls <- sqlTables(Turtle)
  Turtle.Stranding <- sqlQuery(Turtle,
                               'select * from tbl_Stranding')
  Turtle.Stranding.Details <- sqlQuery(Turtle,
                                       'select * from tbl_Stranding_Detail')
  Sp.table <- sqlQuery(Turtle,
                       'select * from tblSpecies')
  odbcClose(Turtle)

  SWFSCCommon <- odbcConnect(dsn = 'Common', uid = '', pwd = '')
  Sp.table <- sqlQuery(SWFSCCommon,
                       'select * from tblSpecies')
  State.table <- sqlQuery(SWFSCCommon,
                          'select * from tblState')
  City.table <-  sqlQuery(SWFSCCommon,
                          'select * from tblCity')
  island.table <- sqlQuery(SWFSCCommon,
                           "select * from tblIsland")
  county.table <- sqlQuery(SWFSCCommon,
                           "select * from tblCounty")
  odbcClose(SWFSCCommon)


  write.csv(Turtle.Stranding,
            file = paste0("data/Turtle_Stranding_", Sys.Date(), ".csv"),
            quote = F, row.names = F)

  write.csv(Turtle.Stranding.Details,
            file = paste0("data/Turtle_Stranding_Details_", Sys.Date(), ".csv"), 
            quote = F, row.names = F)

  write.csv(Sp.table,
            file = 'data/Species_Table.csv',
            quote = F, row.names = F)

  write.csv(State.table,
            file = 'data/State_Table.csv',
            quote = F, row.names = F)

  write.csv(City.table,
            file = 'data/City_Table.csv',
            quote = F, row.names = F)
  
  write.csv(island.table,
            file = 'data/Island_Table.csv',
            quote = F, row.names = F)
  
  write.csv(county.table,
            file = 'data/County_Table.csv',
            quote = F, row.names = F)
  
} else {
  # if not at SWFSC, use the local files.
  Turtle.Stranding <- read.table(file = 'data/Turtle_Stranding.csv',
                                 header = T,
                                 sep = ",")

  Turtle.Stranding.Details <- read.table(file = 'data/Turtle_Stranding_Details.csv',
                                         header = T,
                                         sep = ",")

  Sp.table <- read.table(file = 'data/Species_Table.csv',
                         header = T, sep = ",")

  State.table <- read.table(file = 'data/State_Table.csv',
                         header = T, sep = ",")

  City.table <- read.table(file = 'data/City_Table.csv',
                            header = T, sep = ",")
}

# remove some fields. ts is a checkbox results
# I also removed NMFS_ID because there are "1st time stranded", "2nd time stranded" etc. 
Turtle.Stranding %>% select(-c(ts, 
                               starts_with("Observed_By"),
                               starts_with("Edit_"),
                               NMFS_ID)) -> Turtle.Stranding.2

# need to remove some fields - they are paragraphs and not useful
Turtle.Stranding.Details %>% select(-c(starts_with("Examined_By"),
                                       ends_with("Description"),
                                       starts_with("Edit_"),
                                       starts_with("Necropsied_"),
                                       ts,
                                       Location,
                                       Locality_Details,
                                       Other_Finding,
                                       Other_Finding_Description,
                                       Necropsy_Remarks)) -> Turtle.Stranding.Details.2

# merge together by ID and Stranding_ID, which are shared
# then merge with Sp ID table (Sp.table)
all.data <- inner_join(Turtle.Stranding.2,
                       Turtle.Stranding.Details.2,
                       by = c("ID" = "Stranding_ID")) %>%
  inner_join(Sp.table, by = c("Species_ID" = "ID")) %>%
  inner_join(State.table, by = c("State_ID" = "ID")) %>%
  rename(State = Name)

  #%>%
  # inner_join(., City.table, by = c("City_ID" = "ID")) %>%
  # rename(., City = Name)

all.data %>% right_join(sp2filter, by = "Genus") %>%
  select(Year_Initially_Observed,
         Month_Initially_Observed,
         Day_Initially_Observed,
         Stranded,
         Released, Died, Latitude, Longitude,
         State,
         Curved_Carapace_Length, 
         Weight,
         Fishery_Interaction,
         Human_Interaction,
         Alive_Released,
         Genus, Species) %>%
  mutate(Date = as.Date(paste(Year_Initially_Observed, 
                              Month_Initially_Observed,
                              Day_Initially_Observed, sep = "-"))) -> sp.data


write.csv(sp.data, 
          file = paste0("data/WC_Strandings_",
                        paste(as.vector(sp2filter$Genus), collapse = "_"), "_", 
                        Sys.Date(), ".csv"),
          row.names = FALSE, quote = FALSE)

# write out the results into .rds format. csv failed with unequal numbers of
# elements written out... not sure what happened there but .rds works.
# write_rds(all.data,
#           path = paste0('data/Turtle_Stranding_all_', Sys.Date(), '.rds'))
