library(tidyverse)
library(skimr) # Package for summary stats on datasets
library(cowplot) # for making multi-paneled plots
#library(maps)
library(ggmap)
data(canada.cities)
options(width = 100) # ensure skim results fit on one line

pdq_cols <- cols(  
  NO_CIV_LIE = col_integer(),
  PREFIX_TEM = col_factor(),
  NOM_TEMP = col_factor(),
  DIR_TEMP = col_factor(),
  MUN_TEMP = col_factor(),
  DESC_LIEU = col_character(),
  Longitude = col_double(),
  Latitude = col_double()
)

pdq <- read_csv("data/pdq_point.csv", col_types = pdq_cols)

pdq <- pdq %>% 
  mutate(pdq_num = str_extract(DESC_LIEU, "\\d+$"),
         pdq_num = as.integer(pdq_num)) %>% 
  select(-DESC_LIEU)

skim(pdq)
view(pdq)

crime_cols <- cols(  
  CATEGORIE = col_factor(),
  DATE = col_date(),
  QUART = col_factor(levels = c("jour", "soir", "nuit")),
  PDQ = col_integer(),
  X = col_double(),
  Y = col_double(),
  LONGITUDE = col_double(),
  LATITUDE = col_double()
)

mtl_crime <- read_csv("data/interventionscitoyendo.csv", col_types = crime_cols)

mtl_crime <- mtl_crime %>% 
  select(-X, -Y) %>% 
  filter(!is.na(PDQ))

mtl_crime %>% 
  filter(LONGITUDE == 1 | LATITUDE == 1 )

skim(mtl_crime)
view(mtl_crime)

mtl_crime %>% 
  group_by(PDQ, CATEGORIE) %>% 
  summarise(n = n()) %>% 
  arrange(PDQ, desc(n)) %>% 
  View()

mtl_crime %>% 
  group_by(CATEGORIE) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

mtl_crime %>% 
  group_by(PDQ) %>% 
  summarise(n = n()) %>% 
  arrange(PDQ)


# get Montreal map
my_loc <- c(lon = -73.5, lat = 45.5)
my_loc <- c("montreal, quebec")
mtl_map <- get_map(location = my_loc,source = "osm")

qmap("Forbidden city",zoom=15)
qmap("Forbidden city",zoom=15, source="osm")
qmap("Forbidden city")
