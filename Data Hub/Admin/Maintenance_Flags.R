
# 1. Setup ----------------------------------------------------------------

## 1a. Load libraries -----

# Basics
library(tidyverse);library(lubridate)
# Utility
library(here);library(janitor)
library(purrr);library(clipr)
# Excel
library(readxl)
# ArcGIS
library(arcgis)



## 1b. Options -----

# Options, call stored Census API key, load fonts
options(scipen = 1000, stringsAsFactors = FALSE)
# Prevent RPlots empty PDF
if(!interactive()) pdf(NULL)

# Load in credentials from config
config <- config::get()



# 2. Ingest data ----------------------------------------------------------


# Read in DataHub AGOL Open Data Content Group
hub_base <- read_csv(here("02_Data", "Raw",
                           "DataHub_Base.csv"))

# Read in Stewards worksheet
steward_base <- read_excel(here("02_Data", "Raw",
                             "AGOL_Stewards.xlsx"),
                        sheet = "Depot_Hub_Crosswalk") %>% 
  clean_names()



# 3. Maintenance Work -----------------------------------------------------

## 3a. Data with no stewards -----

flag_stewards <- hub_base %>% 
  select(agol_item_id, title, "agol_metadata_contact" = metadata_contact) %>% 
  left_join(steward_base %>% 
              select(agol_item_id, steward),
            by = "agol_item_id")

# Current tags
hub_base %>%
  filter(!is.na(tags)) %>%
  select(tags) %>%
  separate_longer_delim(tags,delim = ", ") %>%
  tabyl(tags) %>%
  arrange(desc(n)) %>%
  clipr::write_clip()

# Tags by item
agol_base %>%
  filter(!is.na(tags)) %>%
  select(title, access, tags) %>%
  clipr::write_clip()



# 4. Export data ----------------------------------------------------------

# Write to Raw data folder
saveRDS(agol_clean,
        file = here("02_Data", "Raw",
                    "DataHub_Base.rds"))


