
# 0. Background -----------------------------------------------------------

# This script provides an example of how to load an item from CMAP's ArcGIS Online (AGOL) organization into a R session. While this process is the same for all item in AGOL, this example uses an item from the Data Catalog group.

# In order to run, please install the arcgis and arcgisbinding packages and link them with your ArcGIS Pro instance: https://developers.arcgis.com/r-bridge/installation/



# 1. Setup ----------------------------------------------------------------

# Load in packages
library(tidyverse);library(sf)
library(janitor);library(cmapplot)
library(arcgis);library(arcgisbinding)



# 2. arcgis example -------------------------------------------------------

# Front-end URL in ArcGIS Online is:
# https://cmapgis.maps.arcgis.com/home/item.html?id=bdfe03f4e02f44d98f3f7f02614c17c2

# Item feature service URL (scroll down on right-side menu to URL) for Mmunicipalities boundaries (2024)
muni_url_fs <- "https://services5.arcgis.com/LcMXE3TFhi1BSaCY/arcgis/rest/services/Municipalities_Northeastern_Illinois_2024/FeatureServer"

# For access to organization's ArcGIS Online portal, get a token for API calls from arcgisbinding package. You must already have the package installed and linked with your ArcGIS Pro instance for this section to run: https://developers.arcgis.com/r-bridge/installation/
agol_token <- auth_binding()
set_arc_token(agol_token)

# Open feature service item
muni_fs <- arc_open(muni_url_fs)

# Navigate to individual feature layer (only one to choose from in this case)
muni_fl <- get_layer(muni_fs, id = 0)

# Read in data from layer
cmap_muni_sf <- arc_select(muni_fl)

# You can also read in data directly from feature layer URL if you have that, which is the next level down in the ArcGIS REST API from the feature service URL if you view it in a separate tab
# cmap_muni_sf <- arc_select(arc_open("https://services5.arcgis.com/LcMXE3TFhi1BSaCY/arcgis/rest/services/Municipalities_Northeastern_Illinois_2024/FeatureServer/0"))

# View as a table
cmap_muni_sf %>% 
  view()

# Map with cmapplot blues palette using the shading column
mapview::mapview(cmap_muni_sf,
                 zcol = "FIRST_SHADING",
                 col.regions = fetch_pal("blues"))
