library(tidyverse)
library(janitor)
library(tidycensus)
library(blscrapeR)
library(sf)
library(arcgisbinding)
library(mapview)
arc.check_product()
library(jsonlite)
library(RSocrata)
library(arcpy)
library(purrr)
options(scipen = 9999999, digits = 6)
arc.check_portal()


sign_up_sheet <- readxl::read_xlsx(here::here("Data Catalog", "Draft Data Catalog Sign-Up.xlsx"), sheet = "Enterprise Data Catalog") %>%
  filter(!is.na(ID),
         ID != "NA")

split_values <- strsplit(as.character(sign_up_sheet$ID), ",")

all_values <- trimws(unlist(split_values)) 

#removing LUI For now due to size
all_values <- all_values[all_values != '62faa3a93c124124a050c05d8ed858ea']



gis <- import("arcgis.gis")$GIS
features <- import("arcgis.features")
content_manager <- import("arcgis.gis._impl._content_manager")
portal <- gis("https://cmap-gisent01.cmap.local:7443/arcgis/", "mshapey_cmapgisENT", "CMAPPER2024!", verify_cert = FALSE)

items <- portal$content$search("", max_items=1000)

items_filtered <- keep(items, ~ .x$id %in% all_values)

# items_df <- tibble(
#   title = map_chr(items, ~ .x$title),
#   id = map_chr(items, ~ .x$id),
#   type = map_chr(items, ~ .x$type)
# ) %>%
#   filter(type != "Service Definition")



# Initialize a list to store downloaded/queried content
item_contents <- list()

# Loop through each item and try to get useful content
for (i in seq_along(items_filtered)) {
  item <- items_filtered[[i]]
  title <- gsub("[^A-Za-z0-9_]", "_", item$title)  # Safe name
  message(paste0("Processing item ", i, ": ", item$title, " (", item$type, ")"))
  
  try({
    if ("layers" %in% names(item)) {
      # Feature layers or feature layer collections
      for (j in seq_along(item$layers)) {
        lyr <- item$layers[[j]]
        df <- lyr$query(where = "1=1", out_fields = "*", as_df = TRUE)
        layer_name <- paste0(title, "_layer", j)
        item_contents[[layer_name]] <- df
      }
      
    } else if ("tables" %in% names(item)) {
      # Some feature services include tables
      for (j in seq_along(item$tables)) {
        tbl <- item$tables[[j]]
        df <- tbl$query(where = "1=1", out_fields = "*", as_df = TRUE)
        table_name <- paste0(title, "_table", j)
        item_contents[[table_name]] <- df
      }
      
    } else if (item$type == "CSV") {
      # Try to download file-based content
      path <- item$download()
      df <- read.csv(path)
      item_contents[[title]] <- df
      
    } else {
      # For other item types, just store the item object
      item_contents[[title]] <- item
    }
    
  }, silent = TRUE)
}








