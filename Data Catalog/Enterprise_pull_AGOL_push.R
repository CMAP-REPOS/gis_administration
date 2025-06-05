library(tidyverse)
library(janitor)
library(tidycensus)
library(sf)
library(arcgisbinding)
arc.check_product()
arc.check_portal()
library(jsonlite)
library(RSocrata)
library(arcpy)
library(purrr)
library(xml2)
library(arcgislayers)
library(arcgis)
library(httr)
library(zip)
library(fs)
options(scipen = 9999999, digits = 6)
# Get token for API call, make sure active portal is AGOL


agol_token <- auth_binding()
set_arc_token(agol_token)


sign_up_sheet <- readxl::read_xlsx("S:\\AdminGroups\\ResearchAnalysis\\MCS\\Draft Data Catalog Sign-Up.xlsx", sheet = "Enterprise Data Catalog") %>%
  filter(!is.na(ID),
         ID != "NA")

split_values <- strsplit(as.character(sign_up_sheet$ID), ",")

all_values <- trimws(unlist(split_values)) 

#removing LUI For now due to size
all_values <- all_values[all_values != '62faa3a93c124124a050c05d8ed858ea']

#testing with one feature layer to make sure this works
all_values <- all_values[all_values == '706baca55c064d3889ca27edc8d70635' | all_values == '8aa1c80a0ea84112900b85c4f528f949' 
                         | all_values == '912b2e8bf88f449ea9991a4243e613f1'| all_values == '0d92396d3f2446e3ab76db3deeee7b7a'
                         | all_values == '359a7f12ed0b4bfaa19f372765d01a70']

gis <- import("arcgis.gis")$GIS
arcgis <- import("arcgis.gis")
features <- import("arcgis.features")
content_manager <- import("arcgis.gis._impl._content_manager")
portal <- gis("https://cmap-gisent01.cmap.local:7443/arcgis/", "mshapey_cmapgisENT", "CMAPPER2024!", verify_cert = FALSE)

items <- portal$content$search("", max_items=1000)

items_filtered <- keep(items, ~ .x$id %in% all_values)

item_ids <- map_chr(items_filtered, "id")

full_items<- (map(item_ids, ~ portal$content$get(.x)))
# full_dict <- full_items[[1]]$to_dict()
agol_portal <- gis("https://www.arcgis.com/", 'mshapey_cmapgis', 'CMAPPER2024!')
user <- agol_portal$users$get("mshapey_cmapgis")



uploaded_items <- map(full_items, function(item) {
  title <- item[["title"]]
  type <- item[["type"]]
  description <- item[["description"]]
  snippet <- item[["snippet"]]
  categories <- item[["categories"]]
  url <- item[["url"]]
  
  if (is.null(title) || title == "" || is.null(type) || type == "") {
    stop("Missing required title or type in item metadata.")
  }
  
  # Check if the item type requires a URL or a file upload
  #if item is NOT hosted, it can just republish using link
  #else it has to download and re-upload the item
  if (!is.null(url) && !grepl("/Hosted/", url, ignore.case = TRUE)) {
    # Upload via URL
    url <- item[["url"]]
    if (is.null(url) || url == "") {
      stop(paste("Item of type", type, "requires a valid URL but none found"))
    }
    
    uploaded_item <- agol_portal$content$add(
      item_properties = list(
        title = title,
        type = type,
        description = description,
        snippet = snippet,
        categories = categories,
        url = url
      )
    )
  } else {
    # Export to File Geodatabase
    message("Exporting ", title, " to File Geodatabase...")
    exported <- item$export(title, export_format = "File Geodatabase")
    save_dir <- "C:\\Users\\mshapey\\Documents\\Test"
    dir_create(save_dir)
    
    
    # Download the exported .zip file
    download_path <- exported$download(save_path =save_dir )
    #unzip 
    unzip(download_path, exdir = "C:/Users/mshapey/Documents/Test/")
    #get path of unzipped gdb
    unzipped_contents <- list.dirs("C:/Users/mshapey/Documents/Test/", full.names = TRUE, recursive = FALSE)
    

    #rename gdb
    new_path <- str_remove(download_path, '.zip')
    
    file.rename(unzipped_contents, paste0(new_path, ".gdb"))
    
    #zip gdb
    gdb_folder <- paste0(new_path, '.gdb')
    zip_file <- paste0(gdb_folder, '.zip')
    gdb_files <- list.files(gdb_folder, recursive = TRUE, full.names = TRUE)
    # zip(zipfile = zip_file, files = list.files(gdb_folder, full.names = TRUE))
    # Windows shell command using powershell's Compress-Archive
    system(sprintf(
      'powershell -Command "Compress-Archive -Path \'%s\' -DestinationPath \'%s\'"',
      gdb_folder,
      zip_file
    ))
    
    
    
    
    # Upload as a new item
    uploaded_item <- agol_portal$content$add(
      path = zip_file,
      item_properties = list(
        title = title,
        type = "File Geodatabase",
        description = description,
        
        snippet = snippet,
        categories = categories
      ),
      data = zip_file
    )
    # Publish it as a Hosted Feature Layer
    published_item <- try(uploaded_item$publish(), silent = TRUE)
    if (inherits(published_item, "try-error") || is.null(published_item)) {
      message("Publishing failed for ", title)
      return(uploaded_item)
    }
    
    #delete items in folder
    delete_items <- dir_ls("C:/Users/mshapey/Documents/Test/", recurse = FALSE)
    
    walk(delete_items, function(path) {
      if (dir_exists(path)) {
        dir_delete(path)
      } else if (file_exists(path)) {
        file_delete(path)
    
      }
    })
    
    
    
    return(published_item)
    

    
  }
})


