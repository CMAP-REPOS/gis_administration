
# 1. Setup ----------------------------------------------------------------

## 1a. Load libraries -----

# Basics
library(tidyverse);library(lubridate)
# Utility
library(here);library(clipr)
library(janitor);library(purrr)
# ArcGIS
library(arcgis)
# JSON, XML, loops, nested data
library(jsonlite);library(xml2)



## 1b. options -----

# Options, call stored Census API key, load fonts
options(scipen = 1000, stringsAsFactors = FALSE)
# Prevent RPlots empty PDF
if(!interactive()) pdf(NULL)



# 2. Ingest data ----------------------------------------------------------

# Get token for API call, make sure active portal is AGOL
agol_token <- auth_binding()
set_arc_token(agol_token)

# Group content documentation: https://developers.arcgis.com/rest/users-groups-and-items/group-content.htm
# Pulls in JSON of items in group, with properties: https://developers.arcgis.com/rest/users-groups-and-items/item.htm
# AGOL Hub Open Content Group, query using group ID, access token
agol_url <- paste0("https://www.arcgis.com/sharing/rest/content/groups/",
                   "bd257fd07fea4f9192c24e36e2ed4723", # Data Hub group ID
                   "?token=", agol_token$access_token,
                   "&f=pjson")

# Convert to list
agol_raw <- fromJSON(txt = agol_url)

# Pull out dataframe, clean names
agol_raw <- agol_raw$items %>% 
  clean_names() %>% 
  arrange(title)



# 3. Clean data -----------------------------------------------------------

## 3a. Remove columns -----

# Remove unnecessary data
agol_clean <- agol_raw %>% 
  # Other list columns
  select(-c(extent, properties, app_categories,
            industries, languages, screenshots)) %>% 
  # Irrelevant info
  select(-c(guid, is_org_item, documentation,
            content_status, org_id,
            api_token1expiration_date, api_token2expiration_date,
            sub_info, large_thumbnail, banner, group_categories,
            listed, num_comments, num_ratings, avg_rating,
            culture, advanced_settings, proxy_filter,
            group_designations))



## 3b. List columns to keep -----

# Clean up list columns
agol_clean <- agol_clean %>% 
  # Type keywords, tags
  mutate("type_keywords" = map(type_keywords, 
                               ~str_flatten(string = .,
                                            collapse = ", ")) %>% 
           unlist(.)) %>% 
  mutate("tags" = map(tags, 
                      ~str_flatten(string = .,
                                   collapse = ", ")) %>% 
           unlist(.)) %>% 
  # Categories
  mutate("categories" = map(categories, 
                            ~str_flatten(string = .,
                                         collapse = ", ")) %>% 
           unlist(.) %>% 
           str_remove_all(string = .,
                          pattern = "/Categories/")) %>% 
  # Set blank vectors as NAs
  mutate(across(c(type_keywords, tags, categories),
                ~ifelse(. == "", NA_character_, .)))



## 3c. Dates and Size -----

# Convert date fields from UNIX milliseconds to date
# Convert size in bytes to megabytes (MB)
agol_clean <- agol_clean %>%
  mutate(across(c(created, modified, last_viewed),
                ~as.Date(as.POSIXct(. / 1000,
                                    origin = "1970-01-01")))) %>% 
  mutate("size" = size / 1e6) %>% 
  rename("size_mb" = size)



## 3d. Collection type -----

# Remove code attachment item from df (only type not reflected in DataHub)
agol_clean <- agol_clean %>% 
  filter(type != "Code Attachment")

# Set type based on front-end logic
agol_clean <- agol_clean %>% 
  mutate("type_front" = 
                     # Apps & Maps
           case_when(type %in% c("Map Service",
                                 "Web Map",
                                 "Web Mapping Application") ~
                       "apps_maps",
                     # Documents
                     type %in% c("Microsoft Excel",
                                 "Document Link",
                                 "PDF") ~
                       "documents",
                     # Data
                     type %in% c("Feature Service",
                                 "CSV Collection",
                                 "File Geodatabase") ~
                       "data",
                     # Other
                     TRUE ~
                       "other")) %>% 
  relocate(type_front, .before = type)



## 3e. Metadata Contact setup -----

# Construct API call for each item's metadata
agol_metadata_df <- agol_clean %>% 
  select(id) %>% 
  mutate("url_to_hit" = paste0("https://www.arcgis.com/sharing/rest/content/items/",
                               id, "/info/metadata/metadata.xml"))

# Write function to call metadata API for each item, return point of contact
getDatahubMetadata <- function(item_id, metadata_url){
  
  ### Conditional checks -----
  
  # Try metadata XML call, if it throws an error then return NULL object
  check_error <- tryCatch({ read_xml(metadata_url) }, error = function(e) { NULL })
  
  # If metadata XML data present, check that Point of Contact exists
  check_poc <- if(is.null(check_error)){
    "XML Error"
  } else {
    as_list(read_xml(metadata_url)) %>%
      pluck("metadata", "mdContact", "rpIndName", 1)
  }
  
  ### Conditional outputs -----
  
  # If metadata XML does not exist for an object, return ID and message
  if(is.null(check_error)){
    
    ### EXAMPLE: See conservation areas static example here:
    # read_xml("https://www.arcgis.com/sharing/rest/content/items/ffddc3674f4b4f4793a3c6091bb4fdf3/info/metadata/metadata.xml")
    
    # Output df for API hit
    output_df <- tibble(item_id) %>% 
      mutate("metadata_contact" = "Metadata XML Not Found")
    
    return(output_df)
    
  # If XML present but Point of Contact not found, then different message 
  } else if(!is.null(check_error) & is.null(check_poc)){
    
    ### EXAMPLE: See 1990 Land Use Inventory static example here:
    # read_xml("https://www.arcgis.com/sharing/rest/content/items/a721a33dc77244389820f635296ff438/info/metadata/metadata.xml") %>%
    #   pluck("metadata", "mdContact", "rpIndName", 1)
    
    ### NOTE: Look at Bikeway Inventory System (BIS) : https://www.arcgis.com/sharing/rest/content/items/4c75874452ab408092eab69ffca4948a/info/metadata/metadata.xml --> No metadata contact, but Agata listed as resource contact ----

    # Output df for API hit
    output_df <- tibble(item_id) %>% 
      mutate("metadata_contact" = "No Point of Contact")
    
    return(output_df)
    
  # If XML exists and Point of Contact present, then run code
  } else {
    
    ### EXAMPLE: See conservation areas static example here:
    # example_metadata <- as_list(read_xml("https://www.arcgis.com/sharing/rest/content/items/7c945c512bfc44188e827abbea0f4d1c/info/metadata/metadata.xml")) %>%
    #   pluck("metadata", "mdContact", "rpIndName", 1) %>%
    #   as_tibble(.) %>%
    #   rename("metadata_contact" = 1)
    
    # Hit API stored in second input/column of df
    # Then go into XML structure and pull out the metadata contact
    output_df <- as_list(read_xml(metadata_url)) %>% 
      pluck("metadata", "mdContact", "rpIndName", 1) %>% 
      as_tibble(.) %>% 
      rename("metadata_contact" = 1) %>% 
      # Create ID column to link back to AGOL data
      mutate("item_id" = item_id) %>% 
      select(item_id, metadata_contact) 
    
    return(output_df)
    
  }
}



## 3f. Metadata Contact function call -----

# Start clock
func_time_start <- Sys.time()

# Use pmap to hit API for each item, create df of outputs
# Takes about 50 seconds
agol_metadata_call <- pmap_df(.l = agol_metadata_df,
                              .f = ~getDatahubMetadata(item_id = ..1,
                                                       metadata_url = ..2)
) %>% 
  rename("id" = item_id)

# Print how long function took
func_time_taken <- Sys.time() - func_time_start
print("AGOL API Metadata function call for all objects in Open Data Hub Content Group - elapsed time:")
func_time_taken

# Join metadata contact to clean dataset
agol_clean <- agol_clean %>% 
  left_join(agol_metadata_call,
            by = "id") %>% 
  relocate(metadata_contact, .before = owner)




# 4. Organize data --------------------------------------------------------

# Organize columns for legibility
agol_clean <- agol_clean %>% 
  rename("agol_item_id" = id,
         "file_name" = name) %>% 
  # Summary text
  relocate("summary" = snippet,
           .before = description) %>% 
  # Metadata score completeness - Item information completeness score based upon item snippet, thumbnail, description, title, tags etc.
  relocate("metadata_score" = score_completeness,
           .after = metadata_contact) 



# 5. Export data ----------------------------------------------------------

# Write to Raw data folder
write_csv(agol_clean,
          file = here("02_Data", "Raw",
                      "DataHub_Base.csv"))


