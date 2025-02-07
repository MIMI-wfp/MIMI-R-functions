################################################################################
############## FUNCTION FOR SENDING API REQUEST FOR WFP SHAPEFILES #############
################################################################################

# Author: Mo Osman
# Date created: 07-02-2025
# Last edited: 

#-------------------------------------------------------------------------------

# DESCRIPTION: 
# In this script, I will define a function that can send an API request to the 
# WFP VAM server to get shapefiles for countries at the ADM1 and ADM2 levels.

#-------------------------------------------------------------------------------

# INSTRUCTIONS:

# 1. Run this script to load the function, you can do this by running the 
# "source" function in the script you are working from, as shown below:
# source("get_shapefile.R")

# Please note that you may need to define the file path if the script is not in
# the same directory as the script you are working from.

# 2. Identify the ADM0 code for the country you require. This can be found at the 
# following link: 
# https://wfp.sharepoint.com/:x:/s/MIMIProject/EfiTVS3W_AVBrmqjqdq0cioBK5vCIvddPAt4QAPHVrXWVQ?e=bzus2d

# 3. Call the function get_shapefile(adm0_code, level), and store the returned 
# object in your environment. For example, if I want to get the ADM1 shapefile 
# for Nigeria (which has ADM0 code 182), I would call the function like this:
# nigeria_adm1 <- get_shapefile(182, "adm1")

# If you encounter any issues with the function, or have questions - please 
# contact me (Mo Osman).

#-------------------------------------------------------------------------------

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("sf", "geojsonsf", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))


#-------------------------------------------------------------------------------

# DEFINE FUNCTION:

get_shapefile <- function(adm0_code, level) {
  if (level == "adm1") {
    
    # Request geoJSON file from WFP API, and convert to a sf object:
    geojson_sf(paste0("https://api.vam.wfp.org/geodata/GetGeoAdmins?adm0=", 
                      adm0_code, "&admcode=", adm0_code))
    
  } else if (level == "adm2") {
    
    # Firstly get the ADM1 level sf object
    geo_sf <- geojson_sf(paste0("https://api.vam.wfp.org/geodata/GetGeoAdmins?adm0=", 
                                adm0_code, "&admcode=", adm0_code))
    
    #Rename columns:
    geo_sf <- geo_sf |> 
      rename(geometry_adm1 = geometry, 
             Code_adm1 = Code, 
             Name_adm1 = Name)
    
    # Create an empty data-frame to store the ADM2 geometries:
    all_adm2 <- data.frame(geometry = character(), Code = character(), 
                           Name = character(), Code_adm1 = character(), 
                           stringsAsFactors = FALSE)
    
    # Write for loop to request ADM2 geometries for each ADM1:
    for (code in geo_sf$Code_adm1) {
      req_adm2 <- geojson_sf(paste0("https://api.vam.wfp.org/geodata/GetGeoAdmins?adm0=", 
                                    adm0_code, "&admcode=", code))
      
      # Add an adm1 code column: 
      req_adm2$Code_adm1 <- code
      
      # Append to all_adm2:
      all_adm2 <- rbind(all_adm2, req_adm2)
      
      # Pause between requests to avoid overwhelming the server: 
      Sys.sleep(0.5)
    }
    # Convert all_adm2 to sf:
    all_adm2 <- st_as_sf(all_adm2)
    
    # Return object:
    all_adm2
  }
}

################################################################################
############################## END OF SCRIPT ###################################
################################################################################