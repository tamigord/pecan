---
title: "Landtrendr_AGB_prep.R"
author: "Tami Gordon"
date: "2024-04-18"
output: html_document
#' Prepare Landtrendr AGB data for the SDA workflow.
#'
#' @param outdir Where the final CSV file will be stored.
#' @param country String value to determine depth of bounding box 
#' @param start_date String value to determine start date of data sequestration  
#' @param end_date String value to determine end date of data sequestration 
#' @param site_info Bety list of file names with the h5 extension
#' @param focus_sites Bety list of file names with the zip extension within GEDI4R package 
#' @param export_csv Decide if we want to export the CSV file.
#'Suggests: rmarkdown, knitr, testthat (>= 3.0.0), GEDI4R, magrittr
#' @export
#' 
#' @examples
#' @author Tami Gordon [02/08/24]
#' @importFrom magrittr %>%
#' 
---
##Setting Parameters
AGB_indir <- "/Users/tamigordon/Desktop/rGEDI/test_data"
## Specific Sites you want to focus on 
site_info = "~/Desktop/rGEDI/data files/site_info_NEON39.Rdata"


# Define the function
Landtrendr_AGB_prep <- function(site_info, AGB_indir, start_date, end_date, outdir = NULL) {
  
  # Load site_info from the .Rdata file
  load(site_info)
  
  # Extract lon and lat from the loaded site_info
  lon <- site_info$lon
  lat <- site_info$lat
  
  # Define the function to create a bounding box
  create_bounding_box <- function(lon, lat) {
    distance_km <- 1  # Assuming a 1 km distance for simplicity
    degrees_per_km <- 1 / 111
    delta_lat <- distance_km * degrees_per_km
    delta_lon <- distance_km * degrees_per_km / cos(lat * pi / 180)  # Adjust for latitude
    
    # Calculate bounding box coordinates
    min_lon <- lon - delta_lon
    max_lon <- lon + delta_lon
    min_lat <- lat - delta_lat
    max_lat <- lat + delta_lat
    
    return(list(ul_lat = max_lat, lr_lat = min_lat, ul_lon = min_lon, lr_lon = max_lon))
  }
  
  # Create the bounding box using the original method
  bounding_box <- create_bounding_box(lon, lat)
  
  # Print the bounding box coordinates for debugging
  print("Bounding Box Coordinates:")
  print(bounding_box)
  
  # Download files within the bounding box
  file_download <- l4_download(
    ul_lat = bounding_box$ul_lat,  # Upper left latitude
    lr_lat = bounding_box$lr_lat,  # Lower right latitude
    ul_lon = bounding_box$ul_lon,  # Upper left longitude
    lr_lon = bounding_box$lr_lon,  # Lower right longitude
    outdir = outdir,  
    from = start_date,
    to = end_date,
    just_path = FALSE  # Assuming you want to download the files, set to TRUE if you only need paths
  )
  
  # Get the merged data from the downloaded files
  gediL4_path <- l4_getmulti(file_download, merge = TRUE)
  
  return(gediL4_path)
}

# Call the function with provided arguments
result <- Landtrendr_AGB_prep(site_info, AGB_indir, start_date = "2020-01-01", end_date = "2020-01-31", outdir = "~/Desktop/")

## Notes: 
## current code runs with error: Error in l4_download(ul_lat = bounding_box$ul_lat, lr_lat = bounding_box$lr_lat,  :  coordinates are not numeric
## Use of shapefile to clip - Is this still necessary? 
## Outdir is now set as a parameter inside of Landtrendr model

## Next Steps:
  ## Debugging - why am I getting the error in l4_download? (Look in GEDI4R manual for debugging instructions, meet with Dongchen)
  ## How will this code find AGB data? It is able to download the files within the bounding box, however, it may not run until the l4_download error is fixed
  ## Projection: Make sure all data is aligned throughout and why I chose the coordinate system(It was stated in the manual for the GEDI4R data that bounding boxes are 1 square km)

## Future Steps:
  ## Save file as csv with: [Format: "date", "site_id", "lat", "lon", "agb", "sd”]
      ## Use of rbind to add columns, must be assigned to a variable name
  


