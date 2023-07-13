# SETUP  --------------------------------------------------------------------

# Libraries
library(gagglr)
library(tidyverse)
library(gisr)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gt)
library(gtExtras)
library(googlesheets4)
library(googledrive)

# Pull coords

pull_coords <- function(cntry, ou_uid, org_lvl, psnu_lvl, baseurl = "https://final.datim.org/"){
  
  print(paste("Running DATIM API for coordinates in", cntry,  Sys.time(),
              sep = " ... "))
  
  paste0(baseurl,
         "api/organisationUnits?filter=path:like:", ou_uid,
         "&filter=level:eq:", org_lvl, "&",
         "&fields=id,path,geometry&paging=false") %>%
    httr::GET(httr::authenticate(glamr::datim_user(),glamr::datim_pwd())) %>%
    httr::content("text") %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("organisationUnits") %>%
    tibble::as_tibble() %>%
    clean_coords(psnu_lvl)
  
} 

clean_coords <- function(df, psnu_lvl){
  
  #limit only to sites with coordinates
  # df <- dplyr::filter(df, geometry$type == "Point") 
  
  #if no sites, return null
  if(nrow(df) < 1)
    return(NULL)
  
  levels <- df$path %>%
    stringr::str_count("/") %>%
    max()
  
  #identify psnu
  df <- df %>% 
    dplyr::mutate(path = stringr::str_remove(path, "^/")) %>%
    tidyr::separate(path, paste0("orglvl_", seq(1:levels)), sep = "/", fill = "right") %>% 
    dplyr::select(orgunituid = id, geometry,
                  psnuuid = dplyr::ends_with(as.character(psnu_lvl)))
  
  #return uid + lat + long
  df <- df %>% 
    dplyr::mutate(coordinates = geometry$coordinates) %>% 
    dplyr::select(-geometry) %>% 
    tidyr::unnest_wider(coordinates, names_sep = "_") %>% 
    dplyr::rename(longitude = "coordinates_1", latitude = "coordinates_2") %>%
    dplyr::mutate_at(dplyr::vars("longitude", "latitude"), as.double)
  
  return(df)
}
# SI specific paths/functions  
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
shpdata <- file.path(glamr::si_path("path_vector")) # for shapefiles
latdata <- list.files(file.path(shpdata, "OU-sites"), pattern = "Malawi", full.names = T)
rasdat <- glamr::si_path("path_raster")

# GIS data
grabr::get_outable() %>% filter(operatingunit == "Asia Region", country == "Philippines")

cntry <- "Philippines"
spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
phl_geo <- purrr::map(list(4, 5, 8), ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                            level = .x))

names(mwi_geo) <- list("adm0", "psnu")

# Breaking change to extract_facilities required modification in workflow (issues submitted in gisr)
df_locs_mwi <- gisr::extract_locations(cntry, level = 7) %>% 
  extract_facilities()

# Site level coordinates
ctry_list <- grabr::get_outable() %>% 
  select(country, country_uid, facility_lvl, psnu_lvl) %>% 
  filter(country == "Philippines")

#pull site coordinates
df_coords <- ctry_list %>%
  pmap_dfr(~pull_coords(..1, ..2, ..3, ..4)) 


# Load Site level data
site_im_path <- return_latest(merdata, "Site_IM.*Asia")

site_df <- read_psd(site_im_path) %>% 
  filter(country == "Philippines")

site_df_tst <- site_df %>% 
  filter(indicator %in% c("HTS_TST_POS", "HTS_TST"), 
         standardizeddisaggregate == "KeyPop/Result")
