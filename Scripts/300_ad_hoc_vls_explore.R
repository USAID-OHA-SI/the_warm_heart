# PROJECT: Exploring Anomaly Detection of TX_PVLS
# PURPOSE: Munge and Analysis of TX_PVLS
# AUTHOR:  Tim Esssam | SI
# REF ID:  9c0a2d41
# LICENSE: MIT
# DATE:   2024-03-13
# NOTES:   

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(glue)
    library(ggtext)
      
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Site_IM_FY22-24.*Malawi")
    
    file_path2 <- return_latest(folderpath = merdata,
                               pattern = "Site_IM_FY15-21.*Malawi")
      
    site_attrb_path <- return_latest(folderpath = file.path(glamr::si_path("path_vector")),
                                     pattern = "Malawi")
    
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "9c0a2d41"
    
  # Functions  
    calculate_percentage_change <- function(.data, id, time, value_column) {
     
       .data %>%
        arrange({{id}}, {{time}}) %>% 
        group_by({{id}}) %>%
        mutate(
          pct_change = ({{value_column}} - lag({{value_column}})) / 
            ifelse({{value_column}} == 0, NA, {{value_column}})
        )
    }
  
  read_fltr_msd <- function(file_path){
    df <- read_psd(file_path) %>% 
      filter(indicator == "TX_PVLS", 
             numeratordenom == "N",
             standardizeddisaggregate == "Total Numerator")
    
    return(df)
}  
  
  

# LOAD DATA ============================================================================  

  df_site_attrb <- read_csv(site_attrb_path)  
    
  df_site_fy24 <- read_fltr_msd(file_path)
  df_stie_fy21 <- read_fltr_msd(file_path2)
    
  df_pvls <- 
    df_msd %>% 
    group_by(indicator, sitename, orgunituid, facility, facilityuid, fiscal_year) %>% 
    summarise(across(.cols = contains("qtr"), \(x) sum(x, na.rm =T))) %>% 
    reshape_msd() %>% 
    ungroup() %>% 
    calculate_percentage_change(id = orgunituid, period, value)
    

  

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

