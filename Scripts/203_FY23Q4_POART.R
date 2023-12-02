# PROJECT: Q4 POART Visuals for FY23Q4
# PURPOSE: Munge and Analysis of PSNU x IM MSD for Malawi
# AUTHOR:  Tim Esssam | SI
# REF ID:  0405242c
# LICENSE: MIT
# DATE:   2023-11-30
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
    site_path <- return_latest(folderpath = merdata,
      pattern = "Site_IM.*Zambia")
    
    file_path <- return_latest(folderpath = merdata,
                               pattern = "PSNU_IM.*Zambia")
    
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "0405242c"
    
  # Functions  
  # Returns TX_CURR by default with no disaggregations  
  calc_trends <- function(.data, indicator = "TX_CURR", ...){
    df <- .data %>% 
      filter(indicator == {{indicator}}, 
             standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
      group_by(fiscal_year, indicator, ...) %>% 
      summarise(across(matches("targ|qtr"), sum, na.rm = T)) %>% 
      ungroup() %>% 
      reshape_msd(direction = "quarters") %>% 
      mutate(achv = results_cumulative / targets,
             qtr_flag = ifelse(period %in% c(metadata$curr_pd), 1, 0)
      )
    return(df)
  }

# LOAD DATA ============================================================================  

  df_msd <- read_psd(file_path) 
      
  # What's causing issues in the new MSD?
  df_msd %>% names() %>% grep("targ", ., value = T)
  df_msd <- df_msd %>% select(-c(target_age_2024, target_modality_2024))  
    
  df_site <- read_psd(site_path)

# MUNGE ============================================================================
  
  # Pull TX overall
  df_tx <- calc_trends(df_msd, "TX_CURR")
  df_tx_new <- calc_trends(df_msd, "TX_NEW")    
  
  # Pull TX by Mechs
  
  
  # Pull TX by SNU1
  df_tx_snu <- calc_trends(df_msd, "TX_CURR", snu1)
  df_tx_new_snu <- calc_trends(df_msd, "TX_NEW", snu1)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

